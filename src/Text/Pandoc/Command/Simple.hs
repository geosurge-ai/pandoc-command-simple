{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Text.Pandoc.Command.Simple
-- Description : JSON-driven, simple block edits for Pandoc documents.
--
-- This tiny library defines a compact command protocol (with Aeson
-- instances) and an interpreter that applies those commands to a Pandoc
-- document. It is intentionally conservative and easy to explain.
--
-- Addressing semantics are documented at the top of this file; they only
-- involve counting indices in obvious places (body, items, definitions).
--
-- The implementation uses `pandoc-lens` for the document body lens and
-- prisms for small, focused edits; for nested container rewrites we use
-- straightforward pattern matching to keep the path semantics explicit.
module Text.Pandoc.Command.Simple
  ( -- * Commands and focusing
    Focus(..)
  , SimpleOp(..)
    -- * Apply commands
  , applySimpleOps
  , applySimpleOpsJSON
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (when)
import           Data.Bifunctor (first)
import           Data.Foldable (toList)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics (Generic)

import           Control.Lens hiding ((.=), set)
import qualified Control.Lens as L
import           Text.Pandoc.Definition
import           Text.Pandoc.Lens

import           Data.Aeson ((.=), eitherDecode', encode, FromJSON(..), ToJSON(..), Value(..), withObject, (.:), (.:?), object)
import           Data.Aeson.Types (Parser)

--------------------------------------------------------------------------------
-- Focus: where to apply an operation
--------------------------------------------------------------------------------

-- | Focus identifies a single 'Block' within a 'Pandoc'.
-- Either by top-level index, or by a path that descends into containers.
data Focus
  = FocusIndex Int      -- ^ top-level index into the document body
  | FocusPath  [Int]    -- ^ path per the rules in the module header
  deriving (Eq, Show, Generic)

-- JSON: { "index": N }  or  { "path": [i, j, ...] }
instance FromJSON Focus where
  parseJSON = withObject "Focus" $ \o ->
    (FocusIndex <$> o .: "index") <|> (FocusPath <$> o .: "path")

instance ToJSON Focus where
  toJSON (FocusIndex i) = object ["index" .= i]
  toJSON (FocusPath p)  = object ["path"  .= p]

--------------------------------------------------------------------------------
-- Commands (operations)
--------------------------------------------------------------------------------

-- | The small set of simple block operations we support.
data SimpleOp
  = Replace       { focus :: Focus, block   :: Block }
  | InsertBefore  { focus :: Focus, block   :: Block }
  | InsertAfter   { focus :: Focus, block   :: Block }
  | Delete        { focus :: Focus }
  | WrapBlockQuote{ focus :: Focus }
  | WrapDiv       { focus :: Focus, attr    :: Attr }
  | SetAttr       { focus :: Focus, attr    :: Attr }
  | HeaderAdjust  { focus :: Focus, set     :: Maybe Int, delta :: Maybe Int }
  | AppendInlines { focus :: Focus, inlines :: [Inline] }
  deriving (Eq, Show, Generic)

-- JSON encoding: tagged by "op"
instance FromJSON SimpleOp where
  parseJSON = withObject "SimpleOp" $ \o -> do
    op <- (o .: "op" :: Parser Text)
    case op of
      "replace"         -> Replace       <$> o .: "focus" <*> o .: "block"
      "insert_before"   -> InsertBefore  <$> o .: "focus" <*> o .: "block"
      "insert_after"    -> InsertAfter   <$> o .: "focus" <*> o .: "block"
      "delete"          -> Delete        <$> o .: "focus"
      "wrap_blockquote" -> WrapBlockQuote<$> o .: "focus"
      "wrap_div"        -> WrapDiv       <$> o .: "focus" <*> o .: "attr"
      "set_attr"        -> SetAttr       <$> o .: "focus" <*> o .: "attr"
      "header_adjust"   -> HeaderAdjust  <$> o .: "focus" <*> o .:? "set" <*> o .:? "delta"
      "append_inlines"  -> AppendInlines <$> o .: "focus" <*> o .: "inlines"
      other             -> fail $ "Unknown op: " <> T.unpack other

instance ToJSON SimpleOp where
  toJSON = \case
    Replace f b        -> object ["op" .= String "replace",         "focus" .= f, "block"   .= b]
    InsertBefore f b   -> object ["op" .= String "insert_before",   "focus" .= f, "block"   .= b]
    InsertAfter f b    -> object ["op" .= String "insert_after",    "focus" .= f, "block"   .= b]
    Delete f           -> object ["op" .= String "delete",          "focus" .= f]
    WrapBlockQuote f   -> object ["op" .= String "wrap_blockquote", "focus" .= f]
    WrapDiv f a        -> object ["op" .= String "wrap_div",        "focus" .= f, "attr"    .= a]
    SetAttr f a        -> object ["op" .= String "set_attr",        "focus" .= f, "attr"    .= a]
    HeaderAdjust f s d -> object ["op" .= String "header_adjust",   "focus" .= f, "set"     .= s, "delta" .= d]
    AppendInlines f is -> object ["op" .= String "append_inlines",  "focus" .= f, "inlines" .= is]

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Apply a batch of simple operations in order.
--
-- If any operation fails, subsequent ones are not applied.
applySimpleOps :: [SimpleOp] -> Pandoc -> Either Text Pandoc
applySimpleOps ops = go ops
  where
    go []     d = Right d
    go (x:xs) d = applySimpleOp x d >>= go xs

-- | Parse one or many commands from JSON and apply them.
-- The JSON can be either a single object or an array of objects.
applySimpleOpsJSON :: BL.ByteString -> Pandoc -> Either String Pandoc
applySimpleOpsJSON bs doc =
  case eitherDecode' bs :: Either String [SimpleOp] of
    Right ops -> first T.unpack (applySimpleOps ops doc)
    Left _    -> do
      op <- eitherDecode' bs :: Either String SimpleOp
      first T.unpack (applySimpleOps [op] doc)

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

applySimpleOp :: SimpleOp -> Pandoc -> Either Text Pandoc
applySimpleOp sop doc =
  let edit = editFor sop
      path = case focus sop of
               FocusIndex i -> [i]
               FocusPath  p -> p
  in overBodyAtPath path edit doc

-- | Build the low-level edit function (operates on a [Block] at an index).
editFor :: SimpleOp -> ([Block] -> Int -> Either Text [Block])
editFor = \case
  Replace _ newBlk        -> \bs i -> replaceAt i newBlk bs
  InsertBefore _ newBlk   -> \bs i -> insertAt i newBlk bs
  InsertAfter _ newBlk    -> \bs i -> insertAt (i+1) newBlk bs
  Delete _                -> \bs i -> deleteAt i bs
  WrapBlockQuote _        -> \bs i -> wrapAt i (BlockQuote . (:[])) bs
  WrapDiv _ a             -> \bs i -> wrapAt i (\b -> Div a [b]) bs
  SetAttr _ a             -> \bs i -> do
                                         (pre,b,post) <- splitAtIndex i bs
                                         b' <- setBlockAttr a b
                                         pure (pre ++ b' : post)
  HeaderAdjust _ mSet mDelta ->
                            \bs i -> do
                                        (pre,b,post) <- splitAtIndex i bs
                                        b' <- headerAdjust mSet mDelta b
                                        pure (pre ++ b' : post)
  AppendInlines _ ins     -> \bs i -> do
                                         (pre,b,post) <- splitAtIndex i bs
                                         b' <- appendInlines ins b
                                         pure (pre ++ b' : post)

--------------------------------------------------------------------------------
-- Editing primitives over the Pandoc body
--------------------------------------------------------------------------------

-- | Apply an \"edit at index\" under a path relative to the Pandoc body.
overBodyAtPath
  :: [Int]
  -> ([Block] -> Int -> Either Text [Block])
  -> Pandoc
  -> Either Text Pandoc
overBodyAtPath [] _ _ = Left "Empty focus path: need at least one index."
overBodyAtPath p edit d =
  do bs' <- applyInBlocks p edit (d ^. body)
     pure (L.set body bs' d)

-- Descend inside nested containers and apply the edit.
applyInBlocks
  :: [Int]
  -> ([Block] -> Int -> Either Text [Block])
  -> [Block]
  -> Either Text [Block]
applyInBlocks [] _ _ = Left "Internal error: empty path at container."
applyInBlocks [i] edit bs = edit bs i
applyInBlocks (i:rest) edit bs = do
  (pre, blk, post) <- splitAtIndex i bs
  blk' <- applyInsideBlock rest edit blk
  pure (pre ++ [blk'] ++ post)

-- Recurse according to the container shape.
applyInsideBlock
  :: [Int]
  -> ([Block] -> Int -> Either Text [Block])
  -> Block
  -> Either Text Block
applyInsideBlock idxs edit = \case
  BlockQuote blks ->
    BlockQuote <$> applyInBlocks idxs edit blks
  Div a blks ->
    Div a <$> applyInBlocks idxs edit blks
  Figure a cap blks ->
    Figure a cap <$> applyInBlocks idxs edit blks
  OrderedList attrs items ->
    case idxs of
      (itemIx : blkIx : rest) -> do
        items' <- modifyNthM itemIx items $ \blks ->
                     applyInBlocks (blkIx : rest) edit blks
        pure (OrderedList attrs items')
      _ -> Left "Path into OrderedList must be [itemIndex, blockIndex, ...]."
  BulletList items ->
    case idxs of
      (itemIx : blkIx : rest) -> do
        items' <- modifyNthM itemIx items $ \blks ->
                     applyInBlocks (blkIx : rest) edit blks
        pure (BulletList items')
      _ -> Left "Path into BulletList must be [itemIndex, blockIndex, ...]."
  DefinitionList defs ->
    case idxs of
      (termIx : defIx : blkIx : rest) -> do
        defs' <- modifyNthM termIx defs $ \(term, defLists) -> do
          defLists' <- modifyNthM defIx defLists $ \blks ->
                          applyInBlocks (blkIx : rest) edit blks
          pure (term, defLists')
        pure (DefinitionList defs')
      _ -> Left "Path into DefinitionList must be [termIndex, definitionIndex, blockIndex, ...]."
  _ ->
    Left "Path goes too deep: this block has no block-children."

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

replaceAt :: Int -> a -> [a] -> Either Text [a]
replaceAt i x xs =
  case splitAt i xs of
    (pre, _ : post) -> Right (pre ++ x : post)
    _               -> Left (oob "replace" i (length xs))

insertAt :: Int -> a -> [a] -> Either Text [a]
insertAt i x xs
  | i < 0 || i > length xs = Left (oob "insert" i (length xs))
  | otherwise              = Right (take i xs ++ x : drop i xs)

deleteAt :: Int -> [a] -> Either Text [a]
deleteAt i xs =
  case splitAt i xs of
    (pre, _ : post) -> Right (pre ++ post)
    _               -> Left (oob "delete" i (length xs))

wrapAt :: Int -> (a -> a) -> [a] -> Either Text [a]
wrapAt i f xs = case splitAtIndex i xs of
  Right (pre, a, post) -> Right (pre ++ f a : post)
  Left e               -> Left e

splitAtIndex :: Int -> [a] -> Either Text ([a], a, [a])
splitAtIndex i xs
  | i < 0 || i >= length xs = Left (oob "index" i (length xs))
  | otherwise = case splitAt i xs of
      (pre, y:post) -> Right (pre, y, post)
      _             -> Left "Impossible: splitAtIndex mismatch."

modifyNthM :: Int -> [a] -> (a -> Either Text a) -> Either Text [a]
modifyNthM i xs f = do
  (pre, a, post) <- splitAtIndex i xs
  a' <- f a
  pure (pre ++ a' : post)

oob :: Text -> Int -> Int -> Text
oob what i len =
  T.pack $ mconcat
    [ "Out-of-bounds ", T.unpack what, ": index ", show i
    , ", length ", show len, "."
    ]

--------------------------------------------------------------------------------
-- Block-level transforms
--------------------------------------------------------------------------------

setBlockAttr :: Attr -> Block -> Either Text Block
setBlockAttr a = \case
  Header lvl _ xs      -> Right (Header lvl a xs)
  CodeBlock _ txt      -> Right (CodeBlock a txt)
  Div _ xs             -> Right (Div a xs)
  Figure _ cap xs      -> Right (Figure a cap xs)
  Table _ cap cs th tb tf -> Right (Table a cap cs th tb tf)
  b                    -> Left ("set_attr: block has no attributes: " <> T.pack (show b))

headerAdjust :: Maybe Int -> Maybe Int -> Block -> Either Text Block
headerAdjust mSet mDelta = \case
  Header lvl attr xs ->
    let target = clamp 1 6 $ case mSet of
                  Just s -> s
                  Nothing -> lvl + fromMaybe 0 mDelta
    in Right (Header target attr xs)
  b -> Left ("header_adjust: not a Header: " <> T.pack (show b))

appendInlines :: [Inline] -> Block -> Either Text Block
appendInlines ins b
  -- Use prisms from pandoc-lens when applicable.
  | Just xs <- b ^? _Para  = Right (L.set _Para  (xs ++ ins) b)
  | Just xs <- b ^? _Plain = Right (L.set _Plain (xs ++ ins) b)
  | otherwise              = Left "append_inlines: expected Para or Plain."

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)