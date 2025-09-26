{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Text.Pandoc.Command.Simple
Description : JSON-driven, simple block edits for Pandoc documents
Copyright   : (c) Your Name, 2024
License     : BSD-3-Clause
Maintainer  : you@example.com
Stability   : experimental
Portability : POSIX

= Overview

This library provides a simple, JSON-driven command protocol for making
targeted edits to Pandoc documents. It's designed to be conservative,
predictable, and easy to understand - perfect for programmatic document
manipulation where you need surgical precision.

== Key Features

* __Simple addressing__: Use either top-level indices or paths to target specific blocks
* __JSON protocol__: All operations can be serialized\/deserialized via JSON
* __Type safety__: Leverages Haskell's type system to prevent invalid operations
* __Composable__: Operations can be batched and applied sequentially
* __Error handling__: Clear error messages for invalid operations or paths

== Quick Start

>>> import Text.Pandoc.Command.Simple
>>> import Text.Pandoc.Definition
>>> let doc = Pandoc nullMeta [Para [Str "Hello"], Para [Str "World"]]
>>> let op = Replace (FocusIndex 1) (Para [Str "Universe"])
>>> applySimpleOps [op] doc
Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "Hello"],Para [Str "Universe"]])

== Addressing System

The addressing system is designed to be intuitive and follows the natural
structure of Pandoc documents:

=== Top-level addressing

Use 'FocusIndex' to target blocks at the document root:

* @FocusIndex 0@ → First block in document body
* @FocusIndex 1@ → Second block in document body
* etc.

=== Path-based addressing

Use 'FocusPath' to target blocks nested within containers:

* @FocusPath [2]@ → Third block in document body (same as @FocusIndex 2@)
* @FocusPath [1, 0]@ → First block inside the second top-level block
* @FocusPath [0, 1, 2]@ → Third block in second item of first list

==== Container-specific path rules

Different container types have specific path requirements:

[BlockQuote, Div, Figure] Single index for the block within the container:

> [containerIndex, blockIndex]

[OrderedList, BulletList] Two indices - item then block:

> [containerIndex, itemIndex, blockIndex]

[DefinitionList] Three indices - term, definition, then block:

> [containerIndex, termIndex, definitionIndex, blockIndex]

== Supported Operations

All operations work on a single 'Block' and are applied atomically:

[@Replace@] Replace a block entirely
[@InsertBefore@] Insert a new block before the target
[@InsertAfter@] Insert a new block after the target
[@Delete@] Remove the target block
[@WrapBlockQuote@] Wrap the target in a 'BlockQuote'
[@WrapDiv@] Wrap the target in a 'Div' with specified attributes
[@SetAttr@] Set attributes on blocks that support them
[@HeaderAdjust@] Modify header levels with clamping to valid range [1..6]
[@AppendInlines@] Append inline elements to 'Para' or 'Plain' blocks

== Error Handling

Operations can fail for several reasons:

* __Out of bounds__: Index exceeds container length
* __Invalid path__: Path descends too deep or uses wrong structure
* __Type mismatch__: Operation not supported for target block type
* __Empty path__: Path list is empty

All errors return descriptive 'Text' messages indicating the specific problem.

== JSON Protocol

The library provides full JSON serialization support. Operations can be
encoded as single objects or arrays:

=== Single operation

> {
>   "op": "replace",
>   "focus": {"index": 0},
>   "block": {"t": "Para", "c": [[{"t": "Str", "c": "New text"}]]}
> }

=== Multiple operations

> [
>   {"op": "delete", "focus": {"path": [1, 0]}},
>   {"op": "insert_after", "focus": {"index": 2}, "block": {...}}
> ]

== Implementation Notes

The implementation prioritizes clarity and correctness:

* Uses @pandoc-lens@ for clean lens-based document traversal
* Pattern matches explicitly on container types for clear path semantics
* Validates all indices and paths before applying operations
* Preserves document structure and metadata

For more complex document transformations, consider using Pandoc's native
AST manipulation or the full @pandoc-lens@ library directly.

== Performance Optimization Example

Here's a practical example of how to optimize operation batches:

> import Data.List (sortOn, groupBy)
> import Data.Function (on)
>
> -- | Optimize a list of operations for better performance
> optimizeOperations :: [SimpleOp] -> [SimpleOp]
> optimizeOperations ops =
>   let -- Group operations by their target location
>       grouped = groupBy (sameContainer `on` focus) $
>                 sortOn (pathDepth . focus) ops
>       -- Sort each group by index within the container
>       sorted = map (sortOn (lastIndex . focus)) grouped
>   in concat sorted
>   where
>     -- Check if two foci target the same container
>     sameContainer (FocusIndex _) (FocusIndex _) = True
>     sameContainer (FocusPath p1) (FocusPath p2) =
>       length p1 == length p2 && init p1 == init p2
>     sameContainer _ _ = False
>
>     -- Get the depth of a focus path
>     pathDepth (FocusIndex _) = 1
>     pathDepth (FocusPath p) = length p
>
>     -- Get the last index in a focus
>     lastIndex (FocusIndex i) = i
>     lastIndex (FocusPath p) = last p
>
> -- Usage:
> -- let optimized = optimizeOperations myOperations
> -- result <- applySimpleOps optimized document

This optimization can significantly improve performance for large operation
batches by minimizing redundant document traversals.
-}
module Text.Pandoc.Command.Simple
  ( -- * Commands and focusing
    Focus(..)
  , SimpleOp(..)
    -- * Apply commands
  , applySimpleOps
  , applySimpleOpsJSON
  ) where

import           Control.Applicative ((<|>))
import           Data.Bifunctor (first)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import           Control.Lens hiding ((.=), set)
import qualified Control.Lens as L
import           Text.Pandoc.Definition
import           Text.Pandoc.Lens

import           Data.Aeson ((.=), eitherDecode', FromJSON(..), ToJSON(..), Value(..), withObject, (.:), (.:?), object)
import           Data.Aeson.Types (Parser)

--------------------------------------------------------------------------------
-- Focus: where to apply an operation
--------------------------------------------------------------------------------

{-| Specifies the target location for an operation within a Pandoc document.

'Focus' provides two ways to identify a specific 'Block':

1. __Simple indexing__ with 'FocusIndex' for top-level blocks
2. __Path-based addressing__ with 'FocusPath' for nested blocks

== Examples

Target the first paragraph in a document:

> FocusIndex 0

Target the second block inside the first list item of the third top-level block:

> FocusPath [2, 0, 1]

Target a block nested three levels deep:

> FocusPath [1, 2, 0, 1]  -- container → item → definition → block

== Path Validation

Paths are validated at runtime. Invalid paths (out-of-bounds indices,
incorrect nesting structure, etc.) will cause operations to fail with
descriptive error messages.

See the module documentation for detailed path semantics for each container type.
-}
data Focus
  = FocusIndex Int      -- ^ Target a top-level block by zero-based index
  | FocusPath  [Int]    -- ^ Target a nested block using a path of indices
  deriving (Eq, Show)

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

{-| A simple block operation that can be applied to a Pandoc document.

'SimpleOp' represents the complete set of supported block-level transformations.
Each operation targets a specific block via a 'Focus' and either modifies it
in-place or affects the surrounding container structure.

== Operation Categories

=== Content Operations
These operations modify or replace block content:

* 'Replace' - Completely replace the target block
* 'Delete' - Remove the target block from its container
* 'AppendInlines' - Add inline elements to 'Para' or 'Plain' blocks

=== Insertion Operations
These operations add new blocks relative to the target:

* 'InsertBefore' - Add a new block immediately before the target
* 'InsertAfter' - Add a new block immediately after the target

=== Wrapping Operations
These operations wrap the target block in a container:

* 'WrapBlockQuote' - Wrap in a 'BlockQuote'
* 'WrapDiv' - Wrap in a 'Div' with specified attributes

=== Attribute Operations
These operations modify block attributes:

* 'SetAttr' - Set attributes on blocks that support them
* 'HeaderAdjust' - Modify header levels with automatic clamping

== Usage Examples

Replace a paragraph:

> Replace (FocusIndex 0) (Para [Str "New content"])

Insert before the second block:

> InsertBefore (FocusPath [1]) (Header 2 nullAttr [Str "Section"])

Wrap the first block in a div with CSS class:

> WrapDiv (FocusIndex 0) ("", ["highlight"], [])

Adjust header level with clamping:

> HeaderAdjust (FocusPath [2]) (Just 1) Nothing  -- Set to level 1
> HeaderAdjust (FocusPath [2]) Nothing (Just 2)  -- Increase by 2 levels

== Type Safety

Operations are type-checked at runtime. For example:

* 'SetAttr' only works on blocks that have attributes ('Header', 'CodeBlock', etc.)
* 'AppendInlines' only works on 'Para' and 'Plain' blocks
* 'HeaderAdjust' only works on 'Header' blocks

Attempting to use an operation on an incompatible block type will result in
a descriptive error message.

== JSON Serialization

All operations can be serialized to\/from JSON using their @op@ field as a discriminator:

> {"op": "replace", "focus": {"index": 0}, "block": {...}}
> {"op": "delete", "focus": {"path": [1, 2]}}
> {"op": "header_adjust", "focus": {"index": 3}, "set": 2}
-}
data SimpleOp
  = Replace       Focus Block
    -- ^ Replace the target block entirely
  | InsertBefore  Focus Block
    -- ^ Insert a new block immediately before the target
  | InsertAfter   Focus Block
    -- ^ Insert a new block immediately after the target
  | Delete        Focus
    -- ^ Remove the target block from its container
  | WrapBlockQuote Focus
    -- ^ Wrap the target block in a 'BlockQuote'
  | WrapDiv       Focus Attr
    -- ^ Wrap the target block in a 'Div' with the given attributes
  | SetAttr       Focus Attr
    -- ^ Set attributes on blocks that support them
  | HeaderAdjust  Focus (Maybe Int) (Maybe Int)
    -- ^ Adjust header level: @set@ overrides current level, @delta@ adds to it.
    --   Results are clamped to the valid range [1..6]
  | AppendInlines Focus [Inline]
    -- ^ Append inline elements to 'Para' or 'Plain' blocks
  deriving (Eq, Show)

-- JSON encoding: tagged by "op"
instance FromJSON SimpleOp where
  parseJSON = withObject "SimpleOp" $ \o -> do
    opName <- (o .: "op" :: Parser Text)
    case opName of
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

{-| Apply a sequence of operations to a Pandoc document.

Operations are applied in order, and the function uses fail-fast semantics:
if any operation fails, processing stops immediately and returns the error.

== Parameters

* @ops@ - List of operations to apply in sequence
* @doc@ - Target Pandoc document

== Return Value

* @Right doc'@ - Successfully transformed document
* @Left error@ - Descriptive error message for the first failed operation

== Examples

Single operation:
>>> let doc = Pandoc nullMeta [Para [Str "Hello"]]
>>> let op = Replace (FocusIndex 0) (Para [Str "World"])
>>> applySimpleOps [op] doc
Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "World"]])

Multiple operations:
>>> let ops = [ Delete (FocusIndex 1)
...           , InsertAfter (FocusIndex 0) (Para [Str "Inserted"])
...           ]
>>> applySimpleOps ops doc
Right (...)

Error case:
>>> applySimpleOps [Delete (FocusIndex 999)] doc
Left "Out-of-bounds delete: index 999, length 1."

== Error Handling

Common error scenarios:

* __Index out of bounds__: Targeting non-existent blocks
* __Invalid paths__: Malformed or impossible addressing
* __Type mismatches__: Using operations on incompatible block types
* __Empty containers__: Operations that would create invalid document structure

All errors include descriptive messages to help diagnose the problem.

== Performance Notes

Operations are applied sequentially with early termination on failure.
Each operation requires a complete traversal of the document structure
to locate the target block, which can be expensive for deeply nested
documents or large operation batches.

=== Optimization Strategies

For better performance with large batches:

1. __Group operations by proximity__: Operations targeting nearby blocks
   benefit from spatial locality and reduced tree traversals.

2. __Sort operations by path depth__: Apply shallow operations first,
   then deeper ones, to minimize the impact of structural changes.

3. __Batch related transformations__: Consider combining multiple simple
   operations into a single custom transformation when possible.

=== Performance Example

__Inefficient approach__ (multiple scattered operations):

> -- Bad: Each operation traverses the entire document
> let inefficientOps =
>       [ Delete (FocusPath [0, 2, 1])      -- Deep in first container
>       , Replace (FocusIndex 5) newBlock   -- Far away at top level
>       , Delete (FocusPath [0, 2, 0])      -- Back to first container
>       , InsertAfter (FocusIndex 6) block  -- Top level again
>       ]

__Optimized approach__ (grouped and sorted operations):

> -- Better: Group by location, process in logical order
> let optimizedOps =
>       [ -- First, handle all operations in container [0,2] together
>         Delete (FocusPath [0, 2, 1])
>       , Delete (FocusPath [0, 2, 0])      -- Now index 0 after previous delete
>         -- Then handle top-level operations in index order
>       , Replace (FocusIndex 5) newBlock
>       , InsertAfter (FocusIndex 6) block  -- Index may shift due to replace
>       ]

__Advanced optimization__ (single traversal with custom logic):

> -- Best: Custom function that handles multiple operations in one pass
> -- This requires implementing your own traversal logic
> processContainerAndTopLevel :: Pandoc -> Either Text Pandoc

=== Performance Characteristics

* __Time complexity__: O(n×d) where n = number of operations, d = average path depth
* __Space complexity__: O(d) for path traversal stack
* __Worst case__: Deep nesting with scattered operations across the document
* __Best case__: Shallow operations on nearby blocks
-}
applySimpleOps :: [SimpleOp] -> Pandoc -> Either Text Pandoc
applySimpleOps ops = go ops
  where
    go []     d = Right d
    go (x:xs) d = applySimpleOp x d >>= go xs

{-| Parse operations from JSON and apply them to a document.

This function provides a convenient interface for JSON-driven document
transformation. It accepts both single operations and arrays of operations.

== Supported JSON Formats

=== Single Operation
@
{
  "op": "replace",
  "focus": {"index": 0},
  "block": {"t": "Para", "c": [[{"t": "Str", "c": "Hello"}]]}
}
@

=== Multiple Operations
@
[
  {"op": "delete", "focus": {"path": [1]}},
  {"op": "insert_after", "focus": {"index": 0}, "block": {...}}
]
@

== Parameters

* @bs@ - JSON-encoded operation(s) as a lazy ByteString
* @doc@ - Target Pandoc document to transform

== Return Value

* @Right doc'@ - Successfully transformed document
* @Left error@ - Parse error or operation error message

== Examples

With valid JSON:
>>> let json = "{\"op\":\"delete\",\"focus\":{\"index\":0}}"
>>> applySimpleOpsJSON (BL.pack json) doc
Right (Pandoc (Meta {unMeta = fromList []}) [])

With invalid JSON:
>>> applySimpleOpsJSON "invalid json" doc
Left "Error in $: Failed reading..."

With valid JSON but invalid operation:
>>> let json = "{\"op\":\"delete\",\"focus\":{\"index\":999}}"
>>> applySimpleOpsJSON (BL.pack json) doc
Left "Out-of-bounds delete: index 999, length 1."

== Error Types

1. __Parse Errors__: Malformed JSON or unknown operation types
2. __Operation Errors__: Valid JSON but failed operations (same as 'applySimpleOps')

Parse errors are returned as @String@ while operation errors use @Text@.
Both provide descriptive error messages for debugging.
-}
applySimpleOpsJSON :: BL.ByteString -> Pandoc -> Either String Pandoc
applySimpleOpsJSON bs doc =
  case eitherDecode' bs :: Either String [SimpleOp] of
    Right ops -> first T.unpack (applySimpleOps ops doc)
    Left _    -> do
      singleOp <- eitherDecode' bs :: Either String SimpleOp
      first T.unpack (applySimpleOps [singleOp] doc)

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

applySimpleOp :: SimpleOp -> Pandoc -> Either Text Pandoc
applySimpleOp sop doc =
  let edit = editFor sop
      path = case getFocus sop of
               FocusIndex i -> [i]
               FocusPath  p -> p
  in overBodyAtPath path edit doc
  where
    getFocus :: SimpleOp -> Focus
    getFocus operation = case operation of
      Replace f _       -> f
      InsertBefore f _  -> f
      InsertAfter f _   -> f
      Delete f          -> f
      WrapBlockQuote f  -> f
      WrapDiv f _       -> f
      SetAttr f _       -> f
      HeaderAdjust f _ _ -> f
      AppendInlines f _ -> f

-- | Build the low-level edit function (operates on a [Block] at an index).
editFor :: SimpleOp -> ([Block] -> Int -> Either Text [Block])
editFor sop = case sop of
  Replace _ newBlk        -> \bs i -> replaceAt i newBlk bs
  InsertBefore _ newBlk   -> \bs i -> insertAt i newBlk bs
  InsertAfter _ newBlk    -> \bs i -> insertAt (i+1) newBlk bs
  Delete _                -> \bs i -> deleteAt i bs
  WrapBlockQuote _        -> \bs i -> wrapAt i (BlockQuote . (:[])) bs
  WrapDiv _ a             -> \bs i -> wrapAt i (\b -> Div a [b]) bs
  SetAttr _ a             -> \bs i -> do
                                         (prefix,b,suffix) <- splitAtIndex i bs
                                         b' <- setBlockAttr a b
                                         pure (prefix ++ b' : suffix)
  HeaderAdjust _ mSet mDelta ->
                            \bs i -> do
                                        (prefix,b,suffix) <- splitAtIndex i bs
                                        b' <- headerAdjust mSet mDelta b
                                        pure (prefix ++ b' : suffix)
  AppendInlines _ ins     -> \bs i -> do
                                         (prefix,b,suffix) <- splitAtIndex i bs
                                         b' <- appendInlines ins b
                                         pure (prefix ++ b' : suffix)

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
  (prefix, blk, suffix) <- splitAtIndex i bs
  blk' <- applyInsideBlock rest edit blk
  pure (prefix ++ [blk'] ++ suffix)

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
  OrderedList listAttrs items ->
    case idxs of
      (itemIx : blkIx : rest) -> do
        items' <- modifyNthM itemIx items $ \blks ->
                     applyInBlocks (blkIx : rest) edit blks
        pure (OrderedList listAttrs items')
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
    (prefix, _ : suffix) -> Right (prefix ++ x : suffix)
    _               -> Left (oob "replace" i (length xs))

insertAt :: Int -> a -> [a] -> Either Text [a]
insertAt i x xs
  | i < 0 || i > length xs = Left (oob "insert" i (length xs))
  | otherwise              = Right (take i xs ++ x : drop i xs)

deleteAt :: Int -> [a] -> Either Text [a]
deleteAt i xs =
  case splitAt i xs of
    (prefix, _ : suffix) -> Right (prefix ++ suffix)
    _               -> Left (oob "delete" i (length xs))

wrapAt :: Int -> (a -> a) -> [a] -> Either Text [a]
wrapAt i f xs = case splitAtIndex i xs of
  Right (prefix, a, suffix) -> Right (prefix ++ f a : suffix)
  Left e               -> Left e

splitAtIndex :: Int -> [a] -> Either Text ([a], a, [a])
splitAtIndex i xs
  | i < 0 || i >= length xs = Left (oob "index" i (length xs))
  | otherwise = case splitAt i xs of
      (prefix, y:suffix) -> Right (prefix, y, suffix)
      _             -> Left "Impossible: splitAtIndex mismatch."

modifyNthM :: Int -> [a] -> (a -> Either Text a) -> Either Text [a]
modifyNthM i xs f = do
  (prefix, a, suffix) <- splitAtIndex i xs
  a' <- f a
  pure (prefix ++ a' : suffix)

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