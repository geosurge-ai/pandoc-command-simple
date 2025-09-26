{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

-- Property-based tests for Text.Pandoc.Command.Simple
--
-- Run with:  cabal test  OR  stack test
-- (Ensure deps: tasty, tasty-quickcheck, tasty-hunit,
--  QuickCheck, aeson, text, bytestring, pandoc-types, pandoc-lens, lens)

import           Prelude
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Data.Aeson
--

import           Text.Pandoc.Definition
import           Text.Pandoc.Command.Simple

--------------------------------------------------------------------------------
-- Generators (Arbitrary instances)
--------------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink t  = T.pack <$> shrink (T.unpack t)

instance Arbitrary Format where
  arbitrary = do
    s <- arbitrary :: Gen Text
    pure (Format s)

instance Arbitrary QuoteType where
  arbitrary = elements [SingleQuote, DoubleQuote]

instance Arbitrary MathType where
  arbitrary = elements [DisplayMath, InlineMath]

instance Arbitrary ListNumberStyle where
  arbitrary = elements [DefaultStyle, Example, Decimal, LowerRoman, UpperRoman, LowerAlpha, UpperAlpha]

instance Arbitrary ListNumberDelim where
  arbitrary = elements [DefaultDelim, Period, OneParen, TwoParens]

instance Arbitrary Alignment where
  arbitrary = elements [AlignLeft, AlignRight, AlignCenter, AlignDefault]

instance Arbitrary ColWidth where
  arbitrary = oneof [ColWidth <$> choose (0,100), pure ColWidthDefault]

instance Arbitrary RowHeadColumns where
  arbitrary = RowHeadColumns <$> choose (0,3)

genAttr :: Gen Attr
genAttr = do
  i  <- arbitrary
  cs <- listOf (elements ["a","b","c","x","y","z","note","callout"])
  kv <- listOf ( (,) <$> elements ["data-k","role","title","name"] <*> arbitrary )
  pure (i, cs, kv)

-- Inlines (keep them small; depth parameter drives size)
genInline :: Int -> Gen Inline
genInline d = frequency $
  [ (6, Str <$> arbitrary)
  , (1, pure Space)
  , (1, pure LineBreak)
  , (1, Code <$> genAttr <*> arbitrary)
  , (1, Math <$> arbitrary <*> arbitrary)
  , (1, RawInline <$> arbitrary <*> arbitrary)
  ] ++ if d <= 0 then []
      else
        [ (2, Emph <$> genInlines (d-1))
        , (2, Strong <$> genInlines (d-1))
        , (1, Quoted <$> arbitrary <*> genInlines (d-1))
        , (1, Superscript <$> genInlines (d-1))
        , (1, Subscript <$> genInlines (d-1))
        , (1, SmallCaps <$> genInlines (d-1))
        , (1, Span <$> genAttr <*> genInlines (d-1))
        ]

genInlines :: Int -> Gen [Inline]
genInlines d = resize (max 0 d + 2) (listOf (genInline d))

-- Blocks (cover all constructors; containers shrink size)
genBlock :: Int -> Gen Block
genBlock d = frequency $
  [ (4, Plain <$> genInlines (d-1))
  , (5, Para  <$> genInlines (d-1))
  , (1, LineBlock <$> resize 2 (listOf (resize 3 (listOf (genInline (d-1))))))
  , (2, CodeBlock <$> genAttr <*> arbitrary)
  , (1, RawBlock <$> arbitrary <*> arbitrary)
  , (1, Header <$> choose (1,6) <*> genAttr <*> genInlines (d-1))
  , (1, pure HorizontalRule)
  , (1, genTable d)
  ] ++ if d <= 0 then []
       else
        [ (2, BlockQuote <$> genBlocks (d-1))
        , (2, Div <$> genAttr <*> genBlocks (d-1))
        , (2, Figure <$> genAttr <*> genCaption (d-1) <*> genBlocks (d-1))
        , (2, OrderedList <$> genListAttrs <*> resize 3 (listOf1 (genBlocks (d-1))))  -- non-empty items; each non-empty
        , (2, BulletList  <$> resize 3 (listOf1 (genBlocks (d-1))))                    -- non-empty items; each non-empty
        , (2, DefinitionList <$> resize 2 (listOf1 genDefItem))                        -- non-empty terms; each has non-empty defs of non-empty blocks
        ]

genBlocks :: Int -> Gen [Block]
genBlocks d = resize 3 (listOf1 (genBlock (d-1)))

genListAttrs :: Gen ListAttributes
genListAttrs = (,,) <$> choose (1,5) <*> arbitrary <*> arbitrary

genCaption :: Int -> Gen Caption
genCaption d = Caption <$> frequency [(1, pure Nothing),(2, Just <$> resize 2 (listOf (genInline (d-1))))] <*> genBlocks (d-1)

genRow :: Int -> Gen Row
genRow d = Row <$> genAttr <*> resize 2 (listOf (genCell d))

genCell :: Int -> Gen Cell
genCell d = Cell <$> genAttr <*> arbitrary <*> (RowSpan <$> choose (1,2)) <*> (ColSpan <$> choose (1,2)) <*> genBlocks (d-1)

genTable :: Int -> Gen Block
genTable d = do
  a  <- genAttr
  cap <- genCaption (d-1)
  cols <- resize 3 (listOf ((,) <$> arbitrary <*> arbitrary))
  headR <- resize 2 (listOf (genRow (d-1)))
  let th = TableHead a headR
  bodies <- resize 2 (listOf (TableBody a <$> arbitrary <*> resize 2 (listOf (genRow (d-1))) <*> resize 2 (listOf (genRow (d-1)))))
  tf <- TableFoot a <$> resize 2 (listOf (genRow (d-1)))
  pure (Table a cap cols th bodies tf)

genDefItem :: Gen ([Inline], [[Block]])
genDefItem = do
  term <- resize 2 (listOf (Str <$> elements ["x","y","z","t","n"]))
  defs <- resize 2 $ listOf1 $ resize 3 $ listOf1 (genBlock 1)
  pure (term, defs)

instance Arbitrary Block where
  arbitrary = sized (\n -> genBlock (max 1 (n `div` 3)))

genPandoc :: Gen Pandoc
genPandoc = do
  bs <- sized (\n -> resize (max 1 (n `div` 2)) (listOf1 (genBlock (max 1 (n `div` 3)))))
  pure (Pandoc nullMeta bs)

--------------------------------------------------------------------------------
-- Paths (valid and invalid) according to our module semantics
--------------------------------------------------------------------------------

-- Locate the container [Block] list and the target index within it
-- following the same rules that applyInsideBlock uses.
locateContainerAndIndex :: Pandoc -> [Int] -> Either Text ([Block], Int)
locateContainerAndIndex (Pandoc _ bs0) = go bs0
  where
    ixAt xs i = if i < 0 || i >= length xs then Nothing else Just (xs !! i)
    go _ [] = Left "Empty path"
    go bs [i] = Right (bs, i)
    go bs (i:rest) =
      case ixAt bs i of
        Nothing -> Left "Index OOB at some level"
        Just blk -> case blk of
          BlockQuote blks -> go blks rest
          Div _ blks      -> go blks rest
          Figure _ _ blks -> go blks rest
          OrderedList _ items ->
            case rest of
              (itemIx:rest2) -> case ixAt items itemIx of
                                  Nothing    -> Left "Item index OOB"
                                  Just blks  -> go blks rest2
          BulletList items ->
            case rest of
              (itemIx:rest2) -> case ixAt items itemIx of
                                  Nothing    -> Left "Item index OOB"
                                  Just blks  -> go blks rest2
          DefinitionList defs ->
            case rest of
              (termIx:defIx:rest2) ->
                case ixAt defs termIx of
                  Nothing -> Left "termIx OOB"
                  Just (_term, defLists) ->
                    case ixAt defLists defIx of
                      Nothing    -> Left "defIx OOB"
                      Just blks  -> go blks rest2
              _ -> Left "Path shape invalid for DefinitionList"
          _ -> Left "Path goes too deep"

-- Generate a valid path into a given document (always ends at a block index).
genValidPath :: Pandoc -> Gen [Int]
genValidPath (Pandoc _ bs0) = do
  i0 <- choose (0, length bs0 - 1)
  go (bs0 !! i0) [i0] 3  -- at most 3 descents
  where
    go :: Block -> [Int] -> Int -> Gen [Int]
    go _ acc 0 = pure acc
    go blk acc depth = do
      let stop = pure acc
      let descendBlockQuote blks = do
            if null blks then stop else do
              j <- choose (0, length blks - 1)
              go (blks !! j) (acc ++ [j]) (depth - 1)
      let descendDiv = descendBlockQuote
      let descendFigure = descendBlockQuote
      let descendOL items =
            if null items then stop else do
              itemIx <- choose (0, length items - 1)
              let blks = items !! itemIx
              if null blks then stop
              else do j <- choose (0, length blks - 1)
                      go (blks !! j) (acc ++ [itemIx, j]) (depth - 1)
      let descendDL defs =
            if null defs then stop else do
              termIx <- choose (0, length defs - 1)
              let (_term, defLists) = defs !! termIx
              if null defLists then stop
              else do defIx <- choose (0, length defLists - 1)
                      let blks = defLists !! defIx
                      if null blks then stop
                      else do j <- choose (0, length blks - 1)
                              go (blks !! j) (acc ++ [termIx, defIx, j]) (depth - 1)
      frequency $
        (4, stop) : case blk of
          BlockQuote blks   -> [(3, descendBlockQuote blks)]
          Div _ blks        -> [(3, descendDiv blks)]
          Figure _ _ blks   -> [(2, descendFigure blks)]
          OrderedList _ its -> [(3, descendOL its)]
          BulletList its    -> [(3, descendOL its)]
          DefinitionList ds -> [(3, descendDL ds)]
          _                 -> []

-- Generate an invalid path by perturbing a valid one
genInvalidPath :: Pandoc -> Gen [Int]
genInvalidPath doc = do
  valid <- genValidPath doc
  oneof [ makeTooDeep valid
        , makeOOB valid
        ]
  where
    makeTooDeep p = pure (p ++ [9999])  -- too deep at the end
    makeOOB p = case locateContainerAndIndex doc p of
      Left _ -> pure (p ++ [9999])    -- fallback
      Right (bs, _i) -> do
        let bad = length bs + 5
        pure (init p ++ [bad])        -- out of bounds at the last hop

-- A simple fresh block and inline for edits
freshBlock :: Gen Block
freshBlock = pure (Para [Str "XXX"])

freshInlines :: Gen [Inline]
freshInlines = pure [Space, Str "APPENDED"]

freshAttr :: Gen Attr
freshAttr = pure ("id-x", ["class-x"], [("data-k","v")])

--------------------------------------------------------------------------------
-- Helpers to inspect containers for properties
--------------------------------------------------------------------------------

containerLengthAt :: Pandoc -> [Int] -> Maybe Int
containerLengthAt doc path = either (const Nothing) (Just . length . fst) (locateContainerAndIndex doc path)

readAt :: Pandoc -> [Int] -> Maybe Block
readAt doc path = do
  (bs, i) <- either (const Nothing) Just (locateContainerAndIndex doc path)
  if i < 0 || i >= length bs then Nothing else Just (bs !! i)

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_replace_preserves_length :: Property
prop_replace_preserves_length =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshBlock $ \newBlk ->
    let beforeLen = containerLengthAt doc path
        res = applySimpleOps [Replace (FocusPath path) newBlk] doc
    in case res of
         Left _ -> property False
         Right doc' ->
           counterexample ("path=" ++ show path) $
           containerLengthAt doc' path === beforeLen

prop_replace_sets_block :: Property
prop_replace_sets_block =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshBlock $ \newBlk ->
    case applySimpleOps [Replace (FocusPath path) newBlk] doc of
      Left _ -> property False
      Right doc' -> readAt doc' path === Just newBlk

prop_insert_before_increases_length_and_shifts :: Property
prop_insert_before_increases_length_and_shifts =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshBlock $ \newBlk ->
    let beforeLen = fromMaybe 0 (containerLengthAt doc path)
        orig      = readAt doc path
        res       = applySimpleOps [InsertBefore (FocusPath path) newBlk] doc
    in case (orig, res) of
         (Just ob, Right doc') ->
           let afterLen = containerLengthAt doc' path
               next     = readAt doc' (init path ++ [last path + 1])
               atI      = readAt doc' path
           in conjoin
              [ counterexample "length" $ afterLen === Just (beforeLen + 1)
              , counterexample "inserted-at-i" $ atI === Just newBlk
              , counterexample "shifted" $ next === Just ob
              ]
         _ -> property False

prop_insert_after_increases_length :: Property
prop_insert_after_increases_length =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshBlock $ \newBlk ->
    let beforeLen = fromMaybe 0 (containerLengthAt doc path)
        orig      = readAt doc path
        res       = applySimpleOps [InsertAfter (FocusPath path) newBlk] doc
    in case (orig, res) of
         (Just ob, Right doc') ->
           conjoin
            [ containerLengthAt doc' path === Just (beforeLen + 1)
            , readAt doc' (init path ++ [last path + 1]) === Just newBlk
            , readAt doc' path === Just ob
            ]
         _ -> property False

prop_delete_decreases_length :: Property
prop_delete_decreases_length =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
    let beforeLen = fromMaybe 0 (containerLengthAt doc path)
        res       = applySimpleOps [Delete (FocusPath path)] doc
    in case res of
         Right doc' -> containerLengthAt doc' path === Just (max 0 (beforeLen - 1))
         _          -> property False

prop_wrap_blockquote_replaces_with_singleton :: Property
prop_wrap_blockquote_replaces_with_singleton =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
    case readAt doc path of
      Nothing -> property False
      Just original ->
        case applySimpleOps [WrapBlockQuote (FocusPath path)] doc of
          Left _ -> property False
          Right doc' ->
            case readAt doc' path of
              Just (BlockQuote [b]) -> b === original
              _ -> property False

prop_wrap_div_replaces_with_singleton :: Property
prop_wrap_div_replaces_with_singleton =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshAttr $ \a ->
    case readAt doc path of
      Nothing -> property False
      Just original ->
        case applySimpleOps [WrapDiv (FocusPath path) a] doc of
          Left _ -> property False
          Right doc' ->
            case readAt doc' path of
              Just (Div a' [b]) -> conjoin [a' === a, property (b == original)]
              _ -> property False

-- SetAttr succeeds only for attribute-bearing blocks
prop_set_attr_success_or_failure :: Property
prop_set_attr_success_or_failure =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshAttr $ \a ->
    case readAt doc path of
      Nothing -> property False
      Just blk ->
        let res = applySimpleOps [SetAttr (FocusPath path) a] doc
        in case blk of
             Header{} -> isAttrSet path a res
             CodeBlock{} -> isAttrSet path a res
             Div{} -> isAttrSet path a res
             Figure{} -> isAttrSet path a res
             Table{} -> isAttrSet path a res
             _ -> case res of
                    Left _ -> property True
                    Right _ -> counterexample "SetAttr should fail for this block" False
  where
    isAttrSet :: [Int] -> Attr -> Either Text Pandoc -> Property
    isAttrSet p a (Right doc') =
      case readAt doc' p of
        Just (Header n a' xs) -> conjoin [a' === a, property (n >= 1 && n <= 6), property (not (null xs) || null xs)]
        Just (CodeBlock a' _) -> a' === a
        Just (Div a' _)       -> a' === a
        Just (Figure a' _ _)  -> a' === a
        Just (Table a' _ _ _ _ _) -> a' === a
        _ -> counterexample "Attr not set" False
    isAttrSet _ _ (Left _) = counterexample "SetAttr failed unexpectedly" False

-- HeaderAdjust succeeds only on headers, clamps to [1..6], obeys set/delta
prop_header_adjust :: Property
prop_header_adjust =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll (oneof [Left <$> choose (1,6), Right <$> choose (-3,3)]) $ \choice ->
    case readAt doc path of
      Just (Header lvl _a _xs) ->
        let (mSet, mDelta) = case choice of
                               Left newL  -> (Just newL, Nothing)
                               Right d    -> (Nothing, Just d)
            res = applySimpleOps [HeaderAdjust (FocusPath path) mSet mDelta] doc
        in case res of
             Left _ -> property False
             Right doc' ->
               case readAt doc' path of
                 Just (Header lvl' _ _) ->
                   conjoin [ property (lvl' >= 1 && lvl' <= 6)
                           , case mSet of
                               Just s -> lvl' === s
                               Nothing -> lvl' === max 1 (min 6 (lvl + fromMaybe 0 mDelta))
                           ]
                 _ -> property False
      _ -> case applySimpleOps [HeaderAdjust (FocusPath path) (Just 3) Nothing] doc of
             Left _ -> property True
             Right _ -> counterexample "HeaderAdjust should fail on non-headers" False

prop_append_inlines :: Property
prop_append_inlines =
  forAll genPandoc $ \doc ->
  forAll (genValidPath doc) $ \path ->
  forAll freshInlines $ \ins ->
    case readAt doc path of
      Just (Para xs) ->
        case applySimpleOps [AppendInlines (FocusPath path) ins] doc of
          Right doc' -> readAt doc' path === Just (Para (xs ++ ins))
          _          -> property False
      Just (Plain xs) ->
        case applySimpleOps [AppendInlines (FocusPath path) ins] doc of
          Right doc' -> readAt doc' path === Just (Plain (xs ++ ins))
          _          -> property False
      _ ->
        case applySimpleOps [AppendInlines (FocusPath path) ins] doc of
          Left _ -> property True
          Right _ -> counterexample "AppendInlines should fail on non Para/Plain" False

prop_invalid_paths_fail :: Property
prop_invalid_paths_fail =
  forAll genPandoc $ \doc ->
  forAll (genInvalidPath doc) $ \badPath ->
  forAll freshBlock $ \b ->
    conjoin
      [ counterexample "replace should fail" $ case applySimpleOps [Replace (FocusPath badPath) b] doc of
          Left _ -> property True
          Right _ -> property False
      , counterexample "insert_before should fail" $ case applySimpleOps [InsertBefore (FocusPath badPath) b] doc of
          Left _ -> property True
          Right _ -> property False
      , counterexample "insert_after should fail" $ case applySimpleOps [InsertAfter (FocusPath badPath) b] doc of
          Left _ -> property True
          Right _ -> property False
      , counterexample "delete should fail" $ case applySimpleOps [Delete (FocusPath badPath)] doc of
          Left _ -> property True
          Right _ -> property False
      ]

prop_json_roundtrip_ops :: Property
prop_json_roundtrip_ops =
  forAll genPandoc $ \_doc ->
  forAll (listOf1 (oneof [ Replace (FocusPath [0]) <$> freshBlock
                         , InsertAfter (FocusPath [0]) <$> freshBlock
                         , pure (WrapBlockQuote (FocusPath [0]))
                         ])) $ \ops ->
    let encoded = encode ops
        decoded = eitherDecode encoded :: Either String [SimpleOp]
    in case decoded of
         Left _ -> property False
         Right ops' -> ops' === ops

prop_apply_ops_fold_equivalence :: Property
prop_apply_ops_fold_equivalence =
  forAll genPandoc $ \doc ->
  forAll (sized (\n -> resize (max 1 (n `div` 2)) (listOf1 (genSomeOp doc)))) $ \ops ->
    let lhs = applySimpleOps ops doc
        rhs = foldl step (Right doc) ops
          where
            step (Left e) _  = Left e
            step (Right d) o = applySimpleOps [o] d
    in lhs === rhs

genSomeOp :: Pandoc -> Gen SimpleOp
genSomeOp doc = do
  valid <- genValidPath doc
  oneof
    [ Replace (FocusPath valid) <$> freshBlock
    , InsertBefore (FocusPath valid) <$> freshBlock
    , InsertAfter (FocusPath valid) <$> freshBlock
    , pure (Delete (FocusPath valid))
    , pure (WrapBlockQuote (FocusPath valid))
    , WrapDiv (FocusPath valid) <$> freshAttr
    , SetAttr (FocusPath valid) <$> freshAttr
    , HeaderAdjust (FocusPath valid) <$> frequency [(2, Just <$> choose (1,6)), (1, pure Nothing)] <*> frequency [(2, Just <$> choose (-2,2)), (1, pure Nothing)]
    , AppendInlines (FocusPath valid) <$> freshInlines
    ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $
  localOption (QuickCheckMaxSize 60) $  -- keep sizes moderate for speed/oom safety  -- keep sizes moderate for speed/oom safety
  testGroup "Text.Pandoc.Command.Simple (property tests)"
  [ QC.testProperty "Replace preserves container length"               prop_replace_preserves_length
  , QC.testProperty "Replace sets the block at focus"                  prop_replace_sets_block
  , QC.testProperty "InsertBefore increases length and shifts"         prop_insert_before_increases_length_and_shifts
  , QC.testProperty "InsertAfter increases length and keeps original"  prop_insert_after_increases_length
  , QC.testProperty "Delete decreases length by 1"                     prop_delete_decreases_length
  , QC.testProperty "WrapBlockQuote wraps with singleton child"        prop_wrap_blockquote_replaces_with_singleton
  , QC.testProperty "WrapDiv wraps with singleton child and attr"      prop_wrap_div_replaces_with_singleton
  , QC.testProperty "SetAttr succeeds/fails by block kind"             prop_set_attr_success_or_failure
  , QC.testProperty "HeaderAdjust clamps and sets/deltas correctly"    prop_header_adjust
  , QC.testProperty "AppendInlines works only on Para/Plain"           prop_append_inlines
  , QC.testProperty "Invalid paths always fail (core ops)"             prop_invalid_paths_fail
  , QC.testProperty "JSON [SimpleOp] roundtrip"                        prop_json_roundtrip_ops
  , QC.testProperty "applySimpleOps equals sequential application"     prop_apply_ops_fold_equivalence
  ]