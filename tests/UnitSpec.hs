{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- Unit tests for each operation in Text.Pandoc.Command.Simple.
-- These are intended to be readable, regression-focused examples.

import           Data.Text (Text)
-- removed unused imports

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Text.Pandoc.Definition
import           Text.Pandoc.Command.Simple
import           Text.Pandoc.Lens (body)
import           Control.Lens
import           Data.Aeson (encode)

p :: Text -> Block
p s = Para [Str s]

plain :: Text -> Block
plain s = Plain [Str s]

hdr :: Int -> Text -> Block
hdr n s = Header n nullAttr [Str s]

code :: Text -> Block
code s = CodeBlock nullAttr s

blist :: Block
blist = BulletList
  [ [ p "li1-1", p "li1-2" ]
  , [ p "li2-1" ]
  ]

-- olist helper was unused; remove to satisfy -Wunused-top-binds

-- dlist helper was unused; remove to satisfy -Wunused-top-binds

fig :: Block
fig = Figure ("fig-1", ["fclass"], []) (Caption Nothing []) [ p "fig-body" ]

dv :: Block
dv = Div ("dv-1", ["callout"], []) [ p "in-div" ]

doc0 :: Pandoc
doc0 = Pandoc nullMeta [ p "a", hdr 2 "h", blist, dv, fig, p "z" ]

assertRight :: Either a b -> IO b
assertRight (Right x) = pure x
assertRight (Left _)  = assertFailure "Expected Right but got Left" >> error "unreachable"

main :: IO ()
main = defaultMain $ testGroup "Text.Pandoc.Command.Simple (unit tests)"
  [ testCase "Replace: top-level block" $ do
      let sop = Replace (FocusPath [1]) (plain "X")
      d' <- assertRight $ applySimpleOps [sop] doc0
      (d' ^. body) @?= [ p "a", plain "X", blist, dv, fig, p "z" ]

  , testCase "InsertBefore: nested in BulletList (first item, position 1)" $ do
      let sop = InsertBefore (FocusPath [2,0,1]) (p "NEW")
      d' <- assertRight $ applySimpleOps [sop] doc0
      -- item 0 now has: li1-1, NEW, li1-2
      case (d' ^. body) !! 2 of
        BulletList (item0:_) -> item0 @?= [ p "li1-1", p "NEW", p "li1-2" ]
        _ -> assertFailure "Expected BulletList after edit"

  , testCase "InsertAfter: top-level after header" $ do
      let sop = InsertAfter (FocusPath [1]) (code "C")
      d' <- assertRight $ applySimpleOps [sop] doc0
      (d' ^. body) @?= [ p "a", hdr 2 "h", code "C", blist, dv, fig, p "z" ]

  , testCase "Delete: remove Div" $ do
      d' <- assertRight $ applySimpleOps [Delete (FocusPath [3])] doc0
      (length (d' ^. body)) @?= 5
      (d' ^. body) @?= [ p "a", hdr 2 "h", blist, fig, p "z" ]

  , testCase "WrapBlockQuote: wrap a paragraph" $ do
      d' <- assertRight $ applySimpleOps [WrapBlockQuote (FocusPath [0])] doc0
      case (d' ^. body) !! 0 of
        BlockQuote [b] -> b @?= p "a"
        _              -> assertFailure "Expected BlockQuote [original]"

  , testCase "WrapDiv: wrap a paragraph with given attr" $ do
      let a = ("wrap", ["box"], [("role","note")])
      d' <- assertRight $ applySimpleOps [WrapDiv (FocusPath [0]) a] doc0
      case (d' ^. body) !! 0 of
        Div a' [b] -> do
          a' @?= a
          b  @?= p "a"
        _ -> assertFailure "Expected Div attr [original]"

  , testCase "SetAttr: succeeds on Header" $ do
      let a = ("h", ["big"], [])
      d' <- assertRight $ applySimpleOps [SetAttr (FocusPath [1]) a] doc0
      case (d' ^. body) !! 1 of
        Header n a' xs -> do
          n  @?= 2
          a' @?= a
          xs @?= [Str "h"]
        _ -> assertFailure "Expected Header with new attr"

  , testCase "SetAttr: fails on Para" $ do
      applySimpleOps [SetAttr (FocusPath [0]) ("x",[],[])] doc0 @?=
        Left "set_attr: block has no attributes: Para [Str \"a\"]"

  , testCase "HeaderAdjust (set): Header level changed and clamped" $ do
      d' <- assertRight $ applySimpleOps [HeaderAdjust (FocusPath [1]) (Just 1) Nothing] doc0
      case (d' ^. body) !! 1 of
        Header n _ _ -> n @?= 1
        _ -> assertFailure "Expected Header"

  , testCase "HeaderAdjust (delta): clamp to 6" $ do
      d' <- assertRight $ applySimpleOps [HeaderAdjust (FocusPath [1]) Nothing (Just 10)] doc0
      case (d' ^. body) !! 1 of
        Header n _ _ -> n @?= 6
        _ -> assertFailure "Expected Header"

  , testCase "AppendInlines on Para" $ do
      let ins = [Space, Str "(continued)"]
      d' <- assertRight $ applySimpleOps [AppendInlines (FocusPath [5]) ins] doc0
      (d' ^. body) !! 5 @?= Para [Str "z", Space, Str "(continued)"]

  , testCase "AppendInlines fails on Header" $ do
      case applySimpleOps [AppendInlines (FocusPath [1]) [Str "!"]] doc0 of
        Left _ -> pure ()
        Right _ -> assertFailure "Expected AppendInlines to fail on Header"

  , testCase "applySimpleOpsJSON: array of commands equals manual" $ do
      let ops = [ Replace (FocusPath [0]) (p "A'")
                , InsertAfter (FocusPath [1]) (p "B")
                ]
      let encoded = encode ops
      d1 <- assertRight $ applySimpleOps ops doc0
      d2 <- assertRight $ applySimpleOpsJSON encoded doc0
      d1 @?= d2

  , testCase "Failure: invalid path (too deep) is an error" $ do
      case applySimpleOps [Delete (FocusPath [0,0])] doc0 of
        Left _ -> pure ()
        Right _ -> assertFailure "Expected failure for path that goes too deep"

  , testCase "Failure: OOB index is an error" $ do
      case applySimpleOps [Replace (FocusPath [42]) (p "nope")] doc0 of
        Left _ -> pure ()
        Right _ -> assertFailure "Expected failure for OOB index"
  , QC.testProperty "QuickCheck flags are recognized" $ QC.property True
  ]
