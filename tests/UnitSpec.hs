{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Main (main) where

-- Unit tests for each operation in Text.Pandoc.Command.Simple.
-- These are intended to be readable, regression-focused examples.

-- removed unused imports

import Control.Lens
import Data.Aeson (encode, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Text.Pandoc.Definition
import Text.Pandoc.Lens (body)

import Text.Pandoc.Command.Simple

p :: Text -> Block
p s = Para [Str s]

plain :: Text -> Block
plain s = Plain [Str s]

hdr :: Int -> Text -> Block
hdr n s = Header n nullAttr [Str s]

code :: Text -> Block
code = CodeBlock nullAttr

blist :: Block
blist =
  BulletList
    [ [p "li1-1", p "li1-2"],
      [p "li2-1"]
    ]

fig :: Block
fig = Figure ("fig-1", ["fclass"], []) (Caption Nothing []) [p "fig-body"]

dv :: Block
dv = Div ("dv-1", ["callout"], []) [p "in-div"]

doc0 :: Pandoc
doc0 = Pandoc nullMeta [p "a", hdr 2 "h", blist, dv, fig, p "z"]

assertRight :: Either a b -> IO b
assertRight (Right x) = pure x
assertRight (Left _) = assertFailure "Expected Right but got Left" >> error "unreachable"

-- | Assert that a SimpleOp can be encoded to JSON and parsed back successfully
-- This ensures JSON schema compliance
assertJSONCompliant :: SimpleOp -> IO ()
assertJSONCompliant sop =
  case parseEither parseJSON (toJSON sop) of
    Right sop' -> sop @?= sop'
    Left err -> assertFailure $ "JSON schema compliance failed for " ++ show sop ++ ": " ++ err

-- | Assert that a list of SimpleOps can be encoded to JSON and parsed back successfully
assertJSONCompliantList :: [SimpleOp] -> IO ()
assertJSONCompliantList ops =
  case parseEither parseJSON (toJSON ops) of
    Right ops' -> ops @?= ops'
    Left err -> assertFailure $ "JSON schema compliance failed for ops list: " ++ err

main :: IO ()
main =
  defaultMain $
    testGroup
      "Text.Pandoc.Command.Simple (unit tests)"
      [ testCase "Replace: top-level block" $ do
          let sop = Replace (FocusPath [1]) (plain "X")
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          (d' ^. body) @?= [p "a", plain "X", blist, dv, fig, p "z"],
        testCase "InsertBefore: nested in BulletList (first item, position 1)" $ do
          let sop = InsertBefore (FocusPath [2, 0, 1]) (p "NEW")
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          -- item 0 now has: li1-1, NEW, li1-2
          case (d' ^. body) !! 2 of
            BulletList (item0 : _) -> item0 @?= [p "li1-1", p "NEW", p "li1-2"]
            _ -> assertFailure "Expected BulletList after edit",
        testCase "InsertAfter: top-level after header" $ do
          let sop = InsertAfter (FocusPath [1]) (code "C")
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          (d' ^. body) @?= [p "a", hdr 2 "h", code "C", blist, dv, fig, p "z"],
        testCase "Delete: remove Div" $ do
          let sop = Delete (FocusPath [3])
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          length (d' ^. body) @?= 5
          (d' ^. body) @?= [p "a", hdr 2 "h", blist, fig, p "z"],
        testCase "WrapBlockQuote: wrap a paragraph" $ do
          let sop = WrapBlockQuote (FocusPath [0])
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          case (d' ^. body) !! 0 of
            BlockQuote [b] -> b @?= p "a"
            _ -> assertFailure "Expected BlockQuote [original]",
        testCase "WrapDiv: wrap a paragraph with given attr" $ do
          let a = ("wrap", ["box"], [("role", "note")])
              sop = WrapDiv (FocusPath [0]) a
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          case (d' ^. body) !! 0 of
            Div a' [b] -> do
              a' @?= a
              b @?= p "a"
            _ -> assertFailure "Expected Div attr [original]",
        testCase "SetAttr: succeeds on Header" $ do
          let a = ("h", ["big"], [])
              sop = SetAttr (FocusPath [1]) a
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          case (d' ^. body) !! 1 of
            Header n a' xs -> do
              n @?= 2
              a' @?= a
              xs @?= [Str "h"]
            _ -> assertFailure "Expected Header with new attr",
        testCase "SetAttr: fails on Para" $ do
          let sop = SetAttr (FocusPath [0]) ("x", [], [])
          assertJSONCompliant sop
          applySimpleOps [sop] doc0
            @?= Left "set_attr: block has no attributes: Para [Str \"a\"]",
        testCase "HeaderAdjust (set): Header level changed and clamped" $ do
          let sop = HeaderAdjust (FocusPath [1]) (Just 1) Nothing
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          case (d' ^. body) !! 1 of
            Header n _ _ -> n @?= 1
            _ -> assertFailure "Expected Header",
        testCase "HeaderAdjust (delta): clamp to 6" $ do
          let sop = HeaderAdjust (FocusPath [1]) Nothing (Just 10)
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          case (d' ^. body) !! 1 of
            Header n _ _ -> n @?= 6
            _ -> assertFailure "Expected Header",
        testCase "AppendInlines on Para" $ do
          let ins = [Space, Str "(continued)"]
              sop = AppendInlines (FocusPath [5]) ins
          assertJSONCompliant sop
          d' <- assertRight $ applySimpleOps [sop] doc0
          (d' ^. body) !! 5 @?= Para [Str "z", Space, Str "(continued)"],
        testCase "AppendInlines fails on Header" $ do
          let sop = AppendInlines (FocusPath [1]) [Str "!"]
          assertJSONCompliant sop
          case applySimpleOps [sop] doc0 of
            Left _ -> pure ()
            Right _ -> assertFailure "Expected AppendInlines to fail on Header",
        testCase "applySimpleOpsJSON: array of commands equals manual" $ do
          let ops =
                [ Replace (FocusPath [0]) (p "A'"),
                  InsertAfter (FocusPath [1]) (p "B")
                ]
          assertJSONCompliantList ops
          let encoded = encode ops
          d1 <- assertRight $ applySimpleOps ops doc0
          d2 <- assertRight $ applySimpleOpsJSON encoded doc0
          d1 @?= d2,
        testCase "Failure: invalid path (too deep) is an error" $ do
          case applySimpleOps [Delete (FocusPath [0, 0])] doc0 of
            Left _ -> pure ()
            Right _ -> assertFailure "Expected failure for path that goes too deep",
        testCase "Failure: OOB index is an error" $ do
          case applySimpleOps [Replace (FocusPath [42]) (p "nope")] doc0 of
            Left _ -> pure ()
            Right _ -> assertFailure "Expected failure for OOB index",
        -- JSON Schema Compliance Tests
        testCase "JSON Schema: All operation types with FocusIndex" $ do
          let ops =
                [ Replace (FocusIndex 0) (p "test"),
                  InsertBefore (FocusIndex 1) (code "test"),
                  InsertAfter (FocusIndex 2) (hdr 1 "test"),
                  Delete (FocusIndex 3),
                  WrapBlockQuote (FocusIndex 4),
                  WrapDiv (FocusIndex 5) ("id", ["class"], [("key", "value")]),
                  SetAttr (FocusIndex 1) ("new-id", ["new-class"], []),
                  HeaderAdjust (FocusIndex 1) (Just 3) Nothing,
                  HeaderAdjust (FocusIndex 1) Nothing (Just 1),
                  HeaderAdjust (FocusIndex 1) (Just 2) (Just (-1)),
                  AppendInlines (FocusIndex 0) [Str "appended"]
                ]
          mapM_ assertJSONCompliant ops,
        testCase "JSON Schema: All operation types with FocusPath" $ do
          let ops =
                [ Replace (FocusPath [0]) (p "test"),
                  InsertBefore (FocusPath [1, 0]) (code "test"),
                  InsertAfter (FocusPath [2, 1, 0]) (hdr 1 "test"),
                  Delete (FocusPath [3, 0, 1]),
                  WrapBlockQuote (FocusPath [4]),
                  WrapDiv (FocusPath [5, 0]) ("id", ["class"], [("key", "value")]),
                  SetAttr (FocusPath [1]) ("new-id", ["new-class"], []),
                  HeaderAdjust (FocusPath [1]) (Just 3) Nothing,
                  HeaderAdjust (FocusPath [1]) Nothing (Just 1),
                  HeaderAdjust (FocusPath [1]) (Just 2) (Just (-1)),
                  AppendInlines (FocusPath [0]) [Str "appended"]
                ]
          mapM_ assertJSONCompliant ops,
        testCase "JSON Schema: Complex nested operations list" $ do
          let ops =
                [ Replace (FocusPath [0]) (p "A"),
                  Delete (FocusPath [1]),
                  InsertAfter (FocusIndex 0) (p "B"),
                  WrapDiv (FocusPath [2]) ("wrapper", ["box"], [("data-test", "true")])
                ]
          assertJSONCompliantList ops,
        QC.testProperty "QuickCheck flags are recognized" $ QC.property True
      ]
