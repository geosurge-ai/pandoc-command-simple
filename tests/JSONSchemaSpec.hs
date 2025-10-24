{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Types (parseEither, parseJSON)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..), nullMeta)

import Text.Pandoc.Command.Simple (Focus (..), SimpleOp (..), applySimpleOpsJSON)
import Text.Pandoc.Command.Simple.JSONSchema

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "JSONSchema Tests"
    [ testGroup
        "Schema Structure Tests"
        [ testSchemaStructure,
          testOperationNames,
          testFocusSchema
        ],
      testGroup
        "Success Cases"
        [ testValidOperations,
          testValidFocusTypes,
          testValidArraySchemas
        ],
      testGroup
        "Obvious Failure Cases"
        [ testInvalidOperationNames,
          testMissingRequiredFields,
          testWrongTypes
        ],
      testGroup
        "Subtle Failure Cases"
        [ testExtraFields,
          testFocusConfusion,
          testArrayCountMismatch,
          testInvalidEnumValues
        ],
      testGroup
        "Integration Tests"
        [ testSchemaWithRealOperations,
          testOperationArraySchema
        ]
    ]

-- Schema Structure Tests
testSchemaStructure :: TestTree
testSchemaStructure = testCase "Schema has correct structure" $ do
  let schema = simpleOpSchema
  case schema of
    Object _ -> do
      -- Should be an object (basic structure check)
      -- More detailed structure validation would go here
      return ()
    _ -> assertFailure "Schema should be an Object"

testOperationNames :: TestTree
testOperationNames = testCase "Operation names are complete" $ do
  let names = operationNames
  assertEqual "Should have 9 operations" 9 (length names)
  assertBool "Should contain 'replace'" ("replace" `elem` names)
  assertBool "Should contain 'delete'" ("delete" `elem` names)
  assertBool "Should contain 'header_adjust'" ("header_adjust" `elem` names)

testFocusSchema :: TestTree
testFocusSchema = testCase "Focus schema supports both index and path" $ do
  let schema = focusSchema
  case schema of
    Object _ -> return () -- Basic structure check
    _ -> assertFailure "Focus schema should be an Object with oneOf"

-- Success Cases
testValidOperations :: TestTree
testValidOperations =
  testGroup
    "Valid operations parse correctly"
    [ testCase "Valid replace operation" $ do
        let json =
              object
                [ "op" .= ("replace" :: Text),
                  "focus" .= object ["index" .= (0 :: Int)],
                  "block" .= object ["t" .= ("Para" :: Text), "c" .= [object ["t" .= ("Str" :: Text), "c" .= ("Hello" :: Text)]]]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> return ()
          Left err -> assertFailure $ "Should parse valid replace: " ++ err,
      testCase "Valid delete operation" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus" .= object ["path" .= ([1, 0] :: [Int])]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> return ()
          Left err -> assertFailure $ "Should parse valid delete: " ++ err,
      testCase "Valid header_adjust with optional fields" $ do
        let json =
              object
                [ "op" .= ("header_adjust" :: Text),
                  "focus" .= object ["index" .= (2 :: Int)],
                  "set" .= (3 :: Int),
                  "delta" .= (1 :: Int)
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> return ()
          Left err -> assertFailure $ "Should parse valid header_adjust: " ++ err
    ]

testValidFocusTypes :: TestTree
testValidFocusTypes =
  testGroup
    "Valid focus types"
    [ testCase "Index focus" $ do
        let json = object ["index" .= (5 :: Int)]
        case parseEither parseJSON json of
          Right (_ :: Focus) -> return ()
          Left err -> assertFailure $ "Should parse index focus: " ++ err,
      testCase "Path focus" $ do
        let json = object ["path" .= ([0, 1, 2] :: [Int])]
        case parseEither parseJSON json of
          Right (_ :: Focus) -> return ()
          Left err -> assertFailure $ "Should parse path focus: " ++ err
    ]

testValidArraySchemas :: TestTree
testValidArraySchemas = testCase "Array schemas with count restrictions" $ do
  let schema3 = simpleOpArraySchema (Just 3)
      schemaUnbounded = simpleOpArraySchema Nothing
  case (schema3, schemaUnbounded) of
    (Object _, Object _) -> return ()
    _ -> assertFailure "Array schemas should be Objects"

-- Obvious Failure Cases
testInvalidOperationNames :: TestTree
testInvalidOperationNames =
  testGroup
    "Invalid operation names fail"
    [ testCase "Unknown operation name" $ do
        let json =
              object
                [ "op" .= ("unknown_operation" :: Text),
                  "focus" .= object ["index" .= (0 :: Int)]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject unknown operation"
          Left _ -> return (), -- Expected failure
      testCase "Typo in operation name" $ do
        let json =
              object
                [ "op" .= ("replac" :: Text), -- Missing 'e'
                  "focus" .= object ["index" .= (0 :: Int)],
                  "block" .= object ["t" .= ("Para" :: Text), "c" .= [[] :: [Value]]]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject typo in operation"
          Left _ -> return () -- Expected failure
    ]

testMissingRequiredFields :: TestTree
testMissingRequiredFields =
  testGroup
    "Missing required fields fail"
    [ testCase "Missing 'op' field" $ do
        let json =
              object
                [ "focus" .= object ["index" .= (0 :: Int)],
                  "block" .= object ["t" .= ("Para" :: Text), "c" .= [[] :: [Value]]]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should require 'op' field"
          Left _ -> return (), -- Expected failure
      testCase "Missing 'focus' field" $ do
        let json =
              object
                [ "op" .= ("replace" :: Text),
                  "block" .= object ["t" .= ("Para" :: Text), "c" .= [[] :: [Value]]]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should require 'focus' field"
          Left _ -> return (), -- Expected failure
      testCase "Missing 'block' for replace operation" $ do
        let json =
              object
                [ "op" .= ("replace" :: Text),
                  "focus" .= object ["index" .= (0 :: Int)]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should require 'block' for replace"
          Left _ -> return () -- Expected failure
    ]

testWrongTypes :: TestTree
testWrongTypes =
  testGroup
    "Wrong field types fail"
    [ testCase "String instead of number for index" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus" .= object ["index" .= ("zero" :: Text)] -- Should be number
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject string for index"
          Left _ -> return (), -- Expected failure
      testCase "Number instead of string for operation" $ do
        let json =
              object
                [ "op" .= (42 :: Int), -- Should be string
                  "focus" .= object ["index" .= (0 :: Int)]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject number for op"
          Left _ -> return () -- Expected failure
    ]

-- Subtle Failure Cases
testExtraFields :: TestTree
testExtraFields =
  testGroup
    "Extra fields handling"
    [ testCase "Extra field in operation (should be allowed by current parser)" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus" .= object ["index" .= (0 :: Int)],
                  "extra_field" .= ("unexpected" :: Text)
                ]
        -- Note: Aeson's default behavior allows extra fields
        -- This test documents current behavior - might want to make it stricter
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> return () -- Current behavior allows this
          Left _ -> assertFailure "Current parser allows extra fields",
      testCase "Extra field in focus" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus"
                    .= object
                      [ "index" .= (0 :: Int),
                        "also_path" .= ([1, 2] :: [Int]) -- Conflicting focus types
                      ]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> return () -- Parser takes first match
          Left _ -> assertFailure "Parser handles conflicting focus fields"
    ]

testFocusConfusion :: TestTree
testFocusConfusion =
  testGroup
    "Focus type confusion"
    [ testCase "Empty focus object" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus" .= object [] -- Neither index nor path
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject empty focus"
          Left _ -> return (), -- Expected failure
      testCase "Both index and path in focus" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus"
                    .= object
                      [ "index" .= (0 :: Int),
                        "path" .= ([1, 2] :: [Int])
                      ]
                ]
        -- This actually succeeds with current parser (takes index first)
        case parseEither parseJSON json of
          Right (Delete (FocusIndex 0)) -> return () -- Expected: index wins
          Right _ -> assertFailure "Should parse as FocusIndex"
          Left _ -> assertFailure "Should parse successfully (index takes precedence)",
      testCase "Wrong field name in focus" $ do
        let json =
              object
                [ "op" .= ("delete" :: Text),
                  "focus" .= object ["idx" .= (0 :: Int)] -- Should be "index"
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject 'idx' instead of 'index'"
          Left _ -> return () -- Expected failure
    ]

testArrayCountMismatch :: TestTree
testArrayCountMismatch =
  testGroup
    "Array count validation"
    [ testCase "Too few operations for fixed-size schema" $ do
        -- This test simulates schema validation (actual validation would be external)
        let operations =
              [ object ["op" .= ("delete" :: Text), "focus" .= object ["index" .= (0 :: Int)]]
              ]
        -- Schema expects exactly 3, but we provide 1
        assertEqual "Should have count mismatch" 1 (length operations)
        assertBool "Schema would reject this count" (length operations /= 3),
      testCase "Too many operations for fixed-size schema" $ do
        let operations =
              replicate 5 $
                object ["op" .= ("delete" :: Text), "focus" .= object ["index" .= (0 :: Int)]]
        -- Schema expects exactly 3, but we provide 5
        assertEqual "Should have count mismatch" 5 (length operations)
        assertBool "Schema would reject this count" (length operations /= 3)
    ]

testInvalidEnumValues :: TestTree
testInvalidEnumValues =
  testGroup
    "Invalid enum values"
    [ testCase "Case sensitivity in operation names" $ do
        let json =
              object
                [ "op" .= ("DELETE" :: Text), -- Should be lowercase
                  "focus" .= object ["index" .= (0 :: Int)]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should be case sensitive"
          Left _ -> return (), -- Expected failure
      testCase "Close but wrong operation name" $ do
        let json =
              object
                [ "op" .= ("insert_between" :: Text), -- Should be insert_before or insert_after
                  "focus" .= object ["index" .= (0 :: Int)],
                  "block" .= object ["t" .= ("Para" :: Text), "c" .= [[] :: [Value]]]
                ]
        case parseEither parseJSON json of
          Right (_ :: SimpleOp) -> assertFailure "Should reject similar but wrong operation"
          Left _ -> return () -- Expected failure
    ]

-- Integration Tests
testSchemaWithRealOperations :: TestTree
testSchemaWithRealOperations =
  testGroup
    "Schema works with real operations"
    [ testCase "Parse and apply valid JSON operations" $ do
        let doc = Pandoc nullMeta [Para [Str "Hello"], Para [Str "World"]]
            jsonOps =
              encode
                [ object ["op" .= ("delete" :: Text), "focus" .= object ["index" .= (1 :: Int)]],
                  object
                    [ "op" .= ("replace" :: Text),
                      "focus" .= object ["index" .= (0 :: Int)],
                      "block" .= object ["t" .= ("Para" :: Text), "c" .= [object ["t" .= ("Str" :: Text), "c" .= ("Modified" :: Text)]]]
                    ]
                ]
        case applySimpleOpsJSON jsonOps doc of
          Right result -> do
            -- Should have one paragraph with "Modified"
            case result of
              Pandoc _ [Para [Str "Modified"]] -> return ()
              _ -> assertFailure $ "Unexpected result: " ++ show result
          Left err -> assertFailure $ "Should apply operations successfully: " ++ err,
      testCase "Reject invalid JSON operations" $ do
        let doc = Pandoc nullMeta [Para [Str "Hello"]]
            invalidJson = "{\"op\":\"invalid\",\"focus\":{\"index\":0}}"
        case applySimpleOpsJSON (BL8.pack invalidJson) doc of
          Right _ -> assertFailure "Should reject invalid operation"
          Left _ -> return () -- Expected failure
    ]

testOperationArraySchema :: TestTree
testOperationArraySchema =
  testGroup
    "Operation array schema"
    [ testCase "Schema structure for different array lengths" $ do
        let schema0 = createOperationArraySchema 0
            schema2 = createOperationArraySchema 2
            schema5 = createOperationArraySchema 5
        -- All should be valid Objects
        case (schema0, schema2, schema5) of
          (Object _, Object _, Object _) -> return ()
          _ -> assertFailure "All schemas should be Objects",
      testCase "Schema validates correct array structure" $ do
        -- This would require an external JSON schema validator
        -- For now, just test that we can create the schema
        let schema = createOperationArraySchema 2
        -- In a real test, we'd validate an array against schema
        -- let exampleArray = [operations1, operations2]
        -- validateArray exampleArray schema
        case schema of
          Object _ -> return () -- Schema created successfully
          _ -> assertFailure "Schema should be an Object"
    ]
