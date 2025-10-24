{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.Pandoc.Command.Simple.JSONSchema
-- Description : JSON Schema definitions for SimpleOp operations
-- Copyright   : (c) Your Name, 2024
-- License     : BSD-3-Clause
-- Maintainer  : you@example.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides JSON Schema definitions for the SimpleOp operations
-- defined in "Text.Pandoc.Command.Simple". These schemas are useful for:
--
-- * API validation
-- * Documentation generation
-- * Client library generation
-- * Structured data validation
--
-- == Usage Examples
--
-- Generate a schema for a single SimpleOp:
--
-- >>> simpleOpSchema
-- Object (fromList [("type",String "object"),("properties",...)])
--
-- Generate a schema for an array of SimpleOps with a specific count:
--
-- >>> simpleOpArraySchema 3
-- Object (fromList [("type",String "array"),("items",...),("minItems",Number 3.0),("maxItems",Number 3.0)])
--
-- Generate a schema for focus addressing:
--
-- >>> focusSchema
-- Object (fromList [("oneOf",[Object (fromList [("type",String "object"),...]),...])
--
-- == Schema Structure
--
-- The schemas follow JSON Schema Draft 7 specification and include:
--
-- * Strict type definitions for all operation fields
-- * Enumerated values for operation names
-- * Proper handling of optional fields
-- * Validation for focus addressing (index vs path)
-- * Support for Pandoc AST structures
--
-- == Integration with APIs
--
-- These schemas are particularly useful when working with APIs that require
-- structured input validation. The schemas ensure that JSON data conforms
-- to the expected structure for SimpleOp operations.
module Text.Pandoc.Command.Simple.JSONSchema (
  -- * Schema Definitions
  simpleOpSchema,
  simpleOpArraySchema,
  focusSchema,
  pandocBlockSchema,
  pandocAttrSchema,
  pandocInlineSchema,

  -- * Schema Utilities
  operationNames,
  createOperationArraySchema,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

-- | All supported operation names for SimpleOp
operationNames :: [Text]
operationNames =
  [ "replace",
    "insert_before",
    "insert_after",
    "delete",
    "wrap_blockquote",
    "wrap_div",
    "set_attr",
    "header_adjust",
    "append_inlines"
  ]

-- | JSON Schema for Focus addressing (either index or path)
focusSchema :: Value
focusSchema =
  object
    [ "oneOf"
        .= [ object
               [ "type" .= ("object" :: Text),
                 "properties"
                   .= object
                     [ "index" .= object ["type" .= ("number" :: Text)]
                     ],
                 "required" .= (["index"] :: [Text]),
                 "additionalProperties" .= False
               ],
             object
               [ "type" .= ("object" :: Text),
                 "properties"
                   .= object
                     [ "path"
                         .= object
                           [ "type" .= ("array" :: Text),
                             "items" .= object ["type" .= ("number" :: Text)]
                           ]
                     ],
                 "required" .= (["path"] :: [Text]),
                 "additionalProperties" .= False
               ]
           ]
    ]

-- | JSON Schema for Pandoc Block elements
-- This is a simplified schema that accepts any object structure
-- since Pandoc blocks have complex nested structures
pandocBlockSchema :: Value
pandocBlockSchema =
  object
    [ "type" .= ("object" :: Text),
      "description" .= ("Pandoc Block element with 't' tag and 'c' content" :: Text)
    ]

-- | JSON Schema for Pandoc Attr (attributes tuple as array)
pandocAttrSchema :: Value
pandocAttrSchema =
  object
    [ "type" .= ("array" :: Text),
      "description" .= ("Pandoc attributes as [id, classes, key-value pairs]" :: Text),
      "minItems" .= (3 :: Int),
      "maxItems" .= (3 :: Int)
    ]

-- | JSON Schema for Pandoc Inline elements
pandocInlineSchema :: Value
pandocInlineSchema =
  object
    [ "type" .= ("array" :: Text),
      "items" .= object ["type" .= ("object" :: Text)],
      "description" .= ("Array of Pandoc Inline elements" :: Text)
    ]

-- | JSON Schema for a single SimpleOp operation
simpleOpSchema :: Value
simpleOpSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "op"
              .= object
                [ "type" .= ("string" :: Text),
                  "enum" .= operationNames,
                  "description" .= ("The operation to perform" :: Text)
                ],
            "focus" .= focusSchema,
            "block" .= pandocBlockSchema,
            "attr" .= pandocAttrSchema,
            "set"
              .= object
                [ "type" .= ("number" :: Text),
                  "description" .= ("Set header level to this value (for header_adjust)" :: Text)
                ],
            "delta"
              .= object
                [ "type" .= ("number" :: Text),
                  "description" .= ("Adjust header level by this amount (for header_adjust)" :: Text)
                ],
            "inlines" .= pandocInlineSchema
          ],
      "required" .= (["op", "focus"] :: [Text]),
      "additionalProperties" .= False,
      "description" .= ("A single SimpleOp operation for Pandoc document editing" :: Text)
    ]

-- | JSON Schema for an array of SimpleOp operations
-- Optionally specify exact count with minItems/maxItems
simpleOpArraySchema :: Maybe Int -> Value
simpleOpArraySchema mCount =
  object $
    [ "type" .= ("array" :: Text),
      "items" .= simpleOpSchema,
      "description" .= ("Array of SimpleOp operations to apply in sequence" :: Text)
    ]
      ++ case mCount of
        Nothing -> []
        Just count ->
          [ "minItems" .= count,
            "maxItems" .= count
          ]

-- | Create a schema for an array of SimpleOp operations with exact length N
-- This generates a schema that validates arrays containing exactly N operation arrays
createOperationArraySchema :: Int -> Value
createOperationArraySchema arrayLength =
  object
    [ "type" .= ("array" :: Text),
      "items"
        .= object
          [ "oneOf"
              .= [ simpleOpArraySchema Nothing, -- Array of SimpleOps
                   object ["type" .= ("null" :: Text)] -- null for no operations
                 ]
          ],
      "minItems" .= arrayLength,
      "maxItems" .= arrayLength,
      "description" .= ("Array of operation lists with exact length " <> T.pack (show arrayLength))
    ]
