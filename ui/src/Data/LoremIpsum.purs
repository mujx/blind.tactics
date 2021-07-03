module Blind.Data.FFI.LoremIpsum where

foreign import generateWords :: Int -> String

foreign import generateSentences :: Int -> String
