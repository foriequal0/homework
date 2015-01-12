{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HW06 where

import           Data.Aeson
import           Data.Monoid
import           GHC.Generics
import           Data.Char

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

ynToBool :: Value -> Value
ynToBool (String s) = 
    case T.toLower(s) of
      "y" -> Bool True
      "n" -> Bool False
      _   -> String s
ynToBool (Object o) = Object $ fmap ynToBool o
ynToBool (Array a) = Array $ fmap ynToBool a
ynToBool v = v


