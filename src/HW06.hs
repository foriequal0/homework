{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HW06 where

import           Data.Aeson
import           Data.Char
import           Data.Monoid
import           GHC.Generics

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

parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode


data Market = Market { marketname :: T.Text,
                       x          :: Float,
                       y          :: Float,
                       state      :: T.Text}
            deriving (Show, Generic)

instance FromJSON Market

fromResult :: Result a -> Either String a
fromResult (Error s) = Left s
fromResult (Success a) = Right a

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets s = do
  parsedValue <- parseData s
  let result = fromJSON parsedValue :: Result [Market]
  case result of
    Error err -> fail err
    Success a -> return a

loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "markets.json"
  let eitherMarket = parseMarkets(filedata)
  case eitherMarket of
    Left err -> fail err
    Right a -> return a

data OrdList a = OrdList { getOrdList :: [a] }
                 deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    (OrdList lhs) `mappend` (OrdList rhs) = OrdList $ appendInOrder lhs rhs
       where appendInOrder [] r = r
             appendInOrder l [] = l
             appendInOrder l@(x:xs) r@(y:ys)
                 | x <= y = x : (xs `appendInOrder`r)
                 | otherwise = y : (l `appendInOrder` ys)
