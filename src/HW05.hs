module HW05 where

import Data.Maybe ( listToMaybe )

import Ring
import Parser

intParsingWork :: Bool
intParsingWork = (parse "3" == Just (3::Integer, "")) &&
                 (parseRing "1 + 2 + 5" == Just (11 :: Integer)) &&
                 (addId == (0 :: Integer))

data Mod5 = MkMod Integer 
            deriving (Show, Eq)

fromMod5 :: Mod5 -> Integer
fromMod5 (MkMod i) = i

instance Ring Mod5 where
    addId = MkMod 0
    addInv x = MkMod ( 5 - fromMod5 x)
    mulId = MkMod 0

    add x y = MkMod ((fromMod5 x + fromMod5 y) `mod` 5)
    mul x y = MkMod ((fromMod5 x * fromMod5 y) `mod` 5)

instance Parsable Mod5 where
    parse s = Just (MkMod ( parsed `mod` 5 ), residue)
        where Just (parsed, residue) = parse s :: Maybe(Integer, String)
              
data Mat2x2 = MkMat2x2 Integer Integer Integer Integer deriving (Show, Eq)

instance Ring Mat2x2 where
    addId = MkMat2x2 0 0 0 0
    addInv MkMat2x2 a b c d = MkMat2x2 -a -b -c -d
    mulId = MkMat2x2 1 0 0 1

    add (MkMat2x2 a11 a12 a21 a22) (MkMat2x2 b11 b12 b21 b22) = MkMat2x2 (a11+b11) (a12+b12) (a21+b21) (a22+b22)
    mul (MkMat2x2 a11 a12 a21 a22) (MkMat2x2 b11 b12 b21 b22) = 
        MkMat2x2 (a11*b11 + a12*b21) (a11*b12 + a12*b22) \
                 (a21*b11 + a22*b21) (a21*b12 + a22*b22)

instance Parsable Mat2x2 where
    parse = evalParser (parseMat2x2 <* eof)

parens2 :: Parser a -> Parser a
parens2 p = eatSpace *> char '[' *> p <* eatSpace <* char ']'

parseMat2x2 :: Parser Mat2x2
parseMat2x2 = 
    where 
