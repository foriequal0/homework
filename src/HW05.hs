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
    addInv (MkMat2x2 a b c d) = MkMat2x2 (negate a) (negate b) (negate c) (negate d)
    mulId = MkMat2x2 1 0 0 1

    add (MkMat2x2 a11 a12 a21 a22) (MkMat2x2 b11 b12 b21 b22) = MkMat2x2 (a11+b11) (a12+b12) (a21+b21) (a22+b22)
    mul (MkMat2x2 a11 a12 a21 a22) (MkMat2x2 b11 b12 b21 b22) = 
        MkMat2x2 (a11*b11 + a12*b21) (a11*b12 + a12*b22) (a21*b11 + a22*b21) (a21*b12 + a22*b22)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe a
safeTail [] = Nothing
safeTail s = Just (last s)

instance Parsable Mat2x2 where
    parse str= do 
      let ('[':xs) = str
      let [((a:b:_), v)] = (reads xs) :: [([Integer], String)]
      let [((c:d:_), w)] = (reads v) :: [([Integer], String)]
      let (']':x) = w
      return $ ((MkMat2x2 a b c d), x)

instance Ring Bool where
    addId = False
    addInv = not
    mulId = True
    add = (==)
    mul = (&&)

instance Parsable Bool where
    parse = listToMaybe . reads

distribute :: (Ring a) => RingExpr a -> RingExpr a
distribute (AddInv a) = AddInv $ distribute a
distribute (Add a b) = Add (distribute a) (distribute b)
distribute (Mul (Add a b) c) = distribute $ Add (Mul a c) (Mul b c)
distribute (Mul a (Add b c)) = distribute $ Add (Mul a b) (Mul a c)
distribute other = other

squashMulId :: (Eq a, Ring a) => RingExpr a -> RingExpr a
squashMulId (Lit lit) 
    | lit == mulId = MulId
    | otherwise    = Lit lit
squashMulId (AddInv x) = AddInv $ squashMulId $ x
squashMulId (Add x y) = Add (squashMulId x) (squashMulId y)
squashMulId (Mul x y) = subSquash sqx sqy
    where sqx = squashMulId x
          sqy = squashMulId y
          subSquash MulId MulId = MulId
          subSquash MulId x' = x'
          subSquash x' MulId = x'
          subSquash x' y' = Mul x' y'
squashMulId other = other
