module HW08 where

import Data.List (stripPrefix, sort)
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Text.Read
import Control.Monad
import Control.Monad.Random

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
	where go str = do
		n  <- readMaybe $ take 1 str
		stripped <- stripPrefix (replicate n 'a') $ drop 1 str
		case stripped of
			[] -> Just ()
			_  -> go stripped

specialNumbers :: [Int]
specialNumbers = [ x | x <- [1..100], (x `mod` 5 == 0) && (x `mod` 7 /= 0)]

type StdRand = Rand StdGen

type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
	deriving Show

type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

instance Monoid ArmyCounts where
	mempty = ArmyCounts 0 0
	ArmyCounts latk ldef `mappend` ArmyCounts ratk rdef =
		ArmyCounts (latk + ratk) (ldef + rdef)

roundResult :: (DieRoll, DieRoll) -> ArmyCounts
roundResult (f,s)
    | f > s = ArmyCounts 0 (-1)
	| otherwise = ArmyCounts (-1) 0

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults atk def =
	fold $ map roundResult $ zip (sortDie atk) (sortDie def)
	where sortDie = reverse . sort

limit :: (Ord a) => (a, a) -> a -> a	
limit (lower,upper) x
	| x >= upper = upper
	| x <= lower = lower
	| otherwise  = x

battle :: ArmyCounts -> StdRand ArmyCounts
battle (ArmyCounts atk def) = do
	let rollAtk = limit (0, 3) (atk - 1)
	    rollDef = limit (0, 2) (def)
	diceAtk <- replicateM rollAtk dieRoll
	diceDef <- replicateM rollDef dieRoll
	return $ battleResults diceAtk diceDef

invade :: ArmyCounts -> StdRand ArmyCounts
invade army = do
	if attackers army < 2 || defenders army == 0
	then return $ army
	else do
		result <- battle army
		invade $ army <> result

successProb :: ArmyCounts -> StdRand Double
successProb army = do
	result <- replicateM 1000 $ invade army
	let successCount = Prelude.sum [ 1 | ArmyCounts _ 0 <- result ] :: Integer
	return $ (fromIntegral successCount) / 1000.0
