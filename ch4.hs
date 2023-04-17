--Program to answer exercise 4.2 in E.T. Jaynes' Probability Theory.
--To test alternate data, simply change numBadWidgets/numGoodWidgets
--To change the hypothesis being tested, simply edit odds and the update function
--that provides the update rules for each hypothesis

import Data.Ratio
import Language.Haskell.Exts (function)

numBadWidgets :: Integer
numBadWidgets = 33

numGoodWidgets :: Integer
numGoodWidgets = 66

odds :: [Ratio Integer]
odds = [10 ^ 6 % 11, 10 ^ 7 % 11, 1]

update :: (Integral a, Integral b1, Integral b2) => [Ratio a] -> b1 -> b2 -> [Ratio a]
update startingOdds numBadWidgets numGoodWidgets =
  [ startingOdds !! 0 * (1 % 3) ^ numBadWidgets * (2 % 3) ^ numGoodWidgets,
    startingOdds !! 1 * (1 % 6) ^ numBadWidgets * (5 % 6) ^ numGoodWidgets,
    startingOdds !! 2 * (99 % 100) ^ numBadWidgets * (1 % 100) ^ numGoodWidgets
  ]

main :: IO ()
main = do
  let updatedOdds = update odds numBadWidgets numGoodWidgets
  let floatUpdatedOdds = map fromRational updatedOdds
  let probabilities = map (\x -> x / sum floatUpdatedOdds) floatUpdatedOdds
  --print updatedOdds
  print probabilities
