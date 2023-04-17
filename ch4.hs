import Data.Ratio

numBadWidgets = 33

numGoodWidgets = 66

odds = [10 ^ 6 % 11, 10 ^ 7 % 11, 1]

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