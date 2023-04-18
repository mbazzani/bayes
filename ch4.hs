--Program to answer exercise 4.2 in E.T. Jaynes' Probability Theory.
--To test alternate data, simply change numBadWidgets/numGoodWidgets
--To change the hypothesis being tested, simply edit odds and the update function
--that provides the update rules for each hypothesis
import Data.Ratio

numBadWidgets :: Integer
numBadWidgets = 33

numGoodWidgets :: Integer
numGoodWidgets = 66

data Hypothesis = Hypothesis
  { priorOdds :: Ratio Integer,
    badWidgetProbability :: Ratio Integer
  }
  deriving (Show)

hypothesisList =
  [ Hypothesis
      { priorOdds = 1 % 11,
        badWidgetProbability = 1 % 3
      },
    Hypothesis
      { priorOdds = 10 % 11,
        badWidgetProbability = 1 % 6
      },
    Hypothesis
      { priorOdds = 1 % (10 ^ 6),
        badWidgetProbability = 99 % 100
      }
  ]

odds :: [Ratio Integer]
odds = map priorOdds hypothesisList

main :: IO ()
main = do
  let posteriorOdds = update hypothesisList odds numBadWidgets numGoodWidgets
      floatUpdatedOdds = map fromRational posteriorOdds
      probabilities = map (\x -> x / sum floatUpdatedOdds) floatUpdatedOdds
  print ("Total good widgets: " ++ show numGoodWidgets)
  print ("Total bad widgets: " ++ show numBadWidgets)
  putStr $ concat $ map showHypothesisAndProbabilities (zip hypothesisList probabilities)

showHypothesisAndProbabilities :: Show a => (Hypothesis, a) -> String
showHypothesisAndProbabilities (hypothesis, probability) =
  "\n\nPrior Odds: "
    ++ (show $ fromRational $ priorOdds hypothesis)
    ++ "\nPredicted Bad Widget Frequency: "
    ++ (show $ fromRational $ badWidgetProbability hypothesis)
    ++ "\nLikelihood after observing the Data: "
    ++ show probability

update :: (Ord t, Ord a, Num t, Num a) => [Hypothesis] -> [Ratio Integer] -> t -> a -> [Ratio Integer]
update hypothesisList odds numBadWidgets numGoodWidgets
  | numBadWidgets > 0 = update hypothesisList badUpdateOdds (numBadWidgets - 1) numGoodWidgets
  | numGoodWidgets > 0 = update hypothesisList goodUpdateOdds numBadWidgets (numGoodWidgets - 1)
  | otherwise = odds
  where
    goodWidgetProbability hypothesis = 1 - (badWidgetProbability hypothesis)
    badUpdateOdds = zipWith (*) odds (map badWidgetProbability hypothesisList)
    goodUpdateOdds = zipWith (*) odds (map goodWidgetProbability hypothesisList)
