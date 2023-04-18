--This code was created to solve exercise 3.2 in E.T Jaynes' Probability Theory Textbook
--The problem is as follows:
--Suppose an urn contains N = \sum{Ni} balls, N1 of color 1, N2 of color
--2, . . . , Nk of color k. We draw m balls without replacement; what is the probability
--that we have at least one of each color? Supposing k = 5, all Ni = 10, how many do
--we need to draw in order to have at least a 90% probability for getting a full set?

ballsPerColor = 10

numColors = 5

totalBalls = ballsPerColor * numColors

numToDraw = 15

choose :: Integral p => p -> p -> p
choose n k
  | k > n = 0
  | otherwise = (fac n) `div` ((fac k) * (fac (n - k)))

fac :: (Num a, Enum a) => a -> a
fac n = product [1 .. n]

probability :: (Fractional a1, Integral a2) => a2 -> a2 -> a2 -> a1
probability totalNum numOfColor numToDraw =
  fromIntegral (choose (totalNum - numOfColor) numToDraw) / fromIntegral (choose totalNum numToDraw)

--Sums the probability of at least one color not being chosen
sumProbability :: (Fractional a1, Num a, Eq a) => Integer -> Integer -> a -> a1
sumProbability n_i m numPropositions
  | numPropositions == 1 = probability totalBalls n_i m
  | otherwise =
    probability totalBalls n_i m
      + sumProbability n_i m (numPropositions - 1)
      - sumProbability (n_i + ballsPerColor) m (numPropositions - 1)

main :: IO ()
main = do
  let probabilityChooseOneOfEach = 1 - (sumProbability ballsPerColor numToDraw numColors)
  print probabilityChooseOneOfEach
