--module NaiveBayes where

type Feature = Double
type Class = String
data Instance = Instance Class [Feature] deriving (Show) 
type TrainingSet = [Instance]

gaussianValue :: Double -> Double -> Double -> Double
gaussianValue mean variance value = factor * exponent
    where factor = 1 / (sqrt(2*pi*variance))
          exponent = exp . negate . (*) dist . (/) dist $ 2*variance
          dist = value - mean

mean :: [Double] -> Double
mean xs = flip (/) len . sum $ xs 
    where len = numLength xs

square :: Double -> Double
square v = v*v

isClass :: Class -> Instance -> Bool
isClass c (Instance cl _) = c == cl

variance :: [Double] -> Double
variance xs = (squaredSum - ((1/n) * sumSquared)) / (n - 1)
    where squaredSum = sum . map square $ xs
          sumSquared = square . sum $ xs
          n = numLength xs

standardDeviation :: [Double] -> Double
standardDeviation = sqrt . variance

getFeature :: Int -> Instance -> Feature
getFeature index (Instance _ features) = features !! index

meanFeature :: TrainingSet -> Int -> Double
meanFeature set index = mean . map (getFeature index) $ set

varFeature :: TrainingSet -> Int -> Double
varFeature set index = variance . map (getFeature index) $ set

numLength :: (Num a) => [b] -> a
numLength = fromIntegral . length

--This is the same as 
-- p(feature | class)
--So it finds the condition probablity given the class and the actual value
--
conditionProbabilityDensity :: TrainingSet -> Double -> Int -> Class -> Double
conditionProbabilityDensity set testValue index c = gaussianValue (meanFeature dat index) (varFeature dat index) testValue 
    where dat = filter (isClass c) $ set

probablityClass :: TrainingSet -> Class -> Double
probablityClass set c = (numLength dat) / (numLength set)
    where dat = filter (isClass c) $ set

getClasses :: TrainingSet -> [Class]
getClasses set = []

classify :: TrainingSet -> Instance -> [(Class, Double)]  --We return a mapping from class to probablity
classify set inst = []

trSet :: TrainingSet
trSet = [Instance "male"    [6,    180, 12],
         Instance "male"    [5.92, 190, 11],
         Instance "male"    [5.58, 170, 12],
         Instance "male"    [5.92, 165, 10],
         Instance "female"  [5,    100, 6],
         Instance "female"  [5.5,  150, 8],
         Instance "female"  [5.42, 130, 7],
         Instance "female"  [5.75, 150, 9]]

