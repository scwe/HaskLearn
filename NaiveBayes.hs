module NaiveBayes(
    Feature,
    Class,
    Instance (Instance),
    TrainingSet,
    gaussianValue,
    mean,
    variance,
    standardDeviation,
    getFeature,
    classProbability,
    classify,
    normalizeProb
    ) where
    
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Function

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

getClass :: Instance -> Class
getClass (Instance c _) = c

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

classProbability :: Class -> TrainingSet -> Double
classProbability c set = (numLength dat) / (numLength set)
    where dat = filter (isClass c) $ set

getClasses :: TrainingSet -> [Class]
getClasses = Set.toList . foldr Set.insert Set.empty . map getClass

getFeatures :: Instance -> [Feature]
getFeatures (Instance _ feat) = feat

normalize :: (Floating a) => [a] -> [a]
normalize xs = map (* factor) xs
    where total = sum xs 
          factor = 1 / total

normalizeProb :: [(Class, Double)] -> [(Class, Double)]
normalizeProb probs = zip (map fst probs) . normalize . map snd $ probs

classify :: TrainingSet -> Instance -> [(Class, Double)]  --We return a mapping from class to probablity
classify set (Instance _ features) = List.sortBy (\(_, v1) (_, v2) -> compare v2 v1) . map (\cl -> (cl, posteriorNumerator cl)) $ classes
    where classes = getClasses set
          posteriorNumerator cl = (*) (classProbability cl set) . foldr (*) 1 . foldr (\x acc -> featureProbability x cl : acc) [] . zip [0..] $ features
          featureProbability (index, featureValue) cl = conditionProbabilityDensity set featureValue index cl 

