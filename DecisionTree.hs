{-
    Haskell Implementation of the decision Tree
    Copyright (C) 2014  Scott Weston

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module DecisionTree(
    Category,
    Instance (Instance),
    Attribute,
    DecisionTree (),
    classify,
    buildTree,
    baselineAccuracy,
    getAttribute,
    getCategory,
    weightedPurity,
    parseDecisionTreeFile
) where

import Data.List
import Data.Function

--A category is just the name of the category as a String
type Category = String
--An Attribute is 
type Attribute = String

data Instance = Instance Category [(Attribute, Bool)] deriving (Show)
data DecisionTree = Empty | Node Attribute (Bool, DecisionTree) (Bool, DecisionTree) | Leaf Category Float

decisionTreeShow :: String -> DecisionTree -> String
decisionTreeShow indent Empty = ""
decisionTreeShow indent (Leaf cat prob) = indent ++ "Class "++cat++", prob "++(show prob)++"\n"
decisionTreeShow indent (Node att (b1, leftTree) (b2, rightTree)) = (showSubTree leftTree b1)++(showSubTree rightTree b2)
    where showSubTree tr bo = indent++att++" = "++(show bo)++": \n"++(decisionTreeShow (indent++"   ") tr)

instance Show DecisionTree where
    show = decisionTreeShow ""

--Gets the value of an attribute from a supplied instance
getAttribute :: Instance -> Attribute -> Bool
getAttribute (Instance _ list) att = snd . head . filter ((==) att . fst) $ list

--Gets the Category of the instance, this is just a helper method
getCategory :: Instance -> Category
getCategory (Instance cat _) = cat

--Computes the purity of a collection of instances based on the 
computePurity :: [Instance] -> [Category] -> Float
computePurity instances categories = foldl (*) 1 . map (\l -> l/instLength) $ categoryCounts
    where categoryCounts = map (fromIntegral . length) . map (\cat -> filter (\a -> getCategory a == cat) instances) $ categories
          instLength = fromIntegral . length $ instances

--Working
baselineAccuracy :: [Instance] -> (Category, Float)
baselineAccuracy instances = fmap (\a -> a / (fromIntegral (length instances))) . head . sortBy ((flip compare) `on` snd) . map (\l@((Instance x _):_) -> (x, fromIntegral (length l))) . groupBy ((==) `on` getCategory) . sortBy (compare `on` getCategory) $ instances

--Divides the length of two lists
divideLength :: [a] -> [a] -> Float
divideLength = (/) `on` (fromIntegral . length)

--Given a list of instances and the attribute that we are looking for it returns all the instances
--that have that attribute as true
trueInstances :: [Instance] -> Attribute -> [Instance]
trueInstances instances attribute = filter (flip getAttribute attribute) instances

--Given a list of instances and the attribute that we are looking for it returns all the instances 
--that have that attribute as false
falseInstances :: [Instance] -> Attribute -> [Instance]
falseInstances instances attribute = filter (not . flip getAttribute attribute) instances

--Computes the weighted purite of a collection of attributes
weightedPurity :: [Instance] -> Attribute -> [Category] -> Float
weightedPurity instances attribute categories = weightedPurityTrue + weightedPurityFalse
    where instTrue = trueInstances instances attribute     --working
          instFalse = falseInstances instances attribute          --working
          purityTrue = computePurity instTrue categories 
          purityFalse = computePurity instFalse categories
          weightedPurityTrue = purityTrue * divideLength instTrue instances
          weightedPurityFalse = purityFalse * divideLength instFalse instances

buildTreeInternal :: [Instance] -> [Attribute] -> [Instance] -> [Category] -> DecisionTree
buildTreeInternal instances attributes allInst categories
    | null instances = let (cat, prob) = baselineAccuracy allInst in Leaf cat prob
    | isPure instances = Leaf (getCategory (head instances)) 1
    | null attributes = let (cat, prob) = baselineAccuracy instances in Leaf cat prob
    | otherwise = let (_, bestAtt, bestInstTrue, bestInstFalse) = foldl getNext (100, "", [], []) attributes in 
            Node bestAtt 
                (True, (buildTreeInternal bestInstTrue (attributes \\ [bestAtt]) allInst) categories) 
                (False, (buildTreeInternal bestInstFalse (attributes \\ [bestAtt]) allInst) categories)
    where isPure = (==1) . length . groupBy ((==) `on` getCategory)  --working
          getNext acc@(purity,_,_,_) attribute = let wp = weightedPurity instances attribute categories in if wp < purity then (wp, attribute, instTrue attribute, instFalse attribute) else acc
          instTrue attribute = filter (flip getAttribute attribute) instances   --working 
          instFalse attribute = filter (not . flip getAttribute attribute) instances        --working

buildTree :: [Instance] -> [Attribute] -> [Category] -> DecisionTree
buildTree instances attributes categories = buildTreeInternal instances attributes instances categories

classify :: DecisionTree -> Instance -> Category 
classify Empty _ = error "You cannot classify with an empty tree"
classify (Leaf cat _) _ = cat
classify (Node att (_, leftTree) (_, rightTree)) inst = if (getAttribute inst att) then classify leftTree inst else classify rightTree inst 
-- we can assume that the left tree always is the true and the right is always the false

{-

This is code that can be used to parse a decision tree and create one, however this will only work if the files
for both the test and training instances follow the patter

category1 category2 ... categoryN
attribute1 attribute2 ... attributeM
categoryValue attributeValue1 attributeValue2 ... attributeValueM
  .
  .
  .

Where each instance has its own line  

-}
--Working
parseDecisionTreeFile :: String -> ([Category], [Attribute], [Instance])
parseDecisionTreeFile s = (words categoryLine, attributes, map (flip parseInstance attributes) instances)
    where (categoryLine:attributeLine:instances) = lines s
          attributes = words attributeLine 

--Working
parseBool :: String -> Bool
parseBool s = if s == "true" then True else False  --we are just going to treat everything else as a False

--Working
parseInstance :: String -> [Attribute] -> Instance
parseInstance s attributes = Instance cat $ zip attributes . map parseBool $ atts
    where (cat:atts) = words s

main :: IO ()
main = do 
    (trainingFile:testFile:_) <- getArgs
    (categories, attributes, instances) <- parseDecisionTreeFile <$> readFile trainingFile
    (_, _, testInst) <- parseDecisionTreeFile <$> readFile testFile
    let tree = buildTree instances attributes categories
        classifications = map (classify tree) testInst
        (_, baseline) = baselineAccuracy instances 
        correctList = map (\((Instance cl1 _), cl2) -> if cl1 == cl2 then 1 else 0) $ zip testInst classifications 
        percentCorrect = sum correctList / (fromIntegral (length correctList))
    putStrLn $ show tree
    putStrLn $ "Baseline Accuracy is: "++(show baseline)
    putStrLn $ "The percentage that were classified correctly is: "++(show percentCorrect)
    mapM_ (\(inst, cl) -> putStrLn ("Test Instance: "++(show inst)++" classified as "++cl)) $ zip testInst classifications
  