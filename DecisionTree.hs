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
    weightedPurity
) where

import Data.List
import Data.Function

type Category = String
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

--Working as intended
getAttribute :: Instance -> Attribute -> Bool
getAttribute (Instance _ list) att = snd . head . filter ((==) att . fst) $ list

--Working
getCategory :: Instance -> Category
getCategory (Instance cat _) = cat

--Working as intended
computePurity :: [Instance] -> [Category] -> Float
computePurity instances categories = foldl (*) 1 . map (\l -> l/instLength) $ categoryCounts
    where categoryCounts = map (fromIntegral . length) . map (\cat -> filter (\a -> getCategory a == cat) instances) $ categories
          instLength = fromIntegral . length $ instances

--Working
baselineAccuracy :: [Instance] -> (Category, Float)
baselineAccuracy instances = fmap (\a -> a / (fromIntegral (length instances))) . head . sortBy ((flip compare) `on` snd) . map (\l@((Instance x _):_) -> (x, fromIntegral (length l))) . groupBy ((==) `on` getCategory) . sortBy (compare `on` getCategory) $ instances

--Working
divideLength :: [a] -> [a] -> Float
divideLength = (/) `on` (fromIntegral . length)

--Working
trueInstances :: [Instance] -> Attribute -> [Instance]
trueInstances instances attribute = filter (flip getAttribute attribute) instances

--Working
falseInstances :: [Instance] -> Attribute -> [Instance]
falseInstances instances attribute = filter (not . flip getAttribute attribute) instances

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

    Parsing stuff, that wont be needed in the module....

--Working
parseDecisionTree :: String -> ([Category], [Attribute], [Instance])
parseDecisionTree s = (words categoryLine, attributes, map (flip parseInstance attributes) instances)
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
    (categories, attributes, instances) <- parseDecisionTree <$> readFile trainingFile
    (_, _, testInst) <- parseDecisionTree <$> readFile testFile
    let tree = buildTree instances attributes categories
        classifications = map (classify tree) testInst
        (_, baseline) = baselineAccuracy instances 
        correctList = map (\((Instance cl1 _), cl2) -> if cl1 == cl2 then 1 else 0) $ zip testInst classifications 
        percentCorrect = sum correctList / (fromIntegral (length correctList))
    putStrLn $ show tree
    putStrLn $ "Baseline Accuracy is: "++(show baseline)
    putStrLn $ "The percentage that were classified correctly is: "++(show percentCorrect)
    mapM_ (\(inst, cl) -> putStrLn ("Test Instance: "++(show inst)++" classified as "++cl)) $ zip testInst classifications
-}   