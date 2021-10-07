{-|
    Module      : Lib
    Description : Checkpoint voor V2DEP: decision trees
    Copyright   : (c) Nick Roumimper, 2021
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum schrijven we een algoritme om de decision tree te bouwen voor een gegeven dataset.
    Meer informatie over het principe achter decision trees is te vinden in de stof van zowel DEP als CM.
    Let op dat we hier naar een simpele implementatie van decision trees toewerken; parameters zoals maximale
    diepte laten we hier expres weg. Deze code blijft splitsen tot er geen verbetering meer mogelijk is.
    De resulterende boom zal dus "overfit" zijn, maar dat is de verwachting.
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib where

import Data.List (group, sort,sortBy, nub, elemIndex )
import Data.Tuple
import Data.Function (on)




data CRecord = CRecord { properties :: [Float]
                       , label :: String
                       }
  deriving (Show, Eq)


type CDataset = [CRecord]


miniSet1 :: CDataset
miniSet1 = [CRecord [1,1] "blue", CRecord [2,2] "green", CRecord [3,3] "pink", CRecord [4,4] "purple", CRecord [5,5] "gray"]

miniSet2 :: CDataset
miniSet2 = [CRecord [1,1] "pink", CRecord [2,2] "pink", CRecord [3,3] "purple", CRecord [4,4] "blue", CRecord [5,5] "blue"]

miniSet3 :: CDataset
miniSet3 = [CRecord [1,1] "blue", CRecord [1,2] "green", CRecord [2,1] "green", CRecord [2,2] "green", CRecord [3,1] "orange", CRecord [3,2] "orange"] 



{-| The 'getLabels' function returns a list of all labels in the CDataset,
by recursively looping through the list and prepending the labels to a new list.
it takes 1 argument, of type 'CDataset'. It returns type '[String]' -}
getLabels :: CDataset -> [String]
getLabels [] = []
getLabels ((CRecord x y):i) = y: getLabels i


{-| The 'countLabels' function returns a list of tuples with a label and count of said label in given CDataset,
by creating a sorted list of lists of unique labels, example: [["a","a","a"],["b"],["c","c"]]
then applying a map with an anonymous function on it that takes the head and length of the nested lists:
[("a",3),("b",1),("c",2)].
it takes 1 argument, of type 'CDataset'. It returns type '[(String, Int)]' -}
countLabels :: CDataset -> [(String, Int)]
countLabels x = map (\y -> (head y, length y)) (group(sort(getLabels x)))


{-| The 'mostFrequentLabel' function returns the label that appears most often in the CDataset,
by swapping the label and label_count for every tuple in the list, sorting that, and then taking the last label
it takes 1 argument, of type 'CDataset'. It returns type 'String' -}
mostFrequentLabel :: CDataset -> String
mostFrequentLabel x = snd (last (sort (map swap (countLabels x))))


fd :: Int -> Int -> Float
fd x y = (/) (fromIntegral x) (fromIntegral y)

{-| The 'gini' function returns the gini impurity of a dataset,
by putting all the label counts in a list, dividing every label count in that list by the length of the dataset,
multiplying every divided label count by itself, and returning 1 - the sum of those
it takes 1 argument, of type 'CDataset'. It returns type 'Float' -}
gini :: CDataset -> Float
gini x = 1- (sum (map (**2) (map (\y -> fd (snd(y)) (length x)) (countLabels x))))

{-| The 'giniAfterSplit' function returns the combined gini impurity of two datasets,
by multiplying the gini impurity of the first data set with (the length of the first data set divided by the length of both datasets combined),
multiplying the gini impurity of the second data set with (the length of the second data set divided by the length of both datasets combined)
and adding those two together
it takes 2 arguments, of types 'CDataset', 'CDataset'. It returns type 'Float' -}
giniAfterSplit :: CDataset -> CDataset -> Float
giniAfterSplit x y  = (gini x * fd (length x) (length x + length y)) + (gini y * fd (length y) (length x + length y))

data CSplit = CSplit { feature :: Int
                     , value :: Float   
                     }
    deriving (Show, Eq, Ord)


{-| The 'getFeature' function returns a list of values of a specific feature,
by creating a list filled with property sublists from the dataset, and then using the !! operator to create a list
of every xth index of the sublists.
it takes 2 arguments, of types 'Int', 'CDataset'. It returns type '[Float]' -}
getFeature :: Int -> CDataset -> [Float]
getFeature x y  = map (!!x) (map properties y)



{-| The 'getUniqueValuesSorted' function returns a list of floats with unique values, sorted,
by using the nub function to delete duplicate values and the sort function to sort the list
it takes 1 argument, of type '[Float]'. It returns type '[Float]' -}
getUniqueValuesSorted :: [Float] -> [Float]
getUniqueValuesSorted x = sort (nub x)
-- you could also use group, and then take the first elements, but this exists so why not use it


{-| The 'getAverageValues' function returns a list of the means of all consecutive pairs of values in a list,
by using recursion to loop through the list and prepending the means of every pair of values to a new list
it takes 1 argument, of type '[Float]'. It returns type '[Float]' -}
getAverageValues :: [Float] -> [Float]
getAverageValues [x] = []
getAverageValues (x:xs:xss) = (x+xs)/2: getAverageValues (xs:xss)

{-| The 'getFeatureSplits' function returns all possible CSplits for a specific feature for a specific database
by iterating through a list of sorted getAverageFeatures and adding the features and averageValues together
to create a list of CSplits
it takes 2 argument, of types 'Int', 'CDataset'. It returns type '[CSplit]' -}
getFeatureSplits :: Int -> CDataset -> [CSplit]
getFeatureSplits x y = map (\i -> CSplit (x) (i)) (getAverageValues (getUniqueValuesSorted (getFeature x y)))


{-| The 'getAllFeatureSplits' function returns all possible CSplits of a specific dataset
by iterating through a list of [0.. (length properties)-1] calling the getFeatureSplits function on every element
in the list and the CDataset and putting those in a list
it takes 1 argument, of type 'CDataset'. It returns type '[CSplit]' -}
getAllFeatureSplits :: CDataset -> [CSplit]
getAllFeatureSplits x = concat (map (\i -> getFeatureSplits i x)[0..(length (properties (head x))-1)])


{-| The 'splitSingleRecord' function returns True or False by comparing a CSplit to a CRecord
by checking if the xth index of the CRecord properties <= the value of the CSplit
it takes 2 argument, of types 'CSplit', 'CRecord'. It returns type 'bool' -}
splitSingleRecord :: CSplit -> CRecord -> Bool
splitSingleRecord (CSplit x y) (CRecord i _)
                | i!!x <= y = True
                | otherwise = False


{-| The 'splitOnFeature' function splits a dataset in two
by using the filter function in combination with the (not) splitSingleRecord function to decide
which CRecords should be copied to the new datasets
it takes 2 argument, of types 'CDataset','CSplit'. It returns type '(CDataset, CDataset)' -}
splitOnFeature :: CDataset -> CSplit -> (CDataset, CDataset)
splitOnFeature x y =  (filter (splitSingleRecord y) x, filter (not . splitSingleRecord y) x)


{-| The 'generateAllSplits' function creates a list of all possible splits combined in a tuple with the split datasets
by using an anonymous function to iterate through getAllFeatureSplits of the dataset
and then creating a list of tuples of: (the split, the first half of the split dataset, the second half of the split dataset)
it takes 1 argument, of type 'CDataset'. It returns type '[(CSplit, CDataset, CDataset)]' -}
generateAllSplits :: CDataset -> [(CSplit, CDataset, CDataset)]
generateAllSplits x = (map (\i -> (i, fst (splitOnFeature x i) ,snd (splitOnFeature x i))) (getAllFeatureSplits x))

{-| The 'assignGini' function assigns a gini value to every element in [(CSplit, CDataset, CDataset)] it only returns a list of
the gini values and the CSplits
by using pattern matching to get all elements of a 3-element-long tuple and using recursion to calculate the gini impurity
of every tuple
it takes 1 argument, of type '[(CSplit, CDataset, CDataset)]'. It returns type '[(Float, CSplit)]' -}
assignGini :: [(CSplit, CDataset, CDataset)] ->  [(Float,CSplit)]
assignGini [] = []
assignGini ((x,xs,xss):y) = (giniAfterSplit xs xss,x) : assignGini y


{-| The 'returnLowestGini' function returns the lowest gini impurity CSplit combination
by using the sortBy function to sort the list of tuples on the fst element (gini impurity) and returning the first tuple
it takes 1 argument, of type '[(Float,CSplit)]'. It returns type '(Float, CSplit)' -}
returnLowestGini :: [(Float,CSplit)] -> (Float,CSplit)
returnLowestGini x =  head (sortBy (compare `on` fst) x)

{-| The 'findBestSplit' function returns the gini impurity and CSplit of the split of the dataset with the lowest gini impurity
by using the assignGini and returnLowestGini functions
it takes 1 argument, of type 'CDataset'. It returns type '(Float, CSplit)' -}
findBestSplit :: CDataset -> (Float, CSplit)
findBestSplit x = returnLowestGini(assignGini (generateAllSplits x))


data DTree = Branch CSplit DTree DTree | Leaf String deriving (Show, Eq, Ord)

-
{-| The 'buildDecisionTree' function crates a decision tree based on a specific dataset
by using recursion and checking for 2 conditions:
1. is the gini impurity of the dataset 0 OR is the gini impurity of the dataset smaller than the gini impurity of the best split of the dataset
2. otherwise
if 1 add a leaf to the tree consisting of the most frequent label in the dataset,
if 2 add a branch to the tree consisting of the best split, and the dataset split into two, on the best split, recursively called until condition 1 has been met
it takes 1 argument, of type 'CDataset'. It returns type 'DTree' -}
buildDecisionTree :: CDataset -> DTree
buildDecisionTree x
                  | gini x == 0 || gini x <= (fst (findBestSplit x)) = Leaf (mostFrequentLabel x)
                  | otherwise = Branch f (buildDecisionTree(fst s)) (buildDecisionTree(snd s))
                  where s = splitOnFeature x f
                        f = snd(findBestSplit x)


{-| The 'predict' function returns a predicted label  based on a specific decision tree and a specific list of properties
by using recursing and checking for 2 conditions:
1. is the decision tree just a leaf? return the label of the leaf
2. is the decision tree a branch?
   2a. is the branch splittable with the properties i and CSplit x? return the first sub-branch (or leaf) of the checked branch
   2b. otherwise? return the second sub-branch (or leaf) of the checked branch
it takes 2 arguments, of type 'DTree', '[Float]'. It returns type String -}
predict :: DTree -> [Float] -> String
predict (Leaf x) _ = x
predict (Branch x y z) i
                       | splitSingleRecord x (CRecord i "") = predict y i
                       | otherwise = predict z i


