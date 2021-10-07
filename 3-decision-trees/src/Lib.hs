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



-- Allereerst definiëren we een datatype voor één enkele rij uit onze traindataset. (CRecord, voor Classification Record.)
-- Een rij uit onze dataset bestaat uit 
--     1) een lijst van numerieke eigenschappen van onze meeteenheid (properties);
--     2) een label voor de klasse waar deze meeteenheid toe behoort (label).
-- Bij het bouwen van onze boom weten we ook het label; ons doel is voor nieuwe data om op basis van de properties
-- te voorspellen welk label erbij hoort - maar dat implementeren we pas helemaal aan het eind.

data CRecord = CRecord { properties :: [Float]
                       , label :: String
                       }
  deriving (Show, Eq)

-- Onze dataset (CDataset, voor Classification Dataset) is dus simpelweg een lijst van CRecords.

type CDataset = [CRecord]

-- Bijgevoegd een paar simpele datasets, die je kunt gebruiken om mee te testen.

miniSet1 :: CDataset
miniSet1 = [CRecord [1,1] "blue", CRecord [2,2] "green", CRecord [3,3] "pink", CRecord [4,4] "purple", CRecord [5,5] "gray"]

miniSet2 :: CDataset
miniSet2 = [CRecord [1,1] "pink", CRecord [2,2] "pink", CRecord [3,3] "purple", CRecord [4,4] "blue", CRecord [5,5] "blue"]

miniSet3 :: CDataset
miniSet3 = [CRecord [1,1] "blue", CRecord [1,2] "green", CRecord [2,1] "green", CRecord [2,2] "green", CRecord [3,1] "orange", CRecord [3,2] "orange"] 


-- ..:: Sectie 1: Het bepalen van de Gini impurity ::..
-- De Gini impurity meet of de dataset rijen bevat uit maar één klasse ("puur"),
--                       of veel rijen uit allerlei verschillende klassen ("impuur").
-- Dit getal zit tussen de 0 en de 1, waar 0 zo puur mogelijk is en 1 zo impuur mogelijk.
-- Als we een splitsing maken in onze boom, willen we de Gini impurity zo laag mogelijk krijgen.

-- Bij het bepalen van de Gini impurity kijken we alleen naar de labels van de data.

{-| The 'getLabels' function returns a list of all labels in the CDataset,
by recursively looping through the list and prepending the labels to a new list.
it takes 1 argument, of type 'CDataset'. It returns type '[String]' -}
getLabels :: CDataset -> [String]
getLabels [] = []
getLabels ((CRecord x y):i) = y: getLabels i

-- Om de Gini impurity te bepalen, willen we weten hoe vaak alle labels voorkomen.
-- Voorbeeld: ["a", "b", "a", "c", "c", "a"] wordt [("a", 3), ("b", 1), ("c", 2)]
-- We hebben de volgende twee hulpfuncties geïmporteerd:
--     group :: Eq a => [a] -> [[a]]
--        ^ zet alle gelijke waarden naast elkaar in een eigen lijst.
--          Voorbeeld: [1,1,2,2,2,1] => [[1,1],[2,2,2],[1]] 
--     sort :: Ord a => [a] -> [a]
--        ^ sorteert de lijst.

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


-- We definiëren de volgende hulpfunctie (fd, voor "Float Division") om twee Ints te delen als twee Floats.
-- Voorbeeld: fd 3 4 ~> 0.75 (een Float), i.p.v. 0 (een Int).
fd :: Int -> Int -> Float
fd x y = (/) (fromIntegral x) (fromIntegral y)

{-| The 'gini' function returns the gini impurity of a dataset,
by putting all the label counts in a list, dividing every label count in that list by the length of the dataset,
multiplying every divided label count by itself, and returning 1 - the sum of those
it takes 1 argument, of type 'CDataset'. It returns type 'Float' -}
gini :: CDataset -> Float
gini x = 1- (sum (map (**2) (map (\y -> fd (snd(y)) (length x)) (countLabels x))))

-- De gecombineerde Gini impurity van twee datasets (in ons geval: na een splitsing)
-- is de gewogen som van de Gini impurity van beide sets.
-- Voorbeeld: mijn splitsing leidt tot
--     1) een dataset met 3 rijen en een Gini impurity van 0.2;
--     2) een dataset met 2 rijen en een Gini impurity van 0.1.
-- Dan is de gecombineerde Gini impurity (0.2 * (3/5)) + (0.1 * (2/5)) = 0.16.

{-| The 'giniAfterSplit' function returns the combined gini impurity of two datasets,
by multiplying the gini impurity of the first data set with (the length of the first data set divided by the length of both datasets combined),
multiplying the gini impurity of the second data set with (the length of the second data set divided by the length of both datasets combined)
and adding those two together
it takes 2 arguments, of types 'CDataset', 'CDataset'. It returns type 'Float' -}
giniAfterSplit :: CDataset -> CDataset -> Float
giniAfterSplit x y  = (gini x * fd (length x) (length x + length y)) + (gini y * fd (length y) (length x + length y))


-- ..:: Sectie 2 - Het genereren van alle mogelijke splitsingen in de dataset ::..
-- Bij het genereren van onze decision tree kiezen we telkens de best mogelijke splitsing in de data.
-- In deze simpele implementatie doen we dat brute force: we genereren alle mogelijke splitsingen
-- in de data, en checken ze allemaal. Hier beginnen we door de splitsingen te genereren.

-- We slaan elke mogelijke splitsing op in het datatype CSplit (voor Classification Split). Deze bestaat uit:
--     1) de eigenschap waarop gesplitst wordt, opgeslagen als de index in de lijst van properties (feature);
--     2) de waarde van deze feature waarop we splitsen - ofwel kleiner-gelijk-aan, ofwel groter dan (value).
-- Let op: feature refereert aan de positie in de lijst van properties van een CRecord.
-- Oftewel: als we het hebben over feature 1 van CRecord [8.0, 5.0, 3.0] "x", bedoelen we 5.0.
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

-- Met deze functies kunnen we alle mogelijke CSplits bepalen voor één gegeven feature.
-- Voorbeeld: een dataset met in feature 2 de waarden [9.0, 2.0, 5.0, 3.0] wordt
--            [CSplit 2 2.5, CSplit 2 4.0, CSplit 7.0].

{-| The 'getFeatureSplits' function returns all possible CSplits for a specific feature for a specific database
by iterating through a list of sorted getAverageFeatures and adding the features and averageValues together
to create a list of CSplits
it takes 2 argument, of types 'Int', 'CDataset'. It returns type '[CSplit]' -}
getFeatureSplits :: Int -> CDataset -> [CSplit]
getFeatureSplits x y = map (\i -> CSplit (x) (i)) (getAverageValues (getUniqueValuesSorted (getFeature x y)))

-- Door getFeatureSplits toe te passen voor alle mogelijke features, kunnen we alle mogelijke CSplits bepalen.

{-| The 'getAllFeatureSplits' function returns all possible CSplits of a specific dataset
by iterating through a list of [0.. (length properties)-1] calling the getFeatureSplits function on every element
in the list and the CDataset and putting those in a list
it takes 1 argument, of type 'CDataset'. It returns type '[CSplit]' -}
getAllFeatureSplits :: CDataset -> [CSplit]
getAllFeatureSplits x = concat (map (\i -> getFeatureSplits i x)[0..(length (properties (head x))-1)])


-- ..:: Sectie 3 - Het vinden van de beste splitsing ::..
-- Nu we alle splitsingen hebben gegenereerd, rest ons nog de taak de best mogelijke te vinden.
-- Hiervoor moeten we eerst de functies schrijven om één CDataset, op basis van een CSplit,
-- te splitsen in twee CDatasets.

-- Allereerst schrijven we de functie waarmee we bepalen in welke dataset een CRecord belandt
-- gegeven een bepaalde splitsing. Deze functie moet True teruggeven als de waarde van de feature  
-- kleiner-gelijk-aan is aan de splitswaarde, en False als deze groter is dan de splitswaarde.
-- Voorbeelden: gegeven CSplit 1 3.0 en CRecord [4.0, 2.0, 9.0] "x", is het resultaat True.
--              gegeven CSplit 1 1.0 en CRecord [4.0, 2.0, 9.0] "x", is het resultaat False.


{-| The 'splitSingleRecord' function returns True or False by comparing a CSplit to a CRecord
by checking if the xth index of the CRecord properties <= the value of the CSplit
it takes 2 argument, of types 'CSplit', 'CRecord'. It returns type 'bool' -}
splitSingleRecord :: CSplit -> CRecord -> Bool
splitSingleRecord (CSplit x y) (CRecord i _)
                | i!!x <= y = True
                | otherwise = False


{-

My old, extremely jank, way of solving splitOnFeature:

boolRecordSplitter :: [(Bool, CRecord)] -> Bool -> [CRecord]
boolRecordSplitter [] _ = []
boolRecordSplitter ((i,j):xs) y
           | i == y = j : boolRecordSplitter xs y
           | otherwise = boolRecordSplitter xs y

splitOnFeature :: CDataset -> CSplit -> (CDataset, CDataset)
splitOnFeature x y = (boolRecordSplitter (zip (map (\i -> (splitSingleRecord y i) ) x) x) True, boolRecordSplitter (zip (map (\i -> (splitSingleRecord y i) ) x) x) False)


-}

-- Nu kunnen we de functie schrijven die één dataset x  opsplitst in twee, op basis van een CSplit object.
-- HINT: gebruik een functie uit de Prelude. Onthoud dat CDataset = [CRecord]!


{-| The 'splitOnFeature' function splits a dataset in two
by using the filter function in combination with the (not) splitSingleRecord function to decide
which CRecords should be copied to the new datasets
it takes 2 argument, of types 'CDataset','CSplit'. It returns type '(CDataset, CDataset)' -}
splitOnFeature :: CDataset -> CSplit -> (CDataset, CDataset)
splitOnFeature x y =  (filter (splitSingleRecord y) x, filter (not . splitSingleRecord y) x)

-- Nu kunnen we:
--     1) alle splitsingen genereren voor een CDataset, met behulp van Sectie 2;
--     2) de datasets die resulteren bij elk van die splitsingen genereren.
-- Wel is het van belang dat we onthouden welke splitsing bij welke twee datasets hoort.


{-| The 'generateAllSplits' function creates a list of all possible splits combined in a tuple with the split datasets
by using an anonymous function to iterate through getAllFeatureSplits of the dataset
and then creating a list of tuples of: (the split, the first half of the split dataset, the second half of the split dataset)
it takes 1 argument, of type 'CDataset'. It returns type '[(CSplit, CDataset, CDataset)]' -}
generateAllSplits :: CDataset -> [(CSplit, CDataset, CDataset)]
generateAllSplits x = (map (\i -> (i, fst (splitOnFeature x i) ,snd (splitOnFeature x i))) (getAllFeatureSplits x))

-- De laatste stap van deze sectie combineert Sectie 1 en Sectie 3:
--     1) Genereer alle mogelijke splits;
--     2) Bepaal welke van deze splitsingen het beste resultaat geeft - oftewel, de laagste Gini impurity.
-- Hierbij willen we graag zowel de Gini impurity als de splitsing zelf onthouden.
-- HINT: gebruik een functie uit de Prelude. Hoe werkt "kleiner dan" voor tupels?

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


-- ..:: Sectie 4 - Genereren van de decision tree en voorspellen ::..
-- In deze laatste sectie combineren we alle voorgaande om de decision tree op te bouwen,
-- en deze te gebruiken voor voorspellingen.

-- We introduceren het datatype van onze boom, de DTree (Decision Tree).
-- In de DTree is sprake van twee opties:
--     1) We hebben een blad van de boom bereikt, waarin we een voorspelling doen van het label (Leaf String);
--     2) We splitsen op een bepaalde eigenschap, met twee sub-bomen voor <= en > (Branch CSplit DTree DTree).
-- Zoals je al ziet is de definitie van Branch CSplit DTree DTree recursief; er kan dus een onbepaald aantal
-- vertakkingen zijn, maar uiteindelijk eindigt elke vertakking in een blad (Leaf).
-- Let op: we onthouden niet de records uit de dataset, maar wel waarop we ze gesplitst hebben (CSplit)!
data DTree = Branch CSplit DTree DTree | Leaf String deriving (Show, Eq, Ord)

-- De logica achter het recursief bouwen van een decision tree is als volgt:
--     ALS de Gini impurity van de dataset 0.0 is (perfect gesplitst)
--     OF de Gini impurity wordt zelfs met de best mogelijke splitsing niet beter
--         DAN geef ik een Leaf terug met daarin het vaakst voorkomende label;
--     ZO NIET,
--         DAN geef ik een Branch terug met daarin de best mogelijke splitsing
--         en de decision trees (sub-bomen) op basis van de twee datasets na die splitsing.

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
                  | otherwise = Branch bs (buildDecisionTree(fst spdat)) (buildDecisionTree(snd spdat))
                  where spdat = splitOnFeature x bs
                        bs = snd(findBestSplit x)


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


