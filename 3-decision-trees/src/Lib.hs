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

import Data.List (group, sort)

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
-- TODO: schrijf de functie getLabels die een lijst van alle labels in een dataset teruggeeft.
getLabels :: CDataset -> [String]
getLabels = undefined

-- Om de Gini impurity te bepalen, willen we weten hoe vaak alle labels voorkomen.
-- Voorbeeld: ["a", "b", "a", "c", "c", "a"] wordt [("a", 3), ("b", 1), ("c", 2)]
-- We hebben de volgende twee hulpfuncties geïmporteerd:
--     group :: Eq a => [a] -> [[a]]
--        ^ zet alle gelijke waarden naast elkaar in een eigen lijst.
--          Voorbeeld: [1,1,2,2,2,1] => [[1,1],[2,2,2],[1]] 
--     sort :: Ord a => [a] -> [a]
--        ^ sorteert de lijst.
-- TODO: schrijf de functie countLabels die telt hoe vaak alle labels in de dataset voorkomen.
countLabels :: CDataset -> [(String, Int)]
countLabels = undefined

-- Voor toekomstig gebruik willen we alvast een functie hebben die het meest voorkomende label
-- van een dataset geeft. Bij gelijkspel mag je eender welk teruggeven.
-- TODO: schrijf de functie mostFrequentLabel op basis van countLabels.
-- HINT: gebruik een functie uit de Prelude. We gebruiken mostFrequentLabel pas in de laatste sectie!
mostFrequentLabel :: CDataset -> String
mostFrequentLabel = undefined

-- We definiëren de volgende hulpfunctie (fd, voor "Float Division") om twee Ints te delen als twee Floats.
-- Voorbeeld: fd 3 4 ~> 0.75 (een Float), i.p.v. 0 (een Int).
fd :: Int -> Int -> Float
fd x y = (/) (fromIntegral x) (fromIntegral y)

-- De Gini impurity van één dataset is:
--     de som van de kansen voor elke klasse dat
--         ik, uit alle rijen, willekeurig een rij uit die klasse trek én
--         ik, uit alle rijen, willekeurig een rij uit een andere klasse trek.
-- Zie ook de stof van CM en de Canvas van DEP voor meer context.
-- TODO: schrijf de functie gini die de Gini impurity van één dataset bepaalt.
gini :: CDataset -> Float
gini = undefined

-- De gecombineerde Gini impurity van twee datasets (in ons geval: na een splitsing)
-- is de gewogen som van de Gini impurity van beide sets.
-- Voorbeeld: mijn splitsing leidt tot
--     1) een dataset met 3 rijen en een Gini impurity van 0.2;
--     2) een dataset met 2 rijen en een Gini impurity van 0.1.
-- Dan is de gecombineerde Gini impurity (0.2 * (3/5)) + (0.1 * (2/5)) = 0.16.
-- TODO: schrijf de functie giniAfterSplit die de gecombineerde Gini impurity van twee datasets bepaalt.
giniAfterSplit :: CDataset -> CDataset -> Float
giniAfterSplit = undefined


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

-- Allereerst willen we alle waarden van een bepaalde feature in een aparte lijst hebben.
-- TODO: schrijf de functie getFeature, die gegeven een feature (index in de lijst properties) en een dataset,
--       een lijst teruggeeft van alle waarden van die feature.
getFeature :: Int -> CDataset -> [Float]
getFeature = undefined

-- Als we een lijst van waarden hebben, hoeven we alleen naar de unieke waarden te kijken.
-- Tegelijkertijd is het wel zo makkelijk als de unieke waarden alvast zijn gesorteerd.
-- TODO: schrijf de functie getUniqueValuesSorted, die uit een lijst van Floats de unieke waarden gesorteerd teruggeeft.
-- HINT: gebruik de hulpfuncties uit de vorige sectie.
getUniqueValuesSorted :: [Float] -> [Float]
getUniqueValuesSorted = undefined

-- Als we de dataset splitsen, doen we er verstandig aan om niet precies op een waarde uit de dataset te splitsen.
-- In plaats daarvan splitsen we op het gemiddelde van alle twee naast elkaar gelegen waarden.
-- Voorbeeld: getAverageValues [2.0, 3.0, 5.0, 9.0] ~> [2.5, 4.0, 7.0]
-- Voor de traindata maakt dat geen verschil, maar voor het voorspellen van nieuwe waarden wel.
-- TODO: schrijf de functie getAverageValues, die de gemiddelden bepaalt van alle paren van twee waarden in een lijst.
getAverageValues :: [Float] -> [Float]
getAverageValues = undefined

-- Met deze functies kunnen we alle mogelijke CSplits bepalen voor één gegeven feature.
-- Voorbeeld: een dataset met in feature 2 de waarden [9.0, 2.0, 5.0, 3.0] wordt
--            [CSplit 2 2.5, CSplit 2 4.0, CSplit 7.0].
-- TODO: schrijf de functie getFeatureSplits, die alle mogelijke CSplits bepaalt voor een gegeven feature.
getFeatureSplits :: Int -> CDataset -> [CSplit]
getFeatureSplits = undefined

-- Door getFeatureSplits toe te passen voor alle mogelijke features, kunnen we alle mogelijke CSplits bepalen.
-- TODO: schrijf de functie getAllFeatureSplits, die alle mogelijke CSplits van een dataset bepaalt.
getAllFeatureSplits :: CDataset -> [CSplit]
getAllFeatureSplits = undefined


-- ..:: Sectie 3 - Het vinden van de beste splitsing ::..
-- Nu we alle splitsingen hebben gegenereerd, rest ons nog de taak de best mogelijke te vinden.
-- Hiervoor moeten we eerst de functies schrijven om één CDataset, op basis van een CSplit,
-- te splitsen in twee CDatasets.

-- Allereerst schrijven we de functie waarmee we bepalen in welke dataset een CRecord belandt
-- gegeven een bepaalde splitsing. Deze functie moet True teruggeven als de waarde van de feature  
-- kleiner-gelijk-aan is aan de splitswaarde, en False als deze groter is dan de splitswaarde.
-- Voorbeelden: gegeven CSplit 1 3.0 en CRecord [4.0, 2.0, 9.0] "x", is het resultaat True.
--              gegeven CSplit 1 1.0 en CRecord [4.0, 2.0, 9.0] "x", is het resultaat False.
-- TODO: schrijf de functie splitSingleRecord, die voor een enkel CRecord True of False teruggeeft.
splitSingleRecord :: CSplit -> CRecord -> Bool
splitSingleRecord = undefined

-- Nu kunnen we de functie schrijven die één dataset opsplitst in twee, op basis van een CSplit object.
-- TODO: schrijf de functie splitOnFeature, die één dataset opsplitst in twee.
-- HINT: gebruik een functie uit de Prelude. Onthoud dat CDataset = [CRecord]!
splitOnFeature :: CDataset -> CSplit -> (CDataset, CDataset)
splitOnFeature = undefined

-- Nu kunnen we:
--     1) alle splitsingen genereren voor een CDataset, met behulp van Sectie 2;
--     2) de datasets die resulteren bij elk van die splitsingen genereren.
-- Wel is het van belang dat we onthouden welke splitsing bij welke twee datasets hoort.
-- TODO: schrijf de functie generateAllSplits, die voor een gegeven dataset alle mogelijke splitsingen "uitprobeert".
generateAllSplits :: CDataset -> [(CSplit, CDataset, CDataset)]
generateAllSplits = undefined

-- De laatste stap van deze sectie combineert Sectie 1 en Sectie 3:
--     1) Genereer alle mogelijke splits;
--     2) Bepaal welke van deze splitsingen het beste resultaat geeft - oftewel, de laagste Gini impurity.
-- Hierbij willen we graag zowel de Gini impurity als de splitsing zelf onthouden.
-- TODO: schrijf de functie findBestSplit, die voor een dataset de best mogelijke splitsing vindt.
-- HINT: gebruik een functie uit de Prelude. Hoe werkt "kleiner dan" voor tupels?
findBestSplit :: CDataset -> (Float, CSplit)
findBestSplit = undefined


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
-- TODO: schrijf de functie buildDecisionTree.
buildDecisionTree :: CDataset -> DTree
buildDecisionTree = undefined

-- Tot slot, bij het voorspellen weten we alleen de eigenschappen ([Float]), niet het label.
-- TODO: schrijf de functie predict, die op basis van een boom en de gegeven eigenschappen het label voorspelt.
predict :: DTree -> [Float] -> String
predict = undefined

