{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Text.Read
import Data.Maybe (fromMaybe)
import Control.Exception
import Lib

data Colour = Reset | Red | Yellow | Green | Grey

instance Show Colour where
  show Reset = "\ESC[0m"
  show Red = "\ESC[31m"
  show Yellow = "\ESC[33m"
  show Green = "\ESC[32m"
  show Grey = "\ESC[1;30m"

colour :: Colour -> String -> String
colour c = (show c <>) . (<> show Reset)

successString :: String -> String
successString n = colour Green $ "Cool! Functie " ++ n ++ " werkt!"

incorrectString :: Show a => String -> a -> a -> String
incorrectString n r e = (colour Yellow $ "\nHet uitvoeren van " ++ n ++ " werkt nog niet...\n")
  <> "Je antwoord was " <> (colour Red $ show r)
  <> ", maar dit moest " <> (colour Green $ show e) <> " zijn.\n"

errorString :: String -> String
errorString n = colour Red $ "De functie " ++ n ++ " werkt nog niet, je error was:"

testFunc :: forall a. (Eq a, Show a) => a -> a -> String -> String -> IO ()
testFunc given expected funcname operationname = do
  res <- try (evaluate $ given) :: IO (Either SomeException a)
  case res of
    Left error -> do putStrLn $ errorString funcname
                     putStrLn $ colour Grey $ displayException error
                     putStrLn $ colour Reset ""
    Right value -> if value == expected then putStrLn $ successString funcname
                                        else putStrLn $ incorrectString operationname value expected

data ThreeCase x a b c = ThreeLeft x a | ThreeMiddle x b | ThreeRight x c

singleTest :: forall a. (Eq a, Show a) => (a, a, String, String) -> IO (ThreeCase String SomeException (a, a) a)
singleTest (given, expected, funcname, operationname) = do
  res <- try (evaluate $ given) :: IO (Either SomeException a)
  case res of
    Left error -> do return (ThreeLeft funcname error)
    Right value -> if value == expected then return (ThreeRight funcname value)
                                        else return (ThreeMiddle operationname (value, expected))

lowestCase :: IO (ThreeCase x a b c) -> IO (ThreeCase x a b c) -> IO (ThreeCase x a b c)
lowestCase x y = do
  first <- x
  second <- y
  case (first, second) of
    (x@(ThreeLeft _ _), _)   -> return x
    (_, y@(ThreeLeft _ _))   -> return y
    (x@(ThreeMiddle _ _), _) -> return x
    (_, y@(ThreeMiddle _ _)) -> return y
    otherwise                -> return first

multiTest :: forall a. (Eq a, Show a) => [(a, a, String, String)] -> IO()
multiTest l = do
  result <- foldl1 lowestCase $ map singleTest l
  case result of
    ThreeLeft s x -> do putStrLn $ errorString s
                        putStrLn $ colour Grey $ displayException x
                        putStrLn $ colour Reset ""
    ThreeMiddle s (x, y) -> putStrLn $ incorrectString s x y
    ThreeRight s x -> putStrLn $ successString s

-- Enkele waarden om de functies mee te testen:
tFLInt1 :: FocusList Int
tFLInt1 = FocusList [1,3,5,7,9] []

tFLInt2 :: FocusList Int
tFLInt2 = FocusList [13] [11,7,5,3,2]

tFLInt3 :: FocusList Int
tFLInt3 = FocusList [1,5,9,2] [4,1,3]

tLInt1 :: [Int]
tLInt1 = [0,1,2,3,4,5]

tLInt2 :: [Int]
tLInt2 = [1,3,5,7,9]

tLInt3 :: [Int]
tLInt3 = [2,3,5,7,11,13]

tFLString1 :: FocusList String
tFLString1 = FocusList ["1","3","5","7","9"] []

tFLString2 :: FocusList String
tFLString2 = FocusList ["13"] ["11","7","5","3","2"]

tFLString3 :: FocusList String
tFLString3 = FocusList ["1","5","9","2"] ["4","1","3"]

tFLString4 :: FocusList String
tFLString4 = FocusList [""] []

tFLNone :: FocusList a
tFLNone = FocusList [] []

tFLAut1 :: Automaton
tFLAut1 = FocusList [Alive, Alive] [Alive]

tFLAut2 :: Automaton
tFLAut2 = FocusList [Alive] []

tFLAut3 :: Automaton
tFLAut3 = FocusList [] []

-- rLIF, "really Long Inputs Function", is een implementatie van inputs om mee te testen,
-- maar omdat je pluspunten kunt verdienen met een mooie implementatie, is die hier zo lang (en lelijk!) mogelijk.
rLIF :: [Context]
rLIF = [[Alive, Alive, Alive], [Alive, Alive, Dead], [Alive, Dead, Alive], [Alive, Dead, Dead], [Dead, Alive, Alive], [Dead, Alive, Dead], [Dead, Dead, Alive], [Dead, Dead, Dead]]

-- quickBin, "quickBinary", is een aparte functie die even snel een regel omzet naar een getal.
-- Ik raad je aan geen onderdelen van deze functie te gebruiken - hij is quick and dirty, alleen voor testdoeleinden.
quickBin :: Rule -> Int
quickBin x = foldl1 (+) $ zipWith (*) [128,64,32,16,8,4,2,1] $ map ((\y -> if y == Alive then 1 else 0) . x) rLIF

-- ftst, "full test"
ftst :: IO ()
ftst = do multiTest [((toList intVoorbeeld), [0,1,2,3,4,5], "toList", "toList intVoorbeeld"), 
                     ((toList tFLInt1), [1,3,5,7,9], "toList", "toList $ FocusList [1,3,5,7,9] []"),
                     ((toList tFLInt2), [2,3,5,7,11,13], "toList", "toList $ FocusList [13] [11,7,5,3,2]")]
          multiTest [((fromList tLInt1), FocusList [0,1,2,3,4,5] [], "fromList", "fromList [0,1,2,3,4,5]"),
                     ((fromList tLInt2), FocusList [1,3,5,7,9] [], "fromList", "fromList [1,3,5,7,9]"),
                     ((fromList tLInt3), FocusList [2,3,5,7,11,13] [], "fromList", "fromList [2,3,5,7,11,13]")]
          multiTest [((goRight intVoorbeeld), FocusList [4,5] [3,2,1,0], "goRight", "goRight intVoorbeeld"),
                     ((goRight tFLInt1), FocusList [3,5,7,9] [1], "goRight", "goRight $ FocusList [1,3,5,7,9] []"),
                     ((goRight tFLInt3), FocusList [5,9,2] [1,4,1,3], "goRight", "goRight $ FocusList [1,5,9,2] [4,1,3]")]
          multiTest [((leftMost intVoorbeeld), FocusList [0,1,2,3,4,5] [], "leftMost", "leftMost intVoorbeeld"),
                     ((leftMost tFLInt2), FocusList [2,3,5,7,11,13] [], "leftMost", "leftMost $ FocusList [13] [11,7,5,3,2]"),
                     ((leftMost tFLInt3), FocusList [3,1,4,1,5,9,2] [], "leftMost", "leftMost $ FocusList [1,5,9,2] [4,1,3]")]
          multiTest [((rightMost intVoorbeeld), FocusList [5] [4,3,2,1,0], "rightMost", "rightMost intVoorbeeld"),
                     ((rightMost tFLInt1), FocusList [9] [7,5,3,1], "rightMost", "rightMost $ FocusList [1,3,5,7,9] []"),
                     ((rightMost tFLInt3), FocusList [2] [9,5,1,4,1,3], "rightMost", "rightMost $ FocusList [1,5,9,2] [4,1,3]")]
          multiTest [((totalLeft stringVoorbeeld), FocusList ["2","3","4","5"] ["1","0"], "totalLeft", "totalLeft stringVoorbeeld"),
                     ((totalLeft tFLString1), FocusList ["","1","3","5","7","9"] [], "totalLeft", "totalLeft $ FocusList [\"1\",\"3\",\"5\",\"7\",\"9\"] []"),
                     ((totalLeft tFLString4), FocusList ["",""] [], "totalLeft", "totalLeft $ FocusList [\"\"] []")]
          multiTest [((totalRight stringVoorbeeld), FocusList ["4","5"] ["3","2","1","0"], "totalRight", "totalRight stringVoorbeeld"),
                     ((totalRight tFLString2), FocusList [""] ["13","11","7","5","3","2"], "totalRight", "totalRight $ FocusList [\"13\"] [\"11\",\"7\",\"5\",\"3\",\"2\"]"),
                     ((totalRight tFLString4), FocusList [""] [""], "totalRight", "totalRight $ FocusList [\"\"] []")]
          multiTest [((mapFocusList (* 2) intVoorbeeld), FocusList [6,8,10] [4,2,0], "mapFocusList", "mapFocusList (* 2) intVoorbeeld"),
                     ((mapFocusList (+ 1) tFLInt3), FocusList [2,6,10,3] [5,2,4], "mapFocusList", "mapFocusList (+ 1) $ FocusList [1,5,9,2] [4,1,3]"),
                     ((mapFocusList (* 100) tFLNone), FocusList [] [], "mapFocusList", "mapFocusList (* 100) $ FocusList [] []")]
          multiTest [((zipFocusListWith (*) intVoorbeeld tFLInt3), FocusList [3,20,45] [8,1,0], "zipFocusListWith", "zipFocusListWith (*) intVoorbeeld $ FocusList [1,5,9,2] [4,1,3]"),
                     ((zipFocusListWith (-) tFLInt2 tFLInt3), FocusList [12] [7,6,2], "zipFocusListWith", "zipFocusListWith (-) (FocusList [13] [11,7,5,3,2]) $ FocusList [1,5,9,2] [4,1,3]"),
                     ((zipFocusListWith (+) tFLNone intVoorbeeld), FocusList [] [], "zipFocusListWith", "zipFocusListWith (+) (FocusList [] []) intVoorbeeld")]
          multiTest [((foldFocusList (+) intVoorbeeld), 15, "foldFocusList", "foldFocusList (+) intVoorbeeld"),
                     ((foldFocusList (-) intVoorbeeld), 7, "foldFocusList", "foldFocusList (-) intVoorbeeld"),
                     ((foldFocusList (*) tFLInt1), 945, "foldFocusList", "foldFocusList (*) $ FocusList [1,3,5,7,9] []")]
          multiTest [((safeHead 0 tLInt3), 2, "safeHead", "safeHead 0 [2,3,5,7,11,13]"),
                     ((safeHead 1 []), 1, "safeHead", "safeHead 1 []")]
          multiTest [((takeAtLeast 3 0 tLInt3), [2,3,5], "takeAtLeast", "takeAtLeast 3 0 [2,3,5,7,11,13]"),
                     ((takeAtLeast 4 1 [2,3]), [2,3,1,1], "takeAtLeast", "takeAtLeast 4 1 [2,3]"),
                     ((takeAtLeast 5 2 []), [2,2,2,2,2], "takeAtLeast", "takeAtLeast 5 2 []")]
          multiTest [((context tFLAut1), [Alive, Alive, Alive], "context", "context $ FocusList [Alive, Alive] [Alive]"), 
                     ((context tFLAut2), [Dead, Alive, Dead], "context", "context $ FocusList [Alive] []"),
                     ((context tFLAut3), [Dead, Dead, Dead], "context", "context $ FocusList [] []")]
          multiTest [((expand tFLAut1), FocusList [Alive, Alive, Dead] [Alive, Dead], "expand", "expand $ FocusList [Alive, Alive] [Alive]"), -- 05 expand
                     ((expand tFLAut2), FocusList [Alive, Dead] [Dead], "expand", "expand  $ FocusList [Alive] []"),
                     ((expand tFLAut3), FocusList [Dead] [Dead], "expand", "expand $ FocusList [] []")]
          multiTest [(quickBin rule30, 30, "rule30", "rule30 omzetten naar een integer")]
          multiTest [((and (map (\x -> elem x inputs) rLIF) && and (map (\x -> elem x rLIF) inputs)), True, "inputs", "een check of alle nodige waarden in inputs zitten")]
          multiTest [((Lib.mask [True, False, True, True, False] tLInt2), [1,5,7], "mask", "mask [True, False, True, True, False] [1,3,5,7,9]"),
                     ((Lib.mask [True, True] tLInt1), [0,1], "mask", "mask [True, True] [0,1,2,3,4,5]"),
                     ((Lib.mask [True, False, True, True, False] []), [], "mask", "mask [True, False, True, True, False] []")]
          multiTest [((quickBin $ rule 94), 94, "rule", "rule 94 meegeven en het resultaat weer omzetten naar een integer"),
                     ((quickBin $ rule 182), 182, "rule", "rule 182 meegeven en het resultaat weer omzetten naar een integer"),
                     ((quickBin $ rule 220), 220, "rule", "rule 220 meegeven en het resultaat weer omzetten naar een integer")]

          -- Future improvements:
            -- only write the function name once

draw :: IO ()
draw = do putStrLn "Geef het getal van de gewenste rule? [18]"
          nr <- fromMaybe 18 . readMaybe <$> getLine
          putStrLn "Geef het gewenste aantal iteraties? [15]"
          n <- fromMaybe 15 . readMaybe <$> getLine
          putStrLn $ showPyramid $ iterateRule (rule nr) n start

main :: IO ()
main = do putStrLn "Wil je je code laten testen (t), een piramide tekenen (d) of stoppen (q)?"
          choice <- getLine
          case choice of
            "t"       -> do ftst
                            main
            "d"       -> do draw
                            main
            "q"       -> return ()
            otherwise -> main