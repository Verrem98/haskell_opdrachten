{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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

successString :: Int -> String
successString n = colour Green $ "Cool! Opgave " <> show n <> " werkt!\n"

incorrectString :: Show a => Int -> a -> a -> String
incorrectString n r e = (colour Yellow $ "Opgave " <> show n <> " werkt nog niet...\n")
  <> "Je antwoord was " <> (colour Red $ show r)
  <> ", maar dit moest " <> (colour Green $ show e) <> " zijn.\n"

errorString :: Int -> String
errorString n = colour Red $ "Opgave " <> show n <> " werkt nog niet, je error was:"

testFunc :: forall a. (Eq a, Show a) => a -> a -> Int -> IO ()
testFunc given expected number = do
  res <- try (evaluate given) :: IO (Either SomeException a)
  case res of
    Left error -> do putStrLn $ errorString number
                     putStrLn $ colour Grey $ displayException error
                     putStrLn $ colour Reset ""
    Right value -> if value == expected then putStrLn $ successString number
                                        else putStrLn $ incorrectString number value expected

main :: IO ()
main = do testFunc (ex1 [1..5]) 15 1
          testFunc (ex2 [1..5]) [2..6] 2
          testFunc (ex3 [1..5]) [-1,-2,-3,-4,-5] 3
          testFunc (ex4 [1..5] [6..9]) [1..9] 4
          testFunc (ex5 [1..3] [4..6]) [5,7,9] 5
          testFunc (ex6 [1..3] [4..6]) [4,10,18] 6
          testFunc (ex7 [1..3] [4..6]) 32 7
