{-|
    Module      : Lib
    Description : Checkpoint voor V2DEP: recursie en lijsten
    Copyright   : (c) Brian van de Bijl, 2020
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum oefenen we met het schrijven van simpele functies in Haskell.
    Specifiek leren we hoe je recursie en pattern matching kunt gebruiken om een functie op te bouwen.
    LET OP: Hoewel al deze functies makkelijker kunnen worden geschreven met hogere-orde functies,
    is het hier nog niet de bedoeling om die te gebruiken.
    Hogere-orde functies behandelen we verderop in het vak; voor alle volgende practica mag je deze
    wel gebruiken.
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- | The 'ex1' function calculates the sum of a list of integers
-- it takes 1 argument, of type '[Int]'. It returns type 'Int'
ex1 :: [Int] -> Int
ex1 (x:xs) = x + ex1 xs
ex1 [] = 0


-- | The 'ex2' function increases every integer in a list of integers by one
-- it takes 1 argument, of type '[Int]'. It returns type '[Int]'
ex2 :: [Int] -> [Int]
ex2 (x:xs) = x+1 : ex2 xs
ex2 [] = []


-- | The 'ex3' function multiplies every integer in a list of integers by minus one
-- it takes 1 argument, of type '[Int]'. It returns type '[Int]'
ex3 :: [Int] -> [Int]
ex3 (x:xs) = -x : ex3 xs
ex3 [] = []


-- | The 'ex4' function concatenates 2 lists of integers
-- it takes 2 arguments, of types '[Int]', '[Int]'. It returns type '[Int]'
ex4 :: [Int] -> [Int] -> [Int]
ex4 x[] = x
ex4 []y = y
ex4 (x:xs) (y:ys) = x: ex4 xs (y:ys)


-- | The 'ex5' function looks at the values of two lists of integers (equal in length), and returns a list made up of the sums of said integers at corresponding indices
-- it takes 2 arguments, of types '[Int]', '[Int]'. It returns type '[Int]'
ex5 :: [Int] -> [Int] -> [Int]
ex5 (x:xs) (y:ys) = x+y: ex5 xs ys
ex5 [][] = []


-- | The 'ex6' function looks at the values of two lists of integers (equal in length), and returns a list made up of the products of said integers at corresponding indices
-- it takes 2 arguments, of types '[Int]', '[Int]'. It returns type '[Int]'
ex6 :: [Int] -> [Int] -> [Int]
ex6 (x:xs) (y:ys) = x*y: ex6 xs ys
ex6 [][] = []


-- | The 'ex7' function calculates the dot product of 2 lists of integers, it uses both the ex1 and ex6 functions to do so
-- it takes 2 arguments, of types '[Int]', '[Int]'. It returns type 'Int'
ex7 :: [Int] -> [Int] -> Int
ex7 x y = ex1 (ex6 x y)
