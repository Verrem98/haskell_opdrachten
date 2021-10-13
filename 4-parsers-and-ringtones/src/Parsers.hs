module Parsers where

import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Control.Applicative
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad(mzero, mplus)
import Data.List (uncons)
import Types
import Instruments

type Parser = (StateT String Maybe)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

-- TODO: Schrijf en documenteer de Parser `pComplementCharSet` die een lijst van karakters meekrijgt,
-- en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit.


{-| The 'pComplementCharSet' function parses the first character of a list if it isn't part of a given set of characters
by doing the opposite of 'pCharSet'
it takes 1 argument, of type 'CharSet'. It returns type 'Parser Char' -}
pComplementCharSet :: CharSet -> Parser Char
pComplementCharSet cs = do input <- uncons <$> get
                           case input of
                                        Just (h, t) -> if h `elem` cs then mzero else put t >> return h
                                        Nothing -> mzero


{-| The 'pString' function tries to parse a given String
by recursively looping through the string and parsing the characters one by one
then recombining the characters into a string and returning it
it takes 1 argument, of type 'String'. It returns type 'Parser String' -}
pString :: String -> Parser String
pString [] = return []
pString (x:xs) = do input <- pCharSet [x]
                    remainder <- pString xs
                    return (input : remainder)

pOptional :: Parser a -> Parser (Maybe a)
pOptional p = Just <$> p <|> return Nothing 

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

pEmpty :: Parser ()
pEmpty = return ()

{-| The 'pRepeat' function repeatedly applies a single parser
by using the pRepeatSepBy and pEmpty functions
It returns type 'Parser [a]' -}
pRepeat :: Parser a -> Parser [a]
pRepeat = pRepeatSepBy pEmpty


{-| The 'pNumber' function parses an even number
by repeatedly using the pCharSet function and casting the result to a Parser Int
It returns type 'Parser Int' -}
pNumber :: Parser Int
pNumber = read <$> pRepeat(pCharSet "0123456789")

pTone :: Parser Tone
pTone = do tone <- tRead . toUpper <$> pCharSet "abcdefg"
           sharp <- pOptional (pCharSet "#")
           if isJust sharp && tone `elem` [C,D,F,G,A]
             then return (succ tone)
             else return tone
  where tRead 'C' = C
        tRead 'D' = D
        tRead 'E' = E
        tRead 'F' = F
        tRead 'G' = G
        tRead 'A' = A
        tRead 'B' = B
        tRead _   = error "Invalid note"


{-| The 'pOctave' function parses a number to an octave
by pattern matching the number to find the octave representation of that number
It returns type 'Parser Octave' -}
pOctave :: Parser Octave
pOctave = do input <- tRead <$> pCharSet "012345678"
             return input
  where tRead '0' = Zero
        tRead '1' = One
        tRead '2' = Two
        tRead '3' = Three
        tRead '4' = Four
        tRead '5' = Five
        tRead '6' = Six
        tRead '7' = Seven
        tRead '8' = Eight
        tRead _   = error "Invalid number"

pDuration :: Parser Duration
pDuration = do number <- pNumber
               case number of
                 1 -> return Full
                 2 -> return Half
                 4 -> return Quarter
                 8 -> return Eighth
                 16 -> return Sixteenth
                 32 -> return Thirtysecond
                 _ -> mzero

pPause :: Duration -> Parser Note
pPause d = do duration <- fromMaybe d <$> pOptional pDuration
              _ <- pCharSet "pP"
              return $ Pause duration

pNote :: Duration -> Octave -> Parser Note
pNote d o = do duration <- fromMaybe d <$> pOptional pDuration
               tone <- pTone
               dot <- pOptional (pCharSet ".")
               octave <- fromMaybe o <$> pOptional pOctave
               return $ Note tone octave (if isJust dot then Dotted duration else duration)

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

-- TODO: Schrijf en documenteer de Parser `pHeader`, die de start van de RTTL-string parset.
-- TIPS: We hebben je de naam van het bestand, en het converteren van bpm met fromIntegral vast gegeven.
--       Het stuk dat je rest om te parsen zit tussen de twee dubbele punten!
pHeader :: Parser (String, Duration, Octave, Beats)
pHeader = do name <- pRepeat (pComplementCharSet ":")
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             -- Your code here!
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             return (name, Full, Zero, fromIntegral 0) -- Pas deze regel ook aan; maak van 0 de waarde van bpm!

pSeparator :: Parser ()
pSeparator = () <$ foldl1 mplus [pString " ", pString ", ", pString ","]

pRTTL :: Parser (String, [Note], Beats)
pRTTL = do (t, d, o, b) <- pHeader
           notes <- pRepeatSepBy pSeparator $ mplus (pNote d o) (pPause d)
           return (t, notes, b)

parse :: String -> Maybe (String, [Note], Beats)
parse x = evalStateT pRTTL x