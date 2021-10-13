{-# LANGUAGE TypeApplications #-}

module Types where

import Data.Int (Int32)

type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)


{-| The 'zipWithL' function zips two lists with a specific function, if the first list has more values than the second list
then append those values to the end of the zipped list
by taking the (length (firstList) - length (secondList)) last values of the first list, and appending those to the
zipped list.
it takes 3 arguments, of types '(a -> b -> a)', '[a]', '[b]'. It returns type '[a]' -}
zipWithL :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithL f x y
             | (length x) > (length y) = zipWith f x y ++ reverse (take ((length x)-(length y)) (reverse x))
             | otherwise = zipWith f x y


{-| The 'zipWithR' function zips two lists with a specific function, if the second list has more values than the first list
then append those values to the end of the zipped list
by taking the (length (secondList) - length (firstList)) last values of the second list, and appending those to the
zipped list.
it takes 3 arguments, of types '(a -> b -> a)', '[a]', '[b]'. It returns type '[a]' -}
zipWithR :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithR f x y
             | (length x) < (length y) = zipWith f x y ++ reverse (take ((length y)-(length x)) (reverse y))
             | otherwise = zipWith f x y

data Sound = FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = (all ((<  0.001) . abs) $ zipWith (-) xs ys) && (length xs == length ys)


instance Semigroup Sound where
  (FloatFrames a) <> (FloatFrames b) = FloatFrames (a <> b)

instance Monoid Sound where
  mempty = FloatFrames (mempty)

{-| The '(<+>)' operator combines two Sounds into a single Sound
we do this by using the zipWithL function if the first Sound is longer
and by using the zipWithR function if the second Sound is longer
if both Sounds are the same length we use the regular zipWith
it takes 2 arguments, of types 'Sound', 'Sound'. It returns type 'Sound' -}
(<+>) :: Sound -> Sound -> Sound
(FloatFrames x) <+> (FloatFrames y)
                                  | length x > length y = FloatFrames (zipWithL (+) x y)
                                  | length x < length y = FloatFrames (zipWithR (+) x y)
                                  | otherwise = FloatFrames (zipWith (+) x y)


floatToInt32 :: Float -> Int32
floatToInt32 x = fromIntegral $ round x

getAsInts :: Sound -> [Int32]
getAsInts (FloatFrames fs) = map (floatToInt32 . \x -> x * fromIntegral (div (maxBound @Int32 ) 2 )) fs

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO: Schrijf en documenteer de functie modifyInstrument, die een Modifier met een Instrument combineert. 
-- TIPS: Kijk goed naar de types! Gebruik een lambda om een functie te maken, die je verpakt in een Instrument.
modifyInstrument :: Instrument -> Modifier -> Instrument
modifyInstrument = undefined

-- TODO: Schrijf en documenteer de functie arrange die de functie in het meegegeven Instrument toepast op de frequentie en duur. 
-- TIPS: Kijk goed naar de types!
arrange :: Instrument -> Hz -> Seconds -> Sound
arrange = undefined