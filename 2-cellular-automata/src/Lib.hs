{-|
    Module      : Lib
    Description : Checkpoint voor V2DeP: cellulaire automata
    Copyright   : (c) Brian van der Bijl & Nick Roumimper, 2020
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum gaan we aan de slag met 1D cellulaire automata.
    Een cellulair automaton is een rekensysteem dat, gegeven een startsituatie en regels,
    in tijdstappen bepaalt of diens cellen (vakjes) "aan" of "uit" zijn ("levend" of "dood"). 
    Denk aan Conway's Game of Life [<https://playgameoflife.com/>], maar dan in één dimensie (op een lijn).
    Als we de tijdstappen verticaal onder elkaar plotten, krijgen we piramides met soms verrassend complexe patronen erin.
    LET OP: lees sowieso de informatie op [<https://mathworld.wolfram.com/Rule30.html>] en de aangesloten pagina's!
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib where
import Data.Maybe (catMaybes) -- Niet gebruikt, maar deze kan van pas komen...
import Data.List (unfoldr)
import Data.Tuple (swap)

-- ..:: Sectie 1: Basisoperaties op de FocusList ::..

-- Om de state van een cellulair automaton bij te houden bouwen we eerst een set functies rond een `FocusList` type. 
-- Dit type representeert een (1-dimensionale) lijst, met een enkel element dat "in focus" is. 
-- Het is hierdoor mogelijk snel en makkelijk een enkele cel en de cellen eromheen te bereiken.
-- Een voorbeeld van zo'n "gefocuste" lijst: 
--     [0, 1, 2, <3>, 4, 5]
-- waarbij 3 het getal is dat in focus is.
-- In Haskell representeren we de bovenstaande lijst als:
--     FocusList [3,4,5] [2,1,0]
-- waarbij het eerste element van de eerste lijst in focus is.
-- De elementen die normaal vóór de focus staan, staan in omgekeerde volgorde in de tweede lijst.
-- Hierdoor kunnen we makkelijk (lees: zonder veel iteraties te hoeven doen) bij de elementen rondom de focus,
-- en de focus makkelijk één plaats opschuiven.

data FocusList a = FocusList { forward :: [a]
                             , backward :: [a]
                             }
  deriving (Show, Eq)

-- De instance-declaraties mag je voor nu negeren.
instance Functor FocusList where
  fmap = mapFocusList

-- Enkele voorbeelden om je functies mee te testen:
intVoorbeeld :: FocusList Int
intVoorbeeld = FocusList [3,4,5] [2,1,0]

stringVoorbeeld :: FocusList String
stringVoorbeeld = FocusList ["3","4","5"] ["2","1","0"]


{-| The 'toList function converts a FocusList to a regular list by taking the two sublists of the FocusList,
reversing the second one, and appending them.
it takes 1 argument, of type 'FocusList a'. It returns type '[a]' -}
toList :: FocusList a -> [a]
toList FocusList {forward = x, backward = y} = reverse y  ++  x



{-| The 'fromList' function converts a regular list to a FocusList (the first element of the regular list is the focus)
by taking the regular list as the forward part of the FocusList and adding an empty list for the backward part
it takes 1 argument, of type '[a]'. It returns type 'FocusList a' -}
fromList :: [a] -> FocusList a
fromList x = FocusList x []

-- Deze functie, die je van ons cadeau krijgt, schuift de focus één naar links op.
-- Voorbeeld: goLeft $ FocusList [3,4,5] [2,1,0] ~> FocusList [2,3,4,5] [1,0]
goLeft :: FocusList a -> FocusList a
goLeft (FocusList x (y:ys)) = FocusList (y:x) ys


{-| The 'goRight' function moves the FocusList one spot to the right
by splitting the forward list of the FocusList in a head and tail, and then returning a FocusList with the tail as the
forward list and the head, prepended to the original backward list, as the new backward list
it takes 1 argument, of type 'FocusList a'. It returns type 'FocusList a' -}
goRight :: FocusList a -> FocusList a
goRight (FocusList (x:xs) y) = FocusList xs (x:y)


{-| The 'leftMost' function moves the focus all the way to the left
by reversing the backward part of the focus list, appending that to the forward part, and returning that as the new
forward part, with an empty list as the backward part
 it takes 1 argument, of type 'FocusList a'. It returns type 'FocusList a' -}
leftMost :: FocusList a -> FocusList a
leftMost (FocusList x y) = FocusList ((reverse y) ++ x) []


{-| The 'rightMost' function moves the focus all the way to the right
by taking the last element of the forward list as the new forward list
and the reverse of the init of the original forward list prepended to the original backward list as the new backward list
it takes 1 argument, of type 'FocusList a'. It returns type 'FocusList a' -}
rightMost :: FocusList a -> FocusList a
rightMost (FocusList x y) = FocusList [last x] ((reverse $ init x)++y)


-- Onze functies goLeft en goRight gaan er impliciet van uit dat er links respectievelijk rechts een waarde gedefinieerd is. 
-- De aanroep `goLeft $ fromList [1,2,3]` zal echter crashen, omdat er in een lege lijst gezocht wordt: er is niets verder naar links. 
-- Dit is voor onze toepassing niet handig, omdat we vaak de cellen direct links en rechts van de focus 
-- nodig hebben, ook als die (nog) niet bestaan.

-- Schrijf de functies totalLeft en totalRight die de focus naar links respectievelijk rechts opschuift; 
-- als er links/rechts geen vakje meer is, dan wordt een lege (dode) cel teruggeven. 
-- Hiervoor gebruik je de waarde `mempty`, waar we met een later college nog op in zullen gaan. 
-- Kort gezegd zorgt dit ervoor dat de FocusList ook op andere types blijft werken - 
-- je kan dit testen door totalLeft/totalRight herhaaldelijk op de `voorbeeldString` aan te roepen, 
-- waar een leeg vakje een lege string zal zijn.
-- NOTE: deze functie werkt niet met intVoorbeeld! (Omdat mempty niet bestaat voor Ints - maar daar komen we nog op terug!)

-- Grafisch voorbeeld: [⟨░⟩, ▓, ▓, ▓, ▓, ░]  ⤚totalLeft→ [⟨░⟩, ░, ▓, ▓, ▓, ▓, ░]


{-| The 'totalLeft' moves the focus list to the left, if there are no more cells to the left, add one
if the backward part of the FocusList is empty, then it prepends an empty cell to the forward part of the new FocusList
it takes 1 argument, of type 'FocusList a'. It returns type 'FocusList a' -}
totalLeft :: (Eq a, Monoid a) => FocusList a -> FocusList a
totalLeft (FocusList x []) = FocusList (mempty:x) []
totalLeft x = goLeft x



{-| The 'totalRight' moves the focus list to the right, if there are no more cells to the right, add one
if the forward part of the FocusList is empty, then it prepends an empty cell to the backward part of the new FocusList
 it takes 1 argument, of type 'FocusList a'. It returns type 'FocusList a' -}
totalRight :: (Eq a, Monoid a) => FocusList a -> FocusList a
totalRight (FocusList (x:xs:xss) y) = FocusList (xs:xss) (x:y)
totalRight (FocusList (x:xs) y) = FocusList [mempty] (x:y)



--Alternative:
--totalRight (FocusList (x:xs) y) = if (length xs) == 0 then FocusList [mempty] (x:y) else FocusList xs (x:y)

-- ..:: Sectie 2 - Hogere-ordefuncties voor de FocusList ::..

-- In de colleges hebben we kennis gemaakt met een aantal hogere-orde functies zoals `map`, `zipWith` en `fold[r/l]`. 
-- Hier stellen we equivalente functies voor de FocusList op.

-- Deze werkt zoals je zou verwachten: de functie wordt op ieder element toegepast, voor, op en na de focus.
-- Je mag hier gewoon map voor gebruiken.

{-| The 'mapFocusList' function applies a function of choice on every value of the a FocusList
by using the map function on the forward and backwards parts of the FocusList
it takes 2 arguments, of types '(a -> b)', 'FocusList a'. It returns type 'FocusList b' -}
mapFocusList :: (a -> b) -> FocusList a -> FocusList b
mapFocusList f (FocusList x y) = FocusList (map f x) (map f y)

-- Deze functie zorgt ervoor dat ieder paar elementen uit de FocusLists als volgt met elkaar gecombineerd wordt:

-- [1, 2, ⟨3⟩,  4, 5]        invoer 1
-- [  -1, ⟨1⟩, -1, 1, -1]    invoer 2
--------------------------- (*)
-- [  -2, ⟨3⟩, -4, 5    ]    resultaat

-- of, in code: zipFocusListWith (*) (FocusList [3,4,5] [2,1]) (FocusList [1,-1,1,-1] [-1]) ~> FocusList [3,-4,5] [-2]
-- Oftewel: de meegegeven functie wordt aangeroepen op de twee focus-elementen, met als resultaat het nieuwe focus-element. 
-- Daarnaast wordt de functie paarsgewijs naar links/rechts doorgevoerd, waarbij gestopt wordt zodra een van beide uiteinden leeg is. 
-- Dit laatste is net als bij de gewone zipWith, die je hier ook voor mag gebruiken.


{-| The 'zipFocusListWith' function uses a function of choice to combine two FocusLists
by using the zipWith function on the forward and backward parts of both FocusList and returning those results as a new Focuslist
it takes 3 arguments, of types '(a -> b -> c)', 'FocusList a', 'FocusList b'. It returns type 'FocusList c' -}
zipFocusListWith :: (a -> b -> c) -> FocusList a -> FocusList b -> FocusList c
zipFocusListWith f (FocusList x y) (FocusList q z) = FocusList (zipWith f x q) (zipWith f y z)

-- Het folden van een FocusList vergt de meeste toelichting: waar we met een normale lijst met een left fold en een
-- right fold te maken hebben, folden we hier vanuit de focus naar buiten.
-- Vanuit de focus worden de elementen van rechts steeds gecombineerd tot een nieuw element, 
-- vanuit het element voor de focus gebeurt hetzelfde vanuit links. 
-- De twee resultaten van beide sublijsten (begin tot aan focus, focus tot en met eind) worden vervolgens nog een keer met de meegegeven functie gecombineerd. 
-- Hieronder een paar voorbeelden:

-- foldFocusList (*) [0, 1, 2, ⟨3⟩, 4, 5] = (0 * (1 * 2)) * ((3 * 4) * 5)
--                                       = (0 * 2) * (12 * 5)
--                                       = 0 * 60
--                                       = 0

-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = (0 - (1 - 2)) - ((3 - 4) - 5)
--                                       = (0 - (-1)) - ((-1) - 5)
--                                       = 1 - (-6)
--                                       = 7

-- Je kunt, buiten de testsuite, `testFold` uitvoeren in "stack ghci" om je functie te testen.
-- Let op: de tweede lijst van de FocusList kan leeg zijn! (De eerste technisch gezien ook, maar dan heb je geen geldige FocusList.)


{-| The foldFocusList function does what the foldl1 function does but applies it to FocusLists
if one of the FocusList sublists is empty it only folds the other, otherwise it applies the function of choice to the result
of the folds of the forward and backword sublists
it takes 2 arguments, of type '(a -> a -> a)', 'FocusList a'. It returns type 'a' -}
foldFocusList :: (a -> a -> a) -> FocusList a -> a
foldFocusList f (FocusList x []) = (foldl1 f x)
foldFocusList f (FocusList [] y) = (foldl1 f y)
foldFocusList f (FocusList x y) = f (foldl1 f y) (foldl1 f x)

-- Testfunctie voor foldFocusList (geeft True als alles klopt, False als er één of meer niet kloppen)
testFold :: Bool
testFold = and [ foldFocusList (+) intVoorbeeld     == 15
               , foldFocusList (-) intVoorbeeld     == 7
               , foldFocusList (++) stringVoorbeeld == "012345"
               ]


-- ..:: Sectie 2.5: Types voor cellulaire automata ::..

-- Nu we een redelijk complete FocusList hebben, kunnen we deze gaan gebruiken om cellulaire automata in te ontwikkelen.
-- In deze sectie hoef je niets aan te passen, maar je moet deze wel even doornemen voor de volgende opgaven.

-- Een cel kan ofwel levend, ofwel dood zijn.
data Cell = Alive | Dead deriving (Show, Eq)

-- De onderstaande instance-declaraties mag je in dit practicum negeren.
instance Semigroup Cell where
  Dead <> x = x
  Alive <> x = Alive

instance Monoid Cell where
  mempty = Dead

-- De huidige status van ons cellulair automaton beschrijven we als een FocusList van Cells.
-- Dit type noemen we Automaton.
type Automaton = FocusList Cell

-- De standaard starttoestand bestaat uit één levende cel in focus.
start :: Automaton
start = FocusList [Alive] []

-- De context van een cel is de waarde van een cel, samen met de linker- en rechterwaarde, op volgorde.
-- Voorbeeld: de context van 4 in [1,2,3,4,5,6] is [3,4,5].
-- In de praktijk bestaat de context altijd uit drie cellen, maar dat zie je niet terug in dit type.
type Context = [Cell]

-- Een regel is een mapping van elke mogelijke context naar de "volgende state" - levend of dood.
-- Omdat er 2^3 = 8 mogelijke contexts zijn, zijn er 2^8 = 256 geldige regels.
-- Zie voor een voorbeeld [<https://mathworld.wolfram.com/Rule30.html>].
type Rule = Context -> Cell


-- ..:: Sectie 3: Rule30 en helperfuncties ::..

-- als de lijst leeg is, wordt een meegegeven defaultwaarde teruggegeven.

{-| The 'safeHead' function returns the first value of a list, if the list is empty is returns a default value instead.
 it takes 2 arguments, of types 'a', '[a]'. It returns type 'a' -}
safeHead :: a        -- ^ Defaultwaarde
         -> [a]      -- ^ Bronlijst
         -> a
safeHead x []  = x
safeHead x y = head y

-- Als de lijst lang genoeg is werkt de functie hetzelfde als `take` en worden de eerste `n` elementen teruggegeven.
-- Zo niet, dan worden zoveel mogelijk elementen teruggegeven, en wordt daarna tot aangevuld met de meegegeven defaultwaarde.
-- Voorbeelden: takeAtLeast 4 0 [1,2,3,4,5] ~> [1,2,3,4]
--              takeAtLeast 4 0 [1,2]       ~> [1,2,0,0]

{-| The 'takeAtLeast' function returns the first x elements of a list as a list, if the length of the list is smaller than x
then it appends x - list.length default values.
we do this by adding together two lists, one for the x values of the list, and one, using the replicate function, for
the x - list.length default values
it takes 3 arguments, of types 'Int', 'a', '[a]'. It returns type '[a]' -}
takeAtLeast :: Int   -- ^ Aantal items om te pakken
            -> a     -- ^ Defaultwaarde
            -> [a]   -- ^ Bronlijst
            -> [a]
takeAtLeast x y z = (take x z) ++ replicate (x - length z) y

-- Niet-gedefinieerde cellen zijn per definitie Dead.

{-| The 'context' function returns the context of a focus-cel in an Automaton
by using the takeAtLeast function on both parts of the FocusList and appending them
it takes 1 argument, of type 'Automaton'. It returns type 'Context' -}
context :: Automaton -> Context
context (FocusList x y) = takeAtLeast 1 Dead y ++ takeAtLeast 2 Dead x


-- We doen voor deze simulatie de aanname dat de "known universe" iedere ronde met 1 uitbreidt naar zowel links als rechts.

{-| The 'expand' function returns an Automaton expanded by dead cells at both sides
by appending a Dead cell list of length one to both parts of the FocusList
it takes 1 argument, of type 'Automaton'. It returns type 'Automaton' -}
expand :: Automaton -> Automaton
expand (FocusList x y) = FocusList (x++[Dead]) (y++[Dead])

-- Voor bonuspunten: doe dit in zo min mogelijk regels code. De underscore _ is je vriend.


{-| The 'rule30' function returns a cell depending on the list of cells input based on rule30
 it takes 1 argument, of type '[Cell]'. It returns type 'Cell' -}
rule30 :: [Cell] -> Cell
rule30 [Alive, Dead, Dead] = Alive
rule30 [Alive, _, _] = Dead
rule30 [Dead, Dead, Dead] = Dead
rule30 [Dead, _, _] = Alive


-- Je kan je rule-30 functie in GHCi (voer `stack ghci` uit) testen met het volgende commando:
-- putStrLn . showPyramid . iterateRule rule30 15 $ start
-- (Lees sectie 3.5 voor uitleg over deze functies - en hoe het afdrukken nou precies werkt!)

-- De verwachte uitvoer is dan:
{-             ▓
              ▓▓▓
             ▓▓░░▓
            ▓▓░▓▓▓▓
           ▓▓░░▓░░░▓
          ▓▓░▓▓▓▓░▓▓▓
         ▓▓░░▓░░░░▓░░▓
        ▓▓░▓▓▓▓░░▓▓▓▓▓▓
       ▓▓░░▓░░░▓▓▓░░░░░▓
      ▓▓░▓▓▓▓░▓▓░░▓░░░▓▓▓
     ▓▓░░▓░░░░▓░▓▓▓▓░▓▓░░▓
    ▓▓░▓▓▓▓░░▓▓░▓░░░░▓░▓▓▓▓
   ▓▓░░▓░░░▓▓▓░░▓▓░░▓▓░▓░░░▓
  ▓▓░▓▓▓▓░▓▓░░▓▓▓░▓▓▓░░▓▓░▓▓▓
 ▓▓░░▓░░░░▓░▓▓▓░░░▓░░▓▓▓░░▓░░▓
▓▓░▓▓▓▓░░▓▓░▓░░▓░▓▓▓▓▓░░▓▓▓▓▓▓▓ -}


-- ..:: Sectie 3.5: Het herhaaldelijk uitvoeren en tonen van een cellulair automaton ::..
-- In deze sectie hoef je niets aan te passen, maar je moet deze wel even doornemen voor de volgende opgaven.

-- Een reeks van Automaton-states achtereen noemen we een TimeSeries. Effectief dus gewoon een lijst van Automatons over tijd.
type TimeSeries = [Automaton]

-- De functie iterateRule voert een regel n keer uit, gegeven een starttoestand (Automaton). 
-- Het resultaat is een reeks van Automaton-states.
iterateRule :: Rule          -- ^ The rule to apply
            -> Int           -- ^ How many times to apply the rule
            -> Automaton     -- ^ The initial state
            -> TimeSeries
iterateRule r 0 s = [s]
iterateRule r n s = s : iterateRule r (pred n) (fromList $ applyRule $ leftMost $ expand s)
  where applyRule :: Automaton -> Context
        applyRule (FocusList [] bw) = []
        applyRule z = r (context z) : applyRule (goRight z)

-- De functie showPyramid zet een reeks van Automaton-states om in een String die kan worden afgedrukt.
showPyramid :: TimeSeries -> String
showPyramid zs = unlines $ zipWith showFocusList zs $ reverse [0..div (pred w) 2]
  where w = length $ toList $ last zs :: Int
        showFocusList :: Automaton -> Int -> String
        showFocusList z p = replicate p ' ' <> concatMap showCell (toList z)
        showCell :: Cell -> String
        showCell Dead  = "░"
        showCell Alive = "▓"


-- ..:: Sectie 4: Cellulaire automata voor alle mogelijke regels ::..

-- Er bestaan 256 regels, die we niet allemaal met de hand gaan uitprogrammeren op bovenstaande manier. 
-- Zoals op de voorgenoemde pagina te zien is, heeft het nummer te maken met binaire codering. 
-- De toestand van een cel hangt af van de toestand van 3 cellen in de vorige ronde: de cel zelf en diens beide buren (de context). 
-- Afhankelijk van het nummer dat een regel heeft, mapt iedere combinatie naar een levende of dode cel.

-- Je mag dit met de hand uitschrijven, maar voor meer punten kun je ook een lijst-comprehensie of andere slimme functie verzinnen.

{-| The 'inputs' function returns a list of all possible contexts by using a list comprehension
 it takes 0 arguments. It returns type [Context] -}
inputs :: [Context]
inputs = [[x,y,z] | x <- [Alive,Dead], y <- [Alive,Dead], z <- [Alive,Dead]]

-- Deze helperfunctie evalueert met de functie (p) de waarde (x); als dit True teruggeeft, is het resultaat Just x, anders Nothing. 
guard :: (a -> Bool) -> a -> Maybe a
guard p x | p x = Just x
          | otherwise = Nothing

-- TODO: Voorzie de functie `binary` van uitgebreid commentaar.
-- Leg in dit commentaar uit: 
-- 1) Wat de functie precies doet;
-- 2) Stap voor stap, hoe de functie dat doel bereikt.
-- Tips: - Zoek de definitie van `unfoldr` op met Hoogle. 
--       - `toEnum` converteert een Int naar een ander type, in dit geval 0 -> False en 1 -> True voor Bool.

{-| The 'binary' function converts an Int to a list of 8 bools representing a number in the binary system
the first line creates a list of 8 zeroes and casts it to bools
it takes 1 argument, of type 'Int'. It returns type '[Bool]' -}
binary :: Int -> [Bool]
binary = map toEnum . reverse . take 8 . (++ repeat 0)
       . unfoldr (guard (/= (0,0)) . swap . flip divMod 2)

-- die (qua positie) overeenkomen met een True.
-- Je kunt hiervoor zipWith en Maybe gebruiken (check `catMaybes` in Data.Maybe) of de recursie met de hand uitvoeren.

{-| The 'mask' function compares compares a list of booleans to a list of other values, and only returns the other values with the same
index as True in the boolean list. We do this by using recursion to loop through the lists, and then prepending the values to a new list
if x = True.
it takes 2 arguments, of types '[Bool]', '[a]'. It returns type '[a]' -}
mask :: [Bool] -> [a] -> [a]
mask (x:xs) (y:ys) = if x then y: mask xs ys else mask xs ys
mask _ _ = []


-- De Int staat hierbij voor het nummer van de regel; de Context `input` is waarnaar je kijkt om te zien of het resultaat
-- met de gevraagde regel Dead or Alive is. 
-- Tips: - Denk eraan dat het type Rule een shorthand is voor een functie-type, dus dat je met 2 argumenten te maken hebt. 
--       - Definieer met `where` een subset van `inputs` die tot een levende danwel dode cel leiden.

{-| The 'rule' function returns the new state of the input based on any valid rule n
we use mask to get a list of inputs based on rule n, that result in Alive
we then check if the input is an element of that list
if it is: we return Alive
if it isn't: we return Dead
it takes 1 argument, of type 'Int'. It returns type 'Rule' -}
rule :: Int -> Rule
rule n input
        | input `elem` mask (binary n) inputs = Alive
        | otherwise = Dead

{- Je kunt je rule-functie in GHCi testen met variaties op het volgende commando:

   putStrLn . showPyramid . iterateRule (rule 18) 15 $ start

                  ▓
                 ▓░▓
                ▓░░░▓
               ▓░▓░▓░▓
              ▓░░░░░░░▓
             ▓░▓░░░░░▓░▓
            ▓░░░▓░░░▓░░░▓
           ▓░▓░▓░▓░▓░▓░▓░▓
          ▓░░░░░░░░░░░░░░░▓
         ▓░▓░░░░░░░░░░░░░▓░▓
        ▓░░░▓░░░░░░░░░░░▓░░░▓
       ▓░▓░▓░▓░░░░░░░░░▓░▓░▓░▓
      ▓░░░░░░░▓░░░░░░░▓░░░░░░░▓
     ▓░▓░░░░░▓░▓░░░░░▓░▓░░░░░▓░▓
    ▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓
   ▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓

   Als het goed is zal `stack run` nu ook werken met de optie (d) uit het menu;
   experimenteer met verschillende parameters en zie of dit werkt.
-}



