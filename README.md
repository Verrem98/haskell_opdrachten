# dep-2021-2022-practica

Deze repository bevat de practica voor de studenten van Declarative Programming, voor het studiejaar 2021 - 2022. Deze repository wordt mettertijd bijgewerkt en aangevuld met de laatste versie van de andere practica.

## Verplichte en optionele software

De volgende software is verplicht om met deze practica te kunnen starten:

- [Haskell Platform](https://www.haskell.org/platform/) installeert alle onderstaande punten in een keer:
  - [GHC](https://www.haskell.org/ghc/);
  - [Cabal](https://www.haskell.org/cabal/); 
  - [Stack](https://docs.haskellstack.org/en/stable/README/); 
- [Haddock](https://haskell-haddock.readthedocs.io/en/latest/intro.html) voor het compileren van de documentatie.

De volgende software is optioneel, maar kan je helpen bij het ontwikkelen:

- [HLint](https://hackage.haskell.org/package/hlint) geeft suggesties om je code te verbeteren;
- [GHCID](https://github.com/ndmitchell/ghcid) is een GHCI daemon, die je code compileert bij iedere save.

## Werken met/aan de practica

Elk van deze practica is een eigen Stack-project. Om het project te kunnen draaien moet je de voorgenoemde verplichte software installeren.
Vervolgens navigeer je in een elevated command prompt (command prompt, uitgevoerd als administrator) naar de hoofdmap van het practicum (bijv. pad/naar/hier/1-recursion-and-lists) en geef je het commando `stack run`.
Dit commando duurt de eerste keer ontzettend lang; laat maar gewoon draaien, dit blijft niet zo.

Door `stack run` aan te roepen voer je het hoofdprogramma uit, hetgeen ook (soms achter een keuzemenu) testcode bevat. **Bijna** alle opdrachten, behalve bijv. opdrachten waarbij je alleen documentatie moet schrijven, worden hierdoor getest tegen door ons geschreven cases. De feedback krijg je in leesbaar formaat in je console te zien. Wel zal die de eerste keer overwegend rood zijn; schrik daar niet van.

De code die je geacht wordt aan te passen vind je in het bestand src/Lib.hs van elk project. Het strekt ter aanbeveling om eerst het bestand en al het commentaar door te nemen, voordat je begint met aanpassen.
De aanpassingen die we van jullie vragen staan aangegeven met het woord TODO, in commentaar boven elke functie.

De vaakst voorkomende aanpassing is "Schrijf en documenteer deze functie." **Let op:** schrijf je commentaar in de Haddock-stijl! De twee vragen die goed commentaar sowieso beantwoordt zijn 1) wat doet deze functie? 2) hóé doet deze functie dat? Voor het gros van de functies is dit genoeg, maar voor de grotere functies is het handig om ook inline wat meer uitleg te geven over de werking.

Elk van de practica heeft tevens een eigen Readme, waar meer context in te vinden is. 

Heel veel succes met Haskell!