# Recursie en Lijsten

Welkom bij het eerste practicum voor DeP. Deze week gaan we oefenen met een aantal basisprincipes. De eerste stap, voordat je begint met coden, is ervoor zorgen dat je omgeving werkt. Installeer Haskell en Stack, en check dat je met `stack run` dit checkpoint kan draaien.
Bij het eerste keer uitvoeren van deze code zullen een paar dingen opvallen:

1. Het duurt verschrikkelijk lang;
2. Als je code eenmaal gecompileerd is krijg je een hoop rode tekst en
3. Als je zelf met de code aan de gang gaat krijg je errors in de compilatie-stap.

Voor het eerste punt: geen paniek, dit is alleen de eerste keer dat je een (nieuw) stack-project op je computer draait. Alle dependencies worden voor je afgehandeld, maar dit kan dus even duren. Hou hier rekening mee als je op meerdere systemen werkt, of zodra je met de volgende checkpoint begint. Je kan het beste eerst een `stack run` doen voordat je zelf aan de code zit.

Voor het tweede en derde punt geldt: succes! De rode tekst komt uit de testfuncties die wij als docenten voor jullie hebben geschreven. De meest voorkomende, in eerste instantie, is een stacktrace over `Prelude.undefined`. Dit komt overeen met de lege functies hieronder. Je kan op elke plaats in een Haskell programma undefined gebruiken, zonder dat de compiler over types gaat klagen, maar bij het uitvoeren van je code gaat het mis. Jullie doel is dan ook alle `undefined`s uit de code te halen, en te vervangen met de juiste oplossing.
Als je dit gaat doen dan krijg je te maken met een nieuw type error: errors van de compiler. Haskell is sterk getypeerd, en vindt het niet grappig als je bijvoorbeeld een `Int` geeft waar een `Bool` had moeten staan. Tijdens het compilen doet Haskell een typecheck, en als deze faalt krijg je een type-error. Het oplossen hiervan is een substantieel deel van Haskell programmeren, maar zodra je programma vrij van deze errors is, zal 90% ook gewoon werken. Haskell is heel streng tijdens het compilen, waardoor tijdens de uitvoering fouten een stuk zeldzamer zijn. Natuurlijk bestaan dit soort fouten nog steeds (`undefined`, delen door 0, items uit een te korte lijst proberen te halen, ...) maar dit is meteen een stuk zeldzamer. Het leren lezen van type-errors kost aanvankelijk wat tijd, maar gaandeweg zal je hier aan wennen en zul je merken dat de compiler meer bezig is met meedenken dan met bestraffen.
 
Voor het uitvoeren van dit practicum ga je dus wat fouten op moeten lossen, doe dat altijd in deze volgorde:
1. Type errors (je code compileert niet)
2. Exceptions (rood) anders dan de `undefined` waar je mee begint (je code bevat fouten)
3. Foute antwoorden (geel) je code werkt, maar geeft niet het verwachte antwoord
4. `undefined` (rood), dit is de code waar je nog niet aan toe bent gekomen
5. Als het goed is, zal uiteindelijk alles groen zijn en kan je je werk inleveren. Hierna volgt in de meeste gevallen nog feedback op je code-style, zodat je dit ook kan verbeteren.

# De REPL
Naast dat je `stack run` kunt aanroepen om je code te testen kun je ook gebruik maken van een interactieve REPL (Read - Evaluate - Print - Loop) met het commando `stack repl`. Je komt hiermee in een interactieve shell terecht, zoals bijvoorbeeld ook bij Python mogelijk is. Je kunt hier je functies testen door ze aan te roepen, Haskell print dan het resultaat. Daarnaast kun je bijvoorbeeld het type van een functie of constante opvragen. Open een REPL en type

> ex1

Stack zal een error geven: je roept een functie aan zonder argumenten. Haskell zal klagen dat `ex1` het type `[Int] -> Int` heeft, en dat het onbekend is hoe je zoiets kan printen.
Fair, om de functie aan te roepen zul je een lijst mee moeten geven. Probeer nu

> ex1 [1,2,3]

Weer krijg je een fout van Haskell, maar nu is het een Exception (run-time fout). De functie `ex1` bevat een `undefined`. Dit is geen probleem, tot je de waarde ervan daadwerkelijk probeert op te vragen. Vul nu de functie in (gebruik eventueel de video van de les als voorbeeld), herlaad het bestand in de REPL en probeer het nog eens:

```
:r
ex1 [1,2,3]
```

Als het goed is krijg je nu de som, 6, terug.

Een aantal belangrijke commando's in GHCi (de REPL):
 - `:r`    herlaadt het bestand
 - `:l`    laadt een bestand (heb je met `stack repl` niet vaak nodig)
 - `:t`    vraag het type op van een functie of constante
 - `:q`    quit, kan ook met CTRL-D

Er zijn nog veel meer commando's mogelijk, maar dit is voor nu genoeg. Als je een `:` typt en dan op TAB drukt krijg je een lijstje. In het algemeen kun je TAB gebruiken om GHCi je huidige woord aan te laten vullen.

Kijk in GHCi nog even naar het type van `ex1`, `[1,2,3]` en `ex1 [1,2,3]`:

```
:t ex1
:t [1,2,3]
:t ex1 [1,2,3]
```

Merk op hoe Haskell omgaat met de types van functies: `ex1 :: [Int] -> Int` betekent dat ex1 een functie is die, gegeven een lijst van integers, een integer terug zal geven. Bij de aanroep wordt `[1,2,3]` ingevuld voor deze lijst, en is het resultaat een simpele `Int`. Merk ook op dat het type van de lijst `[1,2,3]` niet `[Int]` is, maar `Num a => [a]`. Dit komt omdat de lijst ook een lijst van komma-getallen (`Float`s) had kunnen zijn, bijvoorbeeld. Haskell heeft niet genoeg informatie en laat het voor nu even bij dat het getallen zijn (zoveel is duidelijk). Zodra je de lijst aan `ex1` geeft weet Haskell dat het de lijst als een lijst van integers moet zien om een kloppende functie-aanroep te kunnen doen. De notatie met de dubbele pijl kun je lezen als een voorwaarde: de lijst kan alles bevatten, zoalng het maar op een nummer lijkt. In een latere les zullen we hier meer van zien.
Je kunt nu GHCi verlaten, of erbij houden terwijl je met de oefeningen aan de slag gaat. Bij Haskell programmeren wordt doorgaans geen gebruik gemaakt van een IDE, zoals je van andere talen misschien gewend bent, maar wordt de REPL vaak op eenzelfde manier ingezet.

Als alle oefeningen hierboven gelukt zijn: Lekker bezig! Je kan je werk inleveren, waarna je van ons (docenten / studentassistent) feedback kan krijgen op je code. Je kunt echter ook Haskell zelf vragen om suggesties, met het programma'tje `hlint` (extern beschikbaar). Dit roep je aan op een Haskell-bestand of een map met meerdere Haskell-bestanden, waarna je geautomatiseerde feedback krijgt. Deze is niet in 100% van de gevallen ideaal, maar kan wel helpen om beter met de taal te leren werken. In het geval van deze opgaven zul je vaak de mededeling krijgen dat iets korter had gekund met een ingebouwde functie, of door hogere-orde functies te gebruiken. Voor nu is de oefening natuurlijk om het zelf te programmeren, maar het kan geen kwaad om te zien hoe dit korter/makkelijker had gekund.
