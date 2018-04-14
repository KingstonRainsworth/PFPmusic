-- things that are going to create problems
---random generator seeds are not passed through


module MusicMakerProject exposing (..)
import Random exposing (Generator, Seed)
--warnings
-- if the 2nd and third notes are all 1 half step apart turn off diminished chords

type Song = List (Melody, Chord, MelodyR, ChordR)

-- type for ryhthm melody-------------------------------------------
-- whole note half note quarter note eigth note sixteenth note
-- mw = melody whole note
type alias ProbOutKey = {ok : Float} -- prob that melody note is out key used
type alias ProbInKey = {ik : Float} -- probablility that melody note is in key
type alias ProbOutChord = {oc : Float}
type alias ProbInChord = {ic : Float} -- probability chord includes notes from melody in chord
type alias ProbRestMelody = {prm : Float}
type alias ProbRestChord = {prc : Float}


--------patternization inputs ------------------------------------------------------
{-type alias ProbPatternMatch = {ppm : Float} -- probability that the next note is a pattern
type alias ProbNoPatternMatch = {pnpm : Float}-}
type alias ProbInterPatternization = { pip : Float} -- likelihood of patterns in patterns *********0-1
type alias ProbPatternSizeAppear =
                {p4 : Float --probability that a particualr pattern will apear
          ,      p8 : Float
          ,      p12 : Float--odd number for 3/4 of 1/2 measure
          ,      p16 : Float
          ,      p32 : Float
          ,      p64 : Float
          ,      p128 : Float
          ,      p256 : Float}

type alias ProbPatternizationAppear =
                {pz4 : Float --probability that a particualr pattern will apear in a pattern
          ,      pz8 : Float
          ,      pz12 : Float--odd number for 3/4 of 1/2 measure
          ,      pz16 : Float
          ,      pz32 : Float
          ,      pz64 : Float
          ,      pz128 : Float}

type alias NumPatternSize =
                {n4 : Int --number of a particualr pattern to make
          ,      n8 : Int
          ,      n12 : Int
          ,      n16 : Int
          ,      n32 : Int
          ,      n64 : Int
          ,      n128 : Int
          ,      n256 : Int }

type alias PatternList = List (List (List Int)) -- list that holds patterns


type alias RNGSeed = {rngs : Int }

type alias ProbChordMelodyCorralation = { pcmc : Float} -- corralation between how likely chords with the melody with be present
type KeySignature = A|AS|B|C|CS|D|DS|E|F|FS|G|GS  --12  no flats only sharps or pick your own
--type alias Key = { key : KeySignature}
type alias KeySignatureSuggestion = {ksp : List Int}

type alias ProbMR = { mw : Float -- melody rhythm
          ,     m2 : Float
          ,     m3 : Float
          ,     m4 : Float
          ,     m5 : Float
          ,     m6 : Float
          ,     m7 : Float
          ,     m8 : Float
          ,     m9 : Float
          ,     m10 : Float
          ,     m11 : Float
          ,     m12 : Float
          ,     m13 : Float
          ,     m14 : Float
          ,     m15 : Float
          ,     m16 : Float}

type alias ProbCR = { cw : Float -- chord rhythm
          ,     c2 : Float
          ,     c3 : Float
          ,     c4 : Float
          ,     c5 : Float
          ,     c6 : Float
          ,     c7 : Float
          ,     c8 : Float
          ,     c9 : Float
          ,     c10 : Float
          ,     c11 : Float
          ,     c12 : Float
          ,     c13 : Float
          ,     c14 : Float
          ,     c15 : Float
          ,     c16 : Float}

type alias ProbOctaveMelody = { o1 : Float --
          ,     o2 : Float
          ,     o3 : Float
          ,     o4 : Float
          ,     o5 : Float
          ,     o6 : Float}

type alias ProbOctaveChord = { oc1 : Float
          ,     oc2 : Float
          ,     oc3 : Float
          ,     oc4 : Float
          ,     oc5 : Float } -- only goes to 5 to aviod overflowing piano

-- probabilities for chord decision
type alias ProbTypeChord = { proot : Float
          ,     pseveth : Float
          ,     pninth  : Float
          ,     peleventh : Float }
type alias ProbAppliedChord = { pac1 : Float
          ,     pac4 : Float
          ,     pac5 : Float
          ,     pac7 : Float}

type alias ProbRootChord = { prc1 : Float
          ,     prc2 : Float
          ,     prc3 : Float
          ,     prc4 : Float
          ,     prc5 : Float
          ,     prc6 : Float
          ,     prc7 : Float }
type alias ProbAddOnChord = { pnao : Float
          ,     psus2 : Float
          ,     psus4 : FLoat
          ,     pAug  : Float
          ,     pDim  : Float}

-- im removing type Duration and replacing it with a int 1-16 time
type Dur = W|H|Q|EI|S

type alias Note = Int

type alias ChordR = List Dur

type alias Melody = List Int

type alias Chord = List (Note,Note,Note)

type alias MelodyR = List Dur

notes = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88]

-- helper functions for multiple parts -------------------------------------

floatGenerator : Seed -> Float -> Float -> (Seed, Float)
floatGenerator se low high  =
  --  let s = Random.initialSeed se in
    let g = Random.float low high in
    let (n, s2) = Random.step g se in
    (s2 , n)
{-
calculateDur : Dur -> Int
calculateDur d =
  case d of
    W -> 16
    H -> 8
    Q -> 4
    EI -> 2
    S -> 1
-}
totalMR : Int -> MelodyR -> Int
totalMR tot mr =
  case mr of
    [] -> tot
    x::list_ -> totalMR (tot + x) list_

cycleThroughR : MelodyR -> Int -> Int
cycleThroughR list num =
  case list of
    [] -> num
  --  [x] -> cycleThroughR [] (calculateDur x + num)
  --  [x, y] -> cycleThroughR y (calculateDur x + num)
    x::list_ -> if num <= 0 then x
      else cycleThroughR list_ ( num - x)

cycleThroughM : MelodyR -> Melody -> Int -> Int -- used in chord section
cycleThroughM list lim num =
  case list of
    [] -> num --shouldnt happen
    x::list_ -> case lim of
      []-> num --shouldnt happen
      y::list2_ -> if num <= 0 then y
        else cycleThroughM list_ list2_ (x + num)

-------------------------------Main-------------------------------------------------


main : -- input EVERYTHING
main      =
let ppa = patternCreator --stuff here




















------------------------------ start program flow section --------------------------------------

workFlowMain : -> (MelodyR, ChordR, Melody, Chord)
workFlowMain   =

workFlowMainLoop : PatternList -> Int -> Int-> MelodyR -> ChordR -> Melody -> Chord -> (MelodyR, ChordR, Melody, Chord)
workFlowMainLoop pl siz maxsiz mr cr m c  =
  if (maxsiz - siz) < 4 then let tmr = melodyRhythm 0 siz se mr pmr in
                        List.append pat [tmr]  -- not sure how pat list lsit is going to work
                        let tcr = chordRhythm 0 siz se cr pcr
                        List.append pat [tcr]
                        let tm = melodyCreatorMain kss se pok pik ks pom tmr m in
                        List.append pat [tm]
                        let tc = chordCreatorMain se prestc c m mr paoc prc pac ptc loc cr kss in
                        List.append pat [tc]
                        (mr,cr,m,c)
  else
    let lf total = workFlowListGenerator 0 siz ppsa [] pl in
    let (s2, rn) = floatGenerator se 0.0 total in
    let ans = workFlowDecidePattern pl rn lf
    case ans of       -----------------------------------not sure this si the best way to do this
      x::y::z::t -> List.append mr x
                    List.append cr y
                    List.append m z
                    List.append c t
                    workFlowMainLoop pl (siz + totalMR x 0)



workFlowDecidePattern : PatternList -> Float -> List Float -> List (List Int)
workFlowDecidePattern pl rn lf =
  case pl of
    [] -> []
    x::list_ -> case lf of
                  [] -> []
                  y::lf1 -> if y > rn then x
                    else workFlowDecidePattern list_ rn lf1

getProbPatternSizeAppear : ProbPatternSizeAppear -> Int -> Float
getProbPatternSizeAppear ppa num =
  case num of
    4 -> ppa.p4
    8 -> ppa.p8
    12 -> ppa.p12
    16 -> ppa.p16
    32 -> ppa.p32
    64 -> ppa.p64
    128 -> ppa.p128
    256 -> ppa.p256

workFlowFloatListGenerator : Float -> Int -> ProbPatternSizeAppear -> List Float ->  PatternList -> (List Float, Float)  --- possible error in how y works
workFlowFloatListGenerator total siz ppsa lf pl =
  case pl of
    [] -> (lf,total)
    [x]::list_ -> [y]::x
                  if (List.length y) > siz then List.append lf 0 workFlowFloatListGenerator total siz ppsa lf list_
                    else List.append lf ((getProbPatternSizeAppear ppsa (List.length y)) +  workFlowListCreatorFixFloatListLoop  lf loc)
                     workFlowFloatListGenerator ((getProbPatternSizeAppear ppsa (List.length y)) +  workFlowListCreatorFixFloatListLoop  lf loc) siz ppsa lf list_


workFlowListCreatorFixFloatListLoop : List Float -> Int -> Float
workFlowListCreatorFixFloatListLoop lf loc =
  case lf of
    [] -> 0.0
    x::list_ -> if loc <= 0 then x
                else workFlowListCreatorFixFloatListLoop  list_ (loc -1)












------------------------------ start pattern section ----------------------------------------------

getProbPatternizationAppear : ProbPatternizationAppear -> Int -> Float
getProbPatternizationAppear ppa num =
  case num of
    4 -> ppa.pz4
    8 -> ppa.pz8
    12 -> ppa.pz12
    16 -> ppa.pz416
    32 -> ppa.pz32
    64 -> ppa.pz64
    128 -> ppa.pz128

patternCreatorMain : -> (Int)
patternCreatorMain =


findPatternToMatchListCreator : Int -> Float -> Int -> PatternList -> List Float -> ProbPatternizationAppear -> (List Float, Float) -- returns lsit of float probabilities
findPatternToMatchListCreator loc total siz pl lf ppa =
  case pl of
    [] -> (lf,total)
    [x]::list_ -> [y]::x
                if List.length y >= siz then List.append lf 0.0 (findPatternToMatchListCreator total siz list_ lf ppa)
                  else  List.append lf ((getProbPatternizationAppear ppa (List.length y)) + findPatternToMatchListCreatorFixFloatListLoop lf loc ))   -------------------loc might be in the wrong place may need to be loc  -1
                        findPatternToMatchListCreator (total+(getProbPatternizationAppear ppa (List.length y))+ findPatternToMatchListCreatorFixFloatListLoop lf loc ) siz list_ lf ppa

findPatternToMatchListCreatorFixFloatListLoop : List Float -> Int -> Float
findPatternToMatchListCreatorFixFloatListLoop lf loc =
  case lf of
    [] -> 0.0
    x::list_ -> if loc <= 0 then x
                else findPatternToMatchListCreatorFixFloatListLoop  list_ (loc -1)



findPatternMatchCycle : List Float -> Float -> Int -- takes lsit of probabilities and returns
findPatternMatchCycle lf num =
  case lf of
    [] -> lf
    x::list_ -> if x <= num then num
      else findPatternMatchCycle list_ num

findPatternCycle : PatternList -> Int -> List (List Int)
findPatternCycle pl num =
  case pl of
    [] -> pl
    x::list_ -> if num =< 0 then x
      else findPatternMatchCycle list_ (num - 1)

findPatternToMatch : Float -> Seed -> List Float -> ProbPatternizationAppear-> PatternList -> List (List Int) --takes the size or remaining space and calculates the probs for mathcing each pattern in list float form
findPatternToMatch total se lf ppa pl siz =
      let (s2, rn) = floatGenerator se 0.0 total in
      let ans = findPatternMatchCycle lf rn in
      findPatternCycle pl ans




findPatternToMatchMain :  Seed -> Int -> ProbPatternizationAppear-> ->List(List Int)
findPatternToMatchMain se siz ppa pl siz =
  let (lf,total) = findPatternToMatchListCreator  0.0 siz pl [] ppa
  findPatternToMatch total se lf ppa pl siz

patternCreator :
     ProbRestChord -> Chord -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int   -- int is loc
  -> KeySignatureSuggestion -> ProbOutKey -> ProbInKey -> KeySignature -> ProbOctaveMelody -> Melody
  -> ChordR -> ProbCR
  -> MelodyR -> ProbMR
  ->ProbInterPatternization -> Seed -> Int -> PatternList-> Int ->PatternList
patternCreator prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr pip se siz pat numpatsize =
  if siz == 4 then
    case numpatsize of
      0 -> pat
      _ ->  let tmr = melodyRhythm 0 siz se mr pmr in
            List.append pat [tmr]  -- not sure how pat list lsit is going to work
            let tcr = chordRhythm 0 siz se cr pcr
            List.append pat [tcr]
            let tm = melodyCreatorMain kss se pok pik ks pom tmr m in
            List.append pat [tm]
            let tc = chordCreatorMain se prestc c m mr paoc prc pac ptc loc cr kss
            List.append pat [tc]
            patternCreator8 prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr se siz pat (numpatsize -1)
  else
    let (s2, rn) = floatGenerator se 0.0 1.0 in
    if rn <= pip then
      findPatternToMatch ...


    else
      case numpatsize of
        0 -> pat
        _ ->  let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in   -----needs to be here becuase otherwise it would do melody for whole thing instead of 1 note
              let lf pt = melodyRhythmHelper [] arra 0.0 pmr
              let dn = melodyRhythmDecider arra
              let (s2, rn) = floatGenerator se 0.0 pt in
              let nn =  (melodyRhythmDecider arra rn) in

              let tmr = melodyRhythm 0 nn se mr pmr in
              List.append pat [tmr]  -- not sure how pat list lsit is going to work
              let tcr = chordRhythm 0 nn se cr pcr                                   ----------------------------------------------------logic error runs again wont be 0
              List.append pat [tcr]
              let tm = melodyCreatorMain kss se pok pik ks pom tmr m in
              List.append pat [tm]
              let tc = chordCreatorMain se prestc c m mr paoc prc pac ptc loc cr kss
              List.append pat [tc]
              patternCreator8 prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr se siz pat (numpatsize -1)



--------------Melody Rhythm code section -------------------------------------------------------------
keySignatureNeumericalHandler : KeySignature -> Int
keySignatureNeumericalHandler k = case k of
  A -> 0
  AS -> 1
  B -> 2
  C -> 3
  CS -> 4
  D -> 5
  DS -> 6
  E -> 7
  F -> 8
  FS -> 9
  G -> 10
  GS -> 11


getProbMR : Int -> ProbMR -> Float
getProbMR y x = case y of
  1 -> x.mw
  2 -> x.m2
  3 -> x.m3
  4 -> x.m4
  5 -> x.m5
  6 -> x.m6
  7 -> x.m7
  8 -> x.m8
  9 -> x.m9
  10 -> x.m10
  11 -> x.m11
  12 -> x.m12
  13 -> x.m13
  14 -> x.m14
  15 -> x.m15
  16 -> x.m16



melodyRhythmHelper : List  -> List Int -> Float  -> ProbMR -> (List Float, Int)
melodyRhythmHelepr lf li total pmr
  case li of
    [] -> (lf, total)
    x::list_ -> melodyRhythmHelper (List.append lf (probMR x pmr)) list_ (total + probMR x pmr) pmr

melodyRhythmDecider : List Int -> Float -> Int
melodyRhythmDecider li prob =
  case li of
    [] -> Debug.crash "float rythm decider"
    x::list_ -> if x > prob then x
      else melodyRhythmDecider list_ prob

melodyRhythm : Int -> Int -> Seed -> MelodyR ->ProbMR -> MelodyR --main of MR siz is the number of counts of the trick being created
melodyRhythm siz maxnote se mr pmr =
  let do = totalMR 0 mr in
  if do <= (maxnote - siz) then
    let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in
    let lf pt = melodyRhythmHelper [] arra 0.0 pmr
    let dn = melodyRhythmDecider arra
    let (s2, rn) = floatGenerator se 0.0 pt in
    let nn =  (melodyRhythmDecider arra rn) in
    List.append mr nn
    melodyRhythm (siz + nn) maxnote se mr pmr
  else
    let  hi = (cycleThroughR mr 0 )- ( maxnote - siz) in
    List.append mr [hi]
    -- remove last number calculate whats needed and put that in

--------------end of Melody Rhythm code Section ---------------------------------

--------------start of Chord Rhythm code section---------------------------------
-------cr being called mr ecasue of lazy coding
getProbCR : Int -> ProbCR -> Float
getProbCR y x = case y of
  1 -> x.cw
  2 -> x.c2
  3 -> x.c3
  4 -> x.c4
  5 -> x.c5
  6 -> x.c6
  7 -> x.c7
  8 -> x.c8
  9 -> x.c9
  10 -> x.c10
  11 -> x.c11
  12 -> x.c12
  13 -> x.c13
  14 -> x.c14
  15 -> x.c15
  16 -> x.c16

chordRhythmHelper : List  -> List Int -> Float  -> ProbCR -> (List Float, Int)
chordRhythmHelepr lf li total pmr
  case li of
    [] -> (lf, total)
    x::list_ -> chordRhythmHelper (List.append lf (probCR x pmr)) list_ (total + probCR x pmr) pmr

chordRhythmDecider : List Int -> Float -> Int
chordRhythmDecider li prob =
  case li of
    [] -> Debug.crash "chord rythm decider"
    x::list_ -> if x > prob then x
      else chordRhythmDecider list_ prob

chordRhythm : Int -> Int -> Seed -> ChordR ->ProbCR -> ChordR -- main of CR
chordRhythm siz maxnote se mr pmr =
  let do = cycleThroughR mr 0 in
  if do <= (maxnote - siz) then
    let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in
    let lf pt = chordRhythmHelper [] arra 0.0 pmr
    let dn = chordRhythmDecider arra
    let (s2, rn) = floatGenerator se 0.0 pt in
    let cn = chordRhythmDecider arra rn in
    List.append mr cn
    chordRhythm  (siz + cn) maxnote se cr pmr
  else
    let  hi = (cycleThroughR mr 0 ) - (maxnote - siz) in
    List.append mr [hi]


-------end code rhythm section-------------------------------------------

-------Start of Melody section -------------------------------------------
keySignatureHelperhandler : KeySignatureSuggestion -> List Int
keySignatureHelperhandler kss =
  case kss of
    [] -> [0,2,4,5,7,9,11]
    _ -> kss


-- start with melodyCreatorMain creates list of probs calling probHelpMelodyMain


melodyCreatorMain : KeySignatureSuggestion -> Seed -> ProbOutKey -> ProbInKey -> KeySignature -> ProbOctaveMelody -> MelodyR -> Melody -> Melody--more here
melodyCreatorMain kss se pok pik ks pom mr m =
  case mr of
  [] -> m
--  [_] -> m -- precent root is final
--  [_,_] -> m --something else here
  x::list -> let x = probHelpMelodyMain pok pik ks pom [] notes in
    let pt = Just (List.tail x) in
    let (s2, rn) = floatGenerator se 0.0 pt in
    List.append m [melodyCreatorMainHelper x rn]

--cycles through list of probs till it finds the right int
melodyCreatorMainHelper : List Float -> Float -> Int
melodyCreatorMainHelper lf f =
  case lf of
    [] -> 1 --needs to break with a error message
    x::list -> if x > f then (88 - List.length lf)
      else melodyCreatorMainHelper list f

probHelpMelodyMain :KeySignatureSuggestion -> ProbOutKey-> ProbInKey -> KeySignature -> ProbOctaveMelody ->List Float ->List Int -> List Float
probHelpMelodyMain kss pok pik ks pom pmr lr =
  let kss1 = keySignatureHelperhandler kss in
  case lr of
    [] -> pmr
    x::list_ -> List.append pmr [((octaveControlMelody ks pom x * sameKeyMelodyProb kss1 pok pik x ) + (Just List.tail pmr) ) ]
     probHelpMelodyMain pok pik ks pom pmr list_
--function calls for prob handleers


octaveControlMelody : KeySignature -> ProbOctaveMelody -> Int ->  Float
octaveControlMelody key x note =
  let temp = note + (keySignatureNeumericalHandler key) in
  if (temp / 12) == 0 then x.o1
    else if (temp / 12) == 1 then x.o2
      else if (temp / 12) == 2 then x.o3
        else if (temp / 12) == 3 then x.o4
          else if (temp / 12) == 4 then x.o5
            else if (temp / 12) == 5 then x.o6
  else 0.0

sameKeyMelodyProb : KeySignatureSuggestion -> ProbOutKey ->ProbInKey -> KeySignature -> Int -> Float
sameKeyMelodyProb kss outKey inkey key note =
  let temp = (note + (keySignatureNeumericalHandler key)) - 11 in
  let tem1 te1 = x::kss in
  let tem2 te2 = x::te1 in
  let tem3 te3 = x::te2 in
  let tem4 te4 = x::te3 in
  let tem5 te5 = x::te4 in
  let tem6 te6 = x::te5 in
  if (temp % (tem1+12)) == 0 then inkey
    else if (temp % tem2) == 0 then inkey
      else if (temp % tem3) == 0 then inkey
        else if (temp % tem4) == 0 then inkey
          else if (temp % tem5) == 0 then inkey
            else if (temp % tem6) == 0 then inkey
              else if (temp % te6) == 0 then inkey
              else outkey

--- start chord decision creator--------------------------------------------------------------------
--- Still Working On this part ---------------------------------------------------------------------

getProbTypeChord : Int -> ProbTypeChord -> Float
getProbTypeChord x ptc = case x of
        1 ->    ptc.proot
        2 ->    ptc.pseveth
        3 ->    ptc.pninth
        4 ->    ptc.peleventh

getProbAppliedChord : Int -> ProbAppliedChord -> Float
getProbAppliedChord x ptc = case x of
        1 ->    ptc.pac1
        2 ->    ptc.pac2
        3 ->    ptc.pac3
        4 ->    ptc.pac4

getProbRootChord : Int -> ProbRootChord -> Float
getProbRootChord x ptc = case x of
        0 ->    ptc.prc1
        1 ->    ptc.prc2
        2 ->    ptc.prc3
        3 ->    ptc.prc4
        4 ->    ptc.prc5
        5 ->    ptc.prc6
        6 ->    ptc.prc7

getProbAddOnChord : Int -> ProbAddOnChord -> Float
getProbAddOnChord x ptc = case x of
        0 ->    ptc.pnao
        1 ->    ptc.psus2
        2 ->    ptc.psus4
        3 ->    ptc.pAug
        4 ->    ptc.pDim







chordCreatorMain :Seed ->ProbRestChord -> Chord -> Melody -> MelodyR ->  ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> ChordR -> KeySignatureSuggestion -> Chord --picks chord from prob initiate loc as 0
chordCreatorMain s2 prestc c m mr paoc prc pac ptc loc cr kss =
  let lc = [[]] in
  let lc1 = chordListCreatorLoop kss lc 1 0 1 0 in
  case cr of
    [] -> c
    x::list_ ->   let lf = chordProbMain m mr cr paoc prc pac ptc (loc+x) 1 0 1 0 cr cr  lc1
                  List.append lf prestc
                  List.append lc1 []
                  let pt = Just (List.tail lf) in
                  let (s2, rn) = floatGenerator se 0.0 pt in
                  List.append c (chordListCycleThrough  lc1 (chordCreatorDecider lf rn 0))


chordListCycleThrough : List (List Int) -> Int -> List Int
chordListCycleThrough lc num =
  case lc of
    [] -> num
    x::list_ -> if num == 0 then x
      else chordListCycleThrough list_ (num - 1)


chordCreatorDecider : List Float -> Float ->Int -> Int
chordCreatorDecider probs prob loc =
  case probs of
    []-> Empty
    x::list_ -> if x >= prob then loc
      else chordCreatorDecider list_ prob loc




chordProbMain :Melody -> MelodyR -> ChordR -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord ->Int ->  Int -> Int -> Int -> Int -> List Float -> ChordR -> ChordR -> List List Int -> List Float --run through list chords
chordProbMain m mr cr paoc prc pac ptc loc ac rc tc aoc lf cr cruc lc =
    case lc of
       [] -> 0
       y::list_ -> chordProbMain paoc prc pac ptc loc ac r tc aoc (List.append lf ((chordProbHelper m mr cruc x paoc prc pac ptc loc ac rc tc aoc  ))+y) cr_ lc


chordTypeListLoop : Int -> Int -> Int -> Int -> (Int,Int,Int,Int)
chordTypeListLoop lc ac rc tc aoc =
  if aoc < 4 then ac rc tc (aoc + 1)
    else if tc < 4 then ac rc (tc + 1) 0
        else if rc < 6 then ac (rc + 1) 1 0
            else if ac < 4 then (ac +1) 0 1 0
  else lc

chordProbHelper :Melody -> MelodyR -> ChordR ->List List Int -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> Int -> Int -> Int -> Int -> Float -- returns the prob for chord x
chordProbHelper m mr cr lc paoc prc pac ptc loc ac rc tc aoc =
  let cc = chordProbFindMelodyNotes [] loc 0 m mr cr in
  case lc of
    [] -> lc
    x :: list_
      let p = (getProbTypeChord tc ptc )* (getProbAppliedChord ac pac) * (getProbRootChord rc prc) * (getProbAddOnChord aoc paoc) * (chordProbMelodyCorralation) in
      p -- dont delete needs to append to a list

chordProbMelodyCorralation :  ProbOutChord -> ProbInChord -> List Int -> Float -> MelodyR -> ChordR -> List Int -> Float -- loc is int time where we are in the song chs is hte list of notes of hte chord
chordProbMelodyCorralation poc pic mell prob mr cr chs =
  case chs of
    []-> prob
    x::list_ -> let mn ml = chordProbMelodyCorralationSecondLoop mell
                chordProbMelodyCorralation ml (prob * (chordProbLoop poc pic mn x)) mr cr list_

chordProbMelodyCorralationSecondLoop :List Int -> (Int, List Int)
chordProbMelodyCorralationSecondLoop = mn
  case mn of
    [] -> []
    x::list -> (x, list)

chordProbLoop : ProbOutChord -> ProbInChord -> Int -> Int -> Float
chordProbLoop outkey inkey notem notec =
    let temp = notem % 12 in
    let temp2 = notec % 12 in
    if temp == temp2 then inkey
      else outkey

chordProbFindMelodyNotes : List Int -> Int -> Int -> Melody -> MelodyR -> ChordR -> List Int -- this returns the list of notes that the chord overlap with
chordProbFindMelodyNotes nsf loc counter mr cr =
  case mr of
    []-> nsf
    x::list_ -> if counter >= loc + x then List.append nsf (cycleThroughM mr m counter)
      else if counter >= loc then nsf
        else chordProbFindMelodyNotes nsf loc (counter + x) m mr cr




chordListCreatorLoop : KeySignatureSuggestion -> List List Int ->  Int -> Int -> Int -> Int -> List List Int -- this may need to be revuiesd so all numbers after are 0 !!!!!!!!!!!!!!!!!!!!!
chordListCreatorLoop kss lc ac rc tc aoc =
  if aoc < 4 then chordListCreatorMain kss lc ac rc tc (aoc + 1)
    else if tc < 4 then chordListCreatorMain kss lc ac rc (tc + 1) aoc
        else if rc < 6 then chordListCreatorMain kss lc ac (rc + 1) tc aoc
            else if ac < 4 then chordListCreatorMain kss lc (ac + 1) rc tc aoc
  else lc

chordListCreatorMain: KeySignatureSuggestion -> List List Int ->  Int -> Int -> Int -> Int -> List List Int
chordListCreatorMain kss lc ac rc tc aoc  =
  let app = chordListCreatorApplied kss ac in
  let fr = chordListCreatorFindRoot kss rc in
  let lc1 = chordListCreatorAllElse lc kss fr app ac rc tc aoc in
  lc1

chordListCreatorAppliedHelper : Int -> Int -> Int -> KeySignatureSuggestion -> Int
chordListCreatorAppliedHelper lo hi ans kss =
  if lo == hi then ans
    else if hi > 7 then let hi1 = hi - 7 in
       case kss of
        [] -> Empty
        x::kss -> chordListCreatorAppliedHelper (lo + 1) hi1 (ans + x) kss
      else  case kss of
        [] -> Empty
        x::kss -> chordListCreatorAppliedHelper (lo + 1) hi (ans + x) kss

chordListCreatorApplied : KeySignatureSuggestion -> Int  -> Int
chordListCreatorApplied kss ac  =
  case ac of
    1 -> 0
    2 -> chordListCreatorAppliedHelper 0 4 0 kss
    3 -> chordListCreatorAppliedHelper 0 5 0 kss
    4 -> chordListCreatorAppliedHelper 0 7 0 kss

chordListCreatorFindRoot : KeySignatureSuggestion -> Int -> Int
chordListCreatorFindRoot kss rc =
  chordListCreatorAppliedHelper 0 rc 0 kss

chordListCreatorAllElse : List List Int -> KeySignatureSuggestion -> Int -> Int -> Int -> Int -> Int -> Int -> List List Int -- fr = found root lots here
chordListCreatorAllElse lc kss fr app ac rc tc aoc  =
  if ac == 4 then
    case tc of
      1 -> case aoc of
          0 -> []
          1 -> []
          2 -> []
          3 -> []
          4 ->
            let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1)]
            List.append lc temp
      2 -> case aoc of
          0 -> Nil -- change to empty sets
          1 -> Nil
          2 -> Nil
          3 -> Nil
          4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
                List.append lc temp
      3 -> case aoc of
          0 -> Nil
          1 -> Nil
          2 -> Nil
          3 -> Nil
          4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
                List.append lc temp
      4 -> case aoc of
          0 -> Nil
          1 -> Nil
          2 -> Nil
          3 -> Nil
          4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
                 List.append lc temp
  else
    case tc of
      1 -> case aoc of
        0 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss)]
              List.append lc temp
        1 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss)]
              List.append lc temp
        2 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss)]
              List.append lc temp
        3 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) + 1)]
                List.append lc temp
        4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1)]
              List.append lc temp
      2 -> case aoc of
        0 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        1 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        2 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        3 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
      3 -> case aoc of
        0 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        1 -> Nil
        2 -> Nil
        3 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
      4 -> case aoc of
        0 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        1 -> Nil
        2 -> Nil
        3 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp
        4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)]
              List.append lc temp









  --case len < 2
