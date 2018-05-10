-- things that are going to create problems
---random generator seeds are not passed through


module MusicMakerProject exposing (..)
import Random exposing (Generator, Seed)
import Html exposing (text)

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

type alias StartAtRoot = {sar : Int}
type alias EndAtRoot = {ear : Int}

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

type alias PatternList = List (List (List MelodyR)(List ChordR)(List Melody)(List Chord)) -- list that holds patterns


type alias RNGSeed = {rngs : Int }

type alias ProbChordMelodyCorralation = { pcmc : Float} -- corralation between how likely chords with the melody with be present
type KeySignature = A|AS|B|C|CS|D|DS|E|F|FS|G|GS  -- THIS IS ALSO USER INPUT
--type alias Key = { key : KeySignature}
type alias KeySignatureSuggestion = List Int

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
          ,     psus4 : Float
          ,     pAug  : Float
          ,     pDim  : Float}

-- im removing type Duration and replacing it with a int 1-16 time
--type Dur = W|H|Q|EI|S


notes = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87]

-- helper functions for multiple parts -------------------------------------

floatGenerator : Seed -> Float -> Float -> (Seed, Float)
floatGenerator se low high  =
  --  let s = Random.initialSeed se in
    let g = Random.float low high in
    let (n, s2) = Random.step g se in
    (s2 , n)

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











--------------------------------everything above is user input except Song and PatternList------------------------------------














type alias Note = Int
type alias ChordR = List Int
type alias Melody = List Int
type alias Chord = List (List Int)
type alias MelodyR = List Int

--change order of result
mmk : ProbOutChord -> ProbInChord -> ProbPatternSizeAppear -> ProbRestChord -> ProbOctaveMelody -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord ->
  ProbTypeChord ->ProbCR -> ProbMR -> KeySignature -> KeySignatureSuggestion -> ProbOutKey -> ProbInKey -> Seed -> (ChordR, Melody, Chord, MelodyR)
mmk poc pic ppsa prestc pom paoc prc pac ptc pcr pmr ks kss pok pik se =
--  let hi = textout in
  let mr : MelodyR
      mr = melodyRhythm 0 25 se [] pmr in
  let cr = chordRhythm 0 25 se [] pcr in
  let m = melodyCreatorMain kss se pok pik ks pom mr [] in
  let c : Chord
      c = chordworkaroundloop [] se cr in
  (cr, m, c, mr)





chordworkaroundloop : Chord -> Seed -> ChordR -> Chord
chordworkaroundloop ce se cr =
  let lc = [[12,16,19],[14,17,21],[16,19,23],[17,21,24],[19,23,26],[21,24,28],[23,26,27]] in
  let lf = [1, 2, 3, 4, 5, 6, 7] in
  let (s2m, rn) = floatGenerator se 0.0 7 in
  case cr of
    [] -> ce
    x::list_ -> let c = ce ++ [  getchordfromworkaround rn lf lc ] in
      chordworkaroundloop c s2m list_
getchordfromworkaround : Float ->List Int -> List (List Int) -> List Int
getchordfromworkaround f lf c =
  case lf of
    [] -> Debug.crash (toString(f))
    x::list_ -> case c of
      [] -> Debug.crash "f"
      t::list ->
       if (toFloat x) >= f then t
      else getchordfromworkaround f list_ list




main =
  text "Hello, World!"


getStartAtRoot : StartAtRoot -> Int
getStartAtRoot s =
  s.sar

startAtRoot : KeySignature -> StartAtRoot -> Melody -> Melody
startAtRoot kss sar m =
  let d = getStartAtRoot sar in
  if d == 1 then let n = (keySignatureNeumericalHandler kss) + 48 in
      let temp = List.drop 1 m in
      let temp1 = List.append [n] m in
      temp1
  else  m

getEndAtRoot : EndAtRoot -> Int
getEndAtRoot s =
  s.ear

endAtRoot : KeySignature-> EndAtRoot -> Melody -> Melody
endAtRoot kss sar m =
  let d = getEndAtRoot sar in
  if d == 1 then let n = (keySignatureNeumericalHandler kss) + 48 in
      let temp = List.reverse m in
      let temp1 = List.drop 1 temp in
      let temp2 = List.append [n] temp1 in
      let temp3 = List.reverse temp2 in
      temp3
  else m

getLenFloatList : List Float -> Float -> Float
getLenFloatList lf f =
  case lf of
    [] -> f
    x::list_ -> getLenFloatList list_ (f + x)

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
  _ ->    Debug.crash "tried get bad value"


melodyRhythmHelper : List Float  -> List Int  -> ProbMR -> List Float
melodyRhythmHelper lf li pmr =
  case li of
    [] ->  lf
    x::list_ -> melodyRhythmHelper (List.append lf [getProbMR x pmr]) list_  pmr

melodyRhythmDecider : List Int -> Float -> Int
melodyRhythmDecider li prob =
  case li of
    [] -> Debug.crash "float rythm decider"
    x::list_ -> if (toFloat x ) > prob then x
      else melodyRhythmDecider list_ prob

melodyRhythm : Int -> Int -> Seed -> MelodyR ->ProbMR -> MelodyR --main of MR siz is the number of counts of the trick being created
melodyRhythm siz maxnote se mr pmr =
  if siz <= maxnote then
    let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in
    let lf = melodyRhythmHelper [] arra pmr in
    let pt = getLenFloatList lf 0.0 in
    let dn = melodyRhythmDecider arra in
    let (s2, rn) = floatGenerator se 0.0 pt in
    let nn =  (melodyRhythmDecider arra rn) in
    let mr2 = List.append mr [nn] in
    let do1 = totalMR 0 mr2 in
    if nn > (maxnote - siz) then
      let mr1 = List.append mr [(maxnote - siz)] in
      mr1
    else
      let mr1 = List.append mr [nn] in
      melodyRhythm (siz + nn) maxnote s2 mr1 pmr
  else
    let  hi = (cycleThroughR mr 0 )- ( maxnote - siz) in
    List.append mr [hi]
    -- remove last number calculate whats needed and put that in














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
  _ ->    Debug.crash "tried get bad value"

chordRhythmHelper : List Float -> List Int  -> ProbCR -> List Float
chordRhythmHelper lf li pmr =
  case li of
    [] -> lf
    x::list_ -> let lf1 = List.append lf [(getProbCR x pmr)] in
      chordRhythmHelper lf1 list_  pmr

chordRhythmDecider : List Int -> Float -> Int
chordRhythmDecider li prob =
  case li of
    [] -> Debug.crash "chord rythm decider"
    x::list_ -> let t = toFloat x in
      if t > prob then x
      else chordRhythmDecider list_ prob

chordRhythm : Int -> Int -> Seed -> ChordR ->ProbCR -> ChordR -- main of CR
chordRhythm siz maxnote se mr pmr =
  if siz <= maxnote then
    let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in
    let lf = chordRhythmHelper [] arra pmr in
    let pt = getLenFloatList lf 0.0 in
    let dn = chordRhythmDecider arra in
    let (s2, rn) = floatGenerator se 0.0 pt in
    let nn =  (chordRhythmDecider arra rn) in
    let mr2 = List.append mr [nn] in
    let do1 = totalMR 0 mr2 in
    if nn > (maxnote - siz) then
      let mr1 = List.append mr [(maxnote - siz)] in
      mr1
    else
      let mr1 = List.append mr [nn] in
      chordRhythm (siz + nn) maxnote s2 mr1 pmr
  else
    let  hi = (cycleThroughR mr 0 )- ( maxnote - siz) in
    List.append mr [hi]





-------Start of Melody section -------------------------------------------

--chang List Int to KSS
keySignatureHelperhandler : KeySignatureSuggestion -> KeySignatureSuggestion
keySignatureHelperhandler kss =
  [0,2,4,5,7,9,11]
{-
                case se of
                  0-> Debug.crash "0"
                  _ -> Debug.crash "!0"

-}


-- start with melodyCreatorMain creates list of probs calling probHelpMelodyMain


melodyCreatorMain : KeySignatureSuggestion -> Seed -> ProbOutKey -> ProbInKey -> KeySignature -> ProbOctaveMelody -> MelodyR -> Melody -> Melody--more here
melodyCreatorMain kss se pok pik ks pom mr m =
  case mr of
    [] -> m
    x::list -> let  tem : List (Float)
                    tem = probHelpMelodyMain kss pok pik ks pom [] notes in
      let temp : List Float
          temp = (List.reverse tem) in
      let pt = melodySaveAss 0.0 temp in
      let (s2, rn) = floatGenerator se 0.0 pt in
      melodyCreatorMain kss s2 pok pik ks pom list (List.append m [melodyCreatorMainHelper tem rn])



melodySaveAss : Float -> List (Float) -> Float
melodySaveAss f lf =
  case lf of
    [] -> f
    x::list_ -> melodySaveAss x list_
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
    x::list_ -> if pmr == [] then let temp = List.append pmr [(((octaveControlMelody ks pom x) * (sameKeyMelodyProb kss1 pok pik ks x )) ) ] in
                  probHelpMelodyMain kss pok pik ks pom temp list_
                else if List.length( pmr) < 88 then let temp = List.append pmr [(((octaveControlMelody ks pom x) * (sameKeyMelodyProb kss1 pok pik ks x )) + (probMelodySaveAss pmr 0.0) ) ] in
                    probHelpMelodyMain kss pok pik ks pom temp list_
                  else    Debug.crash (toString(x))
--function calls for prob handleers

probMelodySaveAss : List Float -> Float -> Float
probMelodySaveAss lf f =
  case lf of
    [] -> f
    x::list_ -> probMelodySaveAss list_ x

octaveControlMelody : KeySignature -> ProbOctaveMelody -> Int ->  Float
octaveControlMelody key x note =
  let temp = toFloat (note + (keySignatureNeumericalHandler key)) in
  if ( temp  / 12) == 0 then x.o1
    else if (temp / 12) == 1 then x.o2
      else if (temp / 12) == 2 then x.o3
        else if (temp / 12) == 3 then x.o4
          else if (temp / 12) == 4 then x.o5
            else if (temp / 12) == 5 then x.o6
  else 0.0

getInKey : ProbInKey -> Float
getInKey ik =
  ik.ik

getOutKey : ProbOutKey -> Float
getOutKey ik =
  ik.ok

sameKeyMelodyProb : List Int -> ProbOutKey ->ProbInKey -> KeySignature -> Int -> Float
sameKeyMelodyProb kss outKey inkey key note =
  let temp = (note + (keySignatureNeumericalHandler key)) - 11 in
  case kss of
    [] -> getOutKey outKey
    x::list_ ->   if x == 0 then
                  if (temp % (x + 12)) == 0 then getInKey inkey
                  else sameKeyMelodyProb list_ outKey inkey key note
                else
                  if (temp % x) == 0 then getInKey inkey
                  else sameKeyMelodyProb list_ outKey inkey key note



  --  let t = Debug.crash "tester" in





getInChord : ProbInChord -> Float
getInChord ik =
  ik.ic

getOutChord : ProbOutChord -> Float
getOutChord ik =
  ik.oc






getProbTypeChord : Int -> ProbTypeChord -> Float
getProbTypeChord x ptc = case x of
        1 ->    ptc.proot
        2 ->    ptc.pseveth
        3 ->    ptc.pninth
        4 ->    ptc.peleventh
        _ ->    Debug.crash "tried get bad value"

getProbAppliedChord : Int -> ProbAppliedChord -> Float
getProbAppliedChord x ptc = case x of
        1 ->    ptc.pac1
        2 ->    ptc.pac4
        3 ->    ptc.pac5
        4 ->    ptc.pac7
        _ ->    Debug.crash "tried get bad value"

getProbRootChord : Int -> ProbRootChord -> Float
getProbRootChord x ptc = case x of
        0 ->    ptc.prc1
        1 ->    ptc.prc2
        2 ->    ptc.prc3
        3 ->    ptc.prc4
        4 ->    ptc.prc5
        5 ->    ptc.prc6
        6 ->    ptc.prc7
        _ ->    Debug.crash "tried get bad value"

getProbAddOnChord : Int -> ProbAddOnChord -> Float
getProbAddOnChord x ptc = case x of
        0 ->    ptc.pnao
        1 ->    ptc.psus2
        2 ->    ptc.psus4
        3 ->    ptc.pAug
        4 ->    ptc.pDim
        _ ->    Debug.crash "tried get bad value"


getProbRestChord : ProbRestChord -> Float
getProbRestChord p =
  p.prc

--[[12,16,19][14,17,21][16,19,23][17,21,24][19,23,26][21,24,28][23,26,27]


chordCreatorMain :ProbOutChord -> ProbInChord -> Seed ->ProbRestChord -> Chord -> Melody -> MelodyR ->  ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> ChordR -> KeySignatureSuggestion -> Chord --picks chord from prob initiate loc as 0
chordCreatorMain poc pic se prestc c m mr paoc prc pac ptc loc cr kss =
  let lc : List (List (Int))
      lc = [[]] in
  let lc1 : List (List (Int))
      lc1 =  chordListCreatorLoopChecker kss lc 1 0 1 0 in

  case cr of
    [] -> c
    x::list_ ->
                  let lf :List (Float)
                      lf = chordProbMain poc pic m mr cr paoc prc pac ptc (loc+x) 1 0 1 0 [] cr  lc1 in
                  let lf1 =  List.append lf [getProbRestChord prestc] in
                  let lc2 : List (List (Int))
                      lc2 = List.append lc1 [] in
                  let numend = chordGetend 0.0 lf1 in
                  let (s2, rn) = floatGenerator se 0.0 numend in
                  List.append c [(chordListCycleThrough  lc2 (chordCreatorDecider lf1 rn 0))]

chordGetend : Float -> List Float -> Float
chordGetend f lf =
  case lf of
    [] -> f
    x::list_ -> chordGetend (f+x) list_


chordListCycleThrough : List (List Int) -> Int -> List Int
chordListCycleThrough lc num =
  case lc of
    [] -> [num]
    x::list_ -> if num == 0 then x
      else chordListCycleThrough list_ (num - 1)

chordCreatorDecider : List Float -> Float ->Int -> Int
chordCreatorDecider probs prob loc =
  case probs of
    []-> Debug.crash "chordCreatorDecider"
    x::list_ -> if x >= prob then loc
      else chordCreatorDecider list_ prob loc

chordProbMain : ProbOutChord -> ProbInChord -> Melody -> MelodyR -> ChordR -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord ->Int ->  Int -> Int -> Int -> Int -> List Float -> ChordR -> List (List (Int)) -> List Float --run through list chords
chordProbMain poc pic m mr cruc paoc prc pac ptc loc ac rc tc aoc lf cr lc =
    case lc of
       [] -> lf
       y::list_ -> let lf1 = List.append lf [chordProbHelper poc pic m mr cruc y paoc prc pac ptc loc ac rc tc aoc] in
                              chordProbMain poc pic m mr cruc paoc prc pac ptc loc ac rc tc aoc lf1 cr list_
                  {-else
                              let temp = List.reverse lf in
                              let temp1 = Just (List.head [temp]) in
                              let temp2 = (chordProbHelper m mr cruc y paoc prc pac ptc loc ac rc tc aoc )  in
                              let temp3 = temp2 + Just (temp1) in
                              let lf1 = List.append lf [temp3] in
                              chordProbMain m mr cruc paoc prc pac ptc loc ac rc tc aoc lf1 cr list_
-}
{-chordTypeListLoop : Int -> Int -> Int -> Int -> (Int,Int,Int,Int)
chordTypeListLoop lc ac rc tc aoc =
  if aoc < 4 then (ac, rc, tc, (aoc + 1))
    else if tc < 4 then (ac, rc, (tc + 1), 0)
        else if rc < 6 then (ac, (rc + 1), 1, 0)
            else if ac < 4 then ((ac +1), 0, 1, 0)
  else (4,4,6,4)-}

chordProbHelper :ProbOutChord -> ProbInChord -> Melody -> MelodyR -> ChordR ->List (Int) -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> Int -> Int -> Int -> Int -> Float -- returns the prob for chord x
chordProbHelper poc pic m mr cr lc paoc prc pac ptc loc ac rc tc aoc =
  let cc = chordProbFindMelodyNotes [] loc 0 m mr cr in
  case lc of
    [] -> 0.0
    x :: list_ ->
      let p = (getProbTypeChord tc ptc )* (getProbAppliedChord ac pac) * (getProbRootChord rc prc) * (getProbAddOnChord aoc paoc) * (chordProbMelodyCorralation poc pic x 1 mr cr cc ) in
      p -- dont delete needs to append to a list

chordProbMelodyCorralation :  ProbOutChord -> ProbInChord -> Int -> Float -> MelodyR -> ChordR -> List Int -> Float -- loc is int time where we are in the song chs is hte list of notes of hte chord
chordProbMelodyCorralation poc pic mell prob mr cr chs =
  case chs of
    []-> prob
    x::list_ ->
                chordProbMelodyCorralation poc pic mell (prob * (chordProbLoop poc pic x mell)) mr cr list_

{-chordProbMelodyCorralationSecondLoop :List Int -> (Int, List Int)
chordProbMelodyCorralationSecondLoop mn =
  case mn of
    [] -> (Nothing, [])
    x::list -> (x, list)
-}
chordProbLoop : ProbOutChord -> ProbInChord -> Int -> Int -> Float
chordProbLoop outkey inkey notem notec =
    let temp = notem % 12 in
    let temp2 = notec % 12 in
    if temp == temp2 then getInChord inkey
      else getOutChord outkey

chordProbFindMelodyNotes : List Int -> Int -> Int -> Melody -> MelodyR -> ChordR -> List Int -- this returns the list of notes that the chord overlap with
chordProbFindMelodyNotes nsf loc counter m mr cr =
  case mr of
    []-> nsf
    x::list_ -> if counter >= loc + x then List.append nsf [(cycleThroughM mr m counter)]
      else if counter >= loc then nsf
        else chordProbFindMelodyNotes nsf loc (counter + x) m mr cr


chordListCreatorLoopChecker : KeySignatureSuggestion -> List (List (Int)) ->  Int -> Int -> Int -> Int -> List (List (Int))
chordListCreatorLoopChecker kss lc ac rc tc aoc =
  Debug.crash "aldskfj;lasf"
  {-case aoc of
    3 -> case tc of
      3 -> let t = Debug.crash (toString(lc)) in case rc of
        6 -> case ac of
          3 -> lc
          _ -> chordListCreatorLoop  kss lc ac 0 1 0
        _ -> chordListCreatorLoop  kss lc ac rc 1 0
      _ -> chordListCreatorLoop  kss lc ac 0 1 0
    _ -> chordListCreatorLoop  kss lc 1 0 1 0
  if aoc < 3 then let t = Debug.crash (toString(aoc)) in
    else if tc < 3 then let t = Debug.crash (toString(lc)) in
      else if rc < 6 then
        else if ac < 3 then lc
          else chordListCreatorLoop  kss lc ac 0 1 0
        else  chordListCreatorLoop  kss lc ac rc 1 0
      else  chordListCreatorLoop  kss lc ac rc tc 0
    else  chordListCreatorLoop  kss lc ac rc tc aoc-}


chordListCreatorLoop : KeySignatureSuggestion -> List (List (Int)) ->  Int -> Int -> Int -> Int -> List (List (Int)) -- this may need to be revuiesd so all numbers after are 0 !!!!!!!!!!!!!!!!!!!!!
chordListCreatorLoop kss lc ac rc tc aoc =
  case (ac,rc,tc,aoc) of
    (4,4,6,4) ->  lc
    (_,_,_,_) ->  let lc1 = chordListCreatorMain kss lc ac rc tc aoc in
                  if aoc < 4 then chordListCreatorLoopChecker kss lc1 ac rc tc (aoc + 1)
                    else if tc < 4 then  chordListCreatorLoopChecker kss lc1 ac rc (tc + 1) aoc
                      else if rc <= 6  then chordListCreatorLoopChecker kss lc1 ac (rc + 1) tc aoc
                        else if ac < 4 then chordListCreatorLoopChecker kss lc1 (ac + 1) rc tc aoc
                            else lc

chordListCreatorMain: KeySignatureSuggestion -> List (List (Int)) ->  Int -> Int -> Int -> Int -> List (List (Int))
chordListCreatorMain kss lc ac rc tc aoc  =
  let app = chordListCreatorApplied kss ac in
  let fr = chordListCreatorFindRoot kss rc in
  let lc1 = chordListCreatorAllElse lc kss fr app ac rc tc aoc in
  lc1

chordListCreatorAppliedHelper : Int -> Int -> Int -> List Int -> Int
chordListCreatorAppliedHelper lo hi ans kss =
  if lo == hi then ans
    else if hi > 7 then let hi1 = hi - 7 in
       case kss of
        [] -> ans
        x::kss_ -> chordListCreatorAppliedHelper (lo + 1) hi1 (ans + x) kss_
      else  case kss of
        [] -> ans
        x::kss_ -> chordListCreatorAppliedHelper (lo + 1) hi (ans + x) kss_

chordListCreatorApplied : KeySignatureSuggestion -> Int  -> Int
chordListCreatorApplied kss ac  =
  case ac of
    1 -> 0
    2 -> chordListCreatorAppliedHelper 0 4 0 (keySignatureHelperhandler kss)
    3 -> chordListCreatorAppliedHelper 0 5 0 (keySignatureHelperhandler kss)
    4 -> chordListCreatorAppliedHelper 0 7 0 (keySignatureHelperhandler kss)
    _ ->    Debug.crash "tried get bad value"




chordListCreatorFindRoot : KeySignatureSuggestion -> Int -> Int
chordListCreatorFindRoot kss rc =
  chordListCreatorAppliedHelper 0 rc 0 (keySignatureHelperhandler kss)

chordListCreatorAllElse : List (List (Int)) -> KeySignatureSuggestion -> Int -> Int -> Int -> Int -> Int -> Int -> List (List (Int)) -- fr = found root lots here
chordListCreatorAllElse lc kss fr app ac rc tc aoc  =
--  let t = Debug.crash (toString(ac,rc,tc,aoc)) in
  if ac == 4 then
    case tc of
      1 -> case aoc of
          0 -> lc
          1 -> lc
          2 -> lc
          3 -> lc
          4 ->
            let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)) - 1)] in

            List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      2 -> case aoc of
          0 -> lc -- change to empty sets
          1 -> lc
          2 -> lc
          3 -> lc
          4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
                List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      3 -> case aoc of
          0 -> lc
          1 -> lc
          2 -> lc
          3 -> lc
          4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
                List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      4 -> case aoc of
          0 -> lc
          1 -> lc
          2 -> lc
          3 -> lc
          4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 (keySignatureHelperhandler kss)) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
                 List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      _ ->    Debug.crash "tried get bad value"
  else
    case tc of
      1 -> case aoc of
        0 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        1 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        2 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+3) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        3 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)) + 1)] in
                List.append lc [temp]
        4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)) - 1)] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      2 -> case aoc of
        0 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        1 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        2 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+3) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        3 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 (keySignatureHelperhandler kss)) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      3 -> case aoc of
        0 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        1 -> lc
        2 -> lc
        3 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 (keySignatureHelperhandler kss)) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      4 -> case aoc of
        0 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), (app + chordListCreatorAppliedHelper 0 (rc+3) 0 (keySignatureHelperhandler kss)),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        1 -> lc
        2 -> lc
        3 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 (keySignatureHelperhandler kss)) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 (keySignatureHelperhandler kss)), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 (keySignatureHelperhandler kss)) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 (keySignatureHelperhandler kss))] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      _ ->    Debug.crash "tried get bad value"
