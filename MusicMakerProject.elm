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














--------------------------------everything above is user input except Song and PatternList------------------------------------














type alias Note = Int
type alias ChordR = List Int
type alias Melody = List Int
type alias Chord = List (List Int)
type alias MelodyR = List Int

notes = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88]

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

-------------------------------Main-------------------------------------------------

--change order of result
mmk : ProbOutChord -> ProbInChord -> ProbPatternSizeAppear -> ProbRestChord -> ProbOctaveMelody -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord ->
  ProbTypeChord ->ProbCR -> ProbMR -> KeySignature -> KeySignatureSuggestion -> ProbOutKey -> ProbInKey -> Seed -> (ChordR, Melody, Chord, MelodyR)
mmk poc pic ppsa prestc pom paoc prc pac ptc pcr pmr ks kss pok pik se =
  let mr = melodyRhythm 0 256 se [] pmr in
  let cr = chordRhythm 0 256 se [] pcr in
  let m = melodyCreatorMain kss se pok pik ks pom mr [] in
  let c = chordCreatorMain poc pic se prestc [] m mr paoc prc pac ptc 0 cr kss in
  (cr, m, c, mr)








{-




-------------------------------start/ end at root section --------------------------------------
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


------------------------------ start program flow section --------------------------------------
{-
workFlowMain : -> (MelodyR, ChordR, Melody, Chord)
workFlowMain   =
-}
workFlowMainLoop : ProbOutChord -> ProbInChord -> ProbPatternSizeAppear -> PatternList -> ProbRestChord -> ProbOctaveMelody -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord ->
  ProbTypeChord ->ProbCR -> ProbMR -> KeySignature -> KeySignatureSuggestion -> ProbOutKey -> ProbInKey -> Seed -> PatternList -> Int -> Int-> MelodyR -> ChordR -> Melody -> Chord ->PatternList
workFlowMainLoop poc pic ppsa pat prestc pom paoc prc pac ptc pcr pmr ks kss pok pik se pl siz maxsiz mr cr m c  =
  if (maxsiz - siz) < 4 then
                          let tmr = melodyRhythm 0 siz se mr pmr in
                          let tcr = chordRhythm 0 siz se cr pcr in
                          let tm = melodyCreatorMain kss se pok pik ks pom tmr m in
                          let tc = chordCreatorMain poc pic se prestc c m mr paoc prc pac ptc 0 cr kss in -- 0 in place of loc
                          List.append pat  [tmr,tcr,tm,tc]


  else
    let (lf, total) = workFlowFloatListGenerator 0 siz ppsa [] pl in
    let (s2, rn) = floatGenerator se 0.0 total in
    let ans = workFlowDecidePattern pl rn lf in
    let mr1 =  List.append mr (workFlowInsertPattern ans 0) in
    let cr1 =  List.append cr (workFlowInsertPattern ans 1) in
    let m1 =  List.append m (workFlowInsertPattern ans 2) in
    let c1 =  List.append c (workFlowInsertPattern ans 3) in
    workFlowMainLoop poc pic ppsa pat prestc pom paoc prc pac ptc pcr pmr ks kss pok pik se pl (siz + totalMR 0 (workFlowInsertPattern ans 0)) maxsiz mr1 cr1 m1 c1

workFlowInsertPattern : List (List Int) -> Int -> List Int
workFlowInsertPattern lx w =
  case lx of
    [] -> Debug.crash "bad"
    x::list_ -> if w <= 0 then x
                  else workFlowInsertPattern list_ (w-1)


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
    [x]::list_ -> case x of
      y::h ->
                  if (List.length y) > siz then
                    let lfafter = List.append lf [0.0] in
                    workFlowFloatListGenerator total siz ppsa lfafter list_
                    else let temp =   ((getProbPatternSizeAppear ppsa (List.length y)) +  workFlowListCreatorFixFloatListLoop  lf siz) in
                      let lf3 =  List.append lf [temp] in
                     workFlowFloatListGenerator ((getProbPatternSizeAppear ppsa (List.length y)) +  workFlowListCreatorFixFloatListLoop  lf3 siz) siz ppsa lf3 list_


workFlowListCreatorFixFloatListLoop : List Float -> Int -> Float
workFlowListCreatorFixFloatListLoop lf loc =
  case lf of
    [] -> 0.0
    x::list_ -> if loc <= 0 then x
                else workFlowListCreatorFixFloatListLoop  list_ (loc - 1)












------------------------------ start pattern section ----------------------------------------------
getProbInterPatternization : ProbInterPatternization -> Float
getProbInterPatternization pip =
   pip.pip

getProbPatternizationAppear : ProbPatternizationAppear -> Int -> Float
getProbPatternizationAppear ppa num =
  case num of
    4 -> ppa.pz4
    8 -> ppa.pz8
    12 -> ppa.pz12
    16 -> ppa.pz16
    32 -> ppa.pz32
    64 -> ppa.pz64
    128 -> ppa.pz128
{-
patternCreatorMain : -> (Int)
patternCreatorMain =
-}

findPatternToMatchListCreator : Int -> Float -> Int -> PatternList -> List Float -> ProbPatternizationAppear -> (List Float, Float) -- returns lsit of float probabilities
findPatternToMatchListCreator loc total siz pl lf ppa =
  case pl of
    [] -> (lf,total)
    x::list_ -> case x of
       y::h -> if (List.length y) <= siz then
                  let lf1 = List.append lf [0.0] in
                  findPatternToMatchListCreator loc total siz list_ lf1 ppa
                else
                  let lf1 = List.append lf [(getProbPatternizationAppear ppa (List.length y)) + (findPatternToMatchListCreatorFixFloatListLoop lf loc )]  in -------------------loc might be in the wrong place may need to be loc  -1
                      findPatternToMatchListCreator loc (total+(getProbPatternizationAppear ppa (List.length y))+ findPatternToMatchListCreatorFixFloatListLoop lf1 loc ) siz list_ lf1 ppa

findPatternToMatchListCreatorFixFloatListLoop : List Float -> Int -> Float
findPatternToMatchListCreatorFixFloatListLoop lf loc =
  case lf of
    [] -> 0.0
    x::list_ -> if loc <= 0 then x
                else findPatternToMatchListCreatorFixFloatListLoop  list_ (loc - 1)



findPatternMatchCycle : List Float -> Float -> Int -- takes lsit of probabilities and returns
findPatternMatchCycle lf num =
  case lf of
    [] -> Debug.crash "badpat"
    x::list_ -> if x <= num then num
      else findPatternMatchCycle list_ num

findPatternCycle : PatternList -> Int -> List (List Int)
findPatternCycle pl num =
  case pl of
    [] -> [[]]
    x::list_ -> if num <= 0 then x
      else findPatternCycle list_ (num - 1)

findPatternToMatch : Float -> Seed -> List Float -> ProbPatternizationAppear-> PatternList -> Int -> List (List Int) --takes the size or remaining space and calculates the probs for mathcing each pattern in list float form
findPatternToMatch total se lf ppa pl siz =
      let (s2, rn) = floatGenerator se 0.0 total in
      let ans = findPatternMatchCycle lf rn in
      findPatternCycle pl ans



calculateSize : List Int -> Int -> Int
calculateSize li i =
  case li of
    [] -> i
    x::list_ -> calculateSize list_ (i + x)



findPatternToMatchMain :  Seed -> Int -> ProbPatternizationAppear -> PatternList -> (Int, List(List Int))
findPatternToMatchMain se siz ppa pat =
  let (lf,total) = findPatternToMatchListCreator siz 0.0 siz pat [] ppa in
  let thepat = findPatternToMatch total se lf ppa pat siz in
  case thepat of
      x::list_ ->  ((calculateSize x), thepat)

patternCreator : List Int ->  ProbOutChord -> ProbInChord ->
     ProbPatternizationAppear -> ProbRestChord -> Chord -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int   -- int is loc
  -> KeySignatureSuggestion -> ProbOutKey -> ProbInKey -> KeySignature -> ProbOctaveMelody -> Melody
  -> ChordR -> ProbCR
  -> MelodyR -> ProbMR
  -> ProbInterPatternization -> Seed -> Int -> PatternList-> Int ->PatternList
patternCreator liofptn poc pic ppa prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr pip se siz pat numpatsize =
  if siz <= 0 then case liofptn of
    [] -> pat
    x::list_ ->  patternCreator list_ poc pic ppa prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr pip se 0 pat x
  else
    if (numpatsize - siz) <= 4 then
        let tmr : MelodyR
            tmr = melodyRhythm 0 siz se mr pmr
        in
        let tcr : ChordR
            tcr = chordRhythm 0 siz se cr pcr in
        let tm : Melody
            tm = melodyCreatorMain kss se pok pik ks pom tmr m in
        let tc : Chord
            tc = chordCreatorMain poc pic se prestc c m mr paoc prc pac ptc loc cr kss in
        let pat1 = List.append pat [tmr, tcr, tm,tc] in
        let pat2 = liofptn patternCreator poc pic ppa prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr pip se (siz - tmr) pat1 numpatsize in
        pat2
    else
      let (s2, rn) = floatGenerator se 0.0 1.0 in
      if rn <= (getProbInterPatternization pip ) then
        let (siz1, pat2) =  findPatternToMatchMain se siz ppa pat in
          patternCreator liofptn poc pic ppa prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr pip se (siz + siz1) (List.append pat [pat2]) numpatsize


      else
        case numpatsize of
          0 -> pat
          _ ->  let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in   -----needs to be here becuase otherwise it would do melody for whole thing instead of 1 note
                let (lf, pt) = melodyRhythmHelper [] arra 0.0 pmr in
                let dn = melodyRhythmDecider arra in
                let (s2, rn) = floatGenerator se 0.0 pt in
                let nn =  (melodyRhythmDecider arra rn) in

                let tmr : MelodyR
                    tmr = melodyRhythm 0 nn se mr pmr in -- not sure how pat list lsit is going to work
                let tcr : ChordR
                    tcr = chordRhythm 0 nn se cr pcr  in
                let tmrtcr : List (List MelodyR) (List ChordR)
                    tmrtcr = List.append [tmr] [tcr] in                              ----------------------------------------------------logic erro
                let tm : Melody
                    tm = melodyCreatorMain kss se pok pik ks pom tmr m in
                let tc : Chord
                    tc = chordCreatorMain poc pic se prestc c m mr paoc prc pac ptc loc cr kss in
                let tmtc : List (List Melody)(List Chord)
                    tmtc = List.append [tm] [tc] in
                let pattem : List( List(List MelodyR) (List ChordR) (List Melody) (List Chord))
                    pattem = List.append tmrtcr [tmtc] in
                let pat1 = List.append pat pattem in
                patternCreator liofptn poc pic ppa prestc c paoc prc pac ptc loc kss pok pik ks pom m cr pcr mr pmr pip se (siz + nn) pat1 numpatsize

-}




--------------Melody Rhythm code section -------------------------------------------------------------
getLenFloatList : List Float -> Float -> Float
getLenFloatList lf f =
  case lf of
    [] -> f
    x::list_ -> getLenFloatList lf (f + x)

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
  let do = totalMR 0 mr in
  if do <= (maxnote - siz) then
    let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in
    let lf = melodyRhythmHelper [] arra pmr in
    let pt = getLenFloatList lf 0.0 in
    let dn = melodyRhythmDecider arra in
    let (s2, rn) = floatGenerator se 0.0 pt in
    let nn =  (melodyRhythmDecider arra rn) in
    let mr1 = List.append mr [nn] in
    melodyRhythm (siz + nn) maxnote se mr1 pmr
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
  let do = cycleThroughR mr 0 in
  if do <= (maxnote - siz) then
    let arra = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] in
    let lf = chordRhythmHelper [] arra pmr in
    let pt = getLenFloatList lf 0.0 in
    let dn = chordRhythmDecider arra in
    let (s2, rn) = floatGenerator se 0.0 pt in
    let cn = chordRhythmDecider arra rn in
    let mr1 = List.append mr [cn] in
    chordRhythm  (siz + cn) maxnote se mr1 pmr
  else
    let  hi = (cycleThroughR mr 0 ) - (maxnote - siz) in
    List.append mr [hi]


-------end code rhythm section-------------------------------------------

-------Start of Melody section -------------------------------------------

--chang List Int to KSS
keySignatureHelperhandler : KeySignatureSuggestion -> KeySignatureSuggestion
keySignatureHelperhandler kss =
  [0,2,4,5,7,9,11]



-- start with melodyCreatorMain creates list of probs calling probHelpMelodyMain


melodyCreatorMain : KeySignatureSuggestion -> Seed -> ProbOutKey -> ProbInKey -> KeySignature -> ProbOctaveMelody -> MelodyR -> Melody -> Melody--more here
melodyCreatorMain kss se pok pik ks pom mr m =
  case mr of
  [] -> m
--  [_] -> m -- precent root is final
--  [_,_] -> m --something else here
  x::list -> let  tem : List (Float)
                  tem = probHelpMelodyMain kss pok pik ks pom [] notes in
    let temp : List Float
        temp = (List.reverse tem) in
    let pt = melodySaveAss 0.0 temp in
    let (s2, rn) = floatGenerator se 0.0 pt in
    List.append m [melodyCreatorMainHelper tem rn]

melodySaveAss : Float -> List (Float) -> Float
melodySaveAss f lf =
  case lf of
    [] -> f
    x::list_ -> melodySaveAss x lf
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
    x::list_ -> let temp = List.append pmr [(((octaveControlMelody ks pom x) * (sameKeyMelodyProb kss1 pok pik ks x )) + (probMelodySaveAss pmr 0.0) ) ] in
     probHelpMelodyMain kss pok pik ks pom temp list_
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
    x::list_ -> if x == 0 then
                  if (temp % (x + 12)) == 0 then getInKey inkey
                    else sameKeyMelodyProb list_ outKey inkey key note
                else
                  if (temp % x) == 0 then getInKey inkey
                    else sameKeyMelodyProb list_ outKey inkey key note



--- start chord decision creator--------------------------------------------------------------------
--- Still Working On this part ---------------------------------------------------------------------

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




chordCreatorMain :ProbOutChord -> ProbInChord -> Seed ->ProbRestChord -> Chord -> Melody -> MelodyR ->  ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> ChordR -> KeySignatureSuggestion -> Chord --picks chord from prob initiate loc as 0
chordCreatorMain poc pic se prestc c m mr paoc prc pac ptc loc cr kss =
  let lc : List (List (Int))
      lc = [[]] in
  let lc1 : List (List (Int))
      lc1 = chordListCreatorLoop kss lc 1 0 1 0 in
  case cr of
    [] -> c
    x::list_ ->   let lf :List (Float)
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







chordListCreatorLoop : KeySignatureSuggestion -> List (List (Int)) ->  Int -> Int -> Int -> Int -> List (List (Int)) -- this may need to be revuiesd so all numbers after are 0 !!!!!!!!!!!!!!!!!!!!!
chordListCreatorLoop kss lc ac rc tc aoc =
  if aoc < 4 then chordListCreatorMain kss lc ac rc tc (aoc + 1)
    else if tc < 4 then chordListCreatorMain kss lc ac rc (tc + 1) aoc
        else if rc < 6 then chordListCreatorMain kss lc ac (rc + 1) tc aoc
            else if ac < 4 then chordListCreatorMain kss lc (ac + 1) rc tc aoc
  else lc

chordListCreatorMain: KeySignatureSuggestion -> List (List (Int)) ->  Int -> Int -> Int -> Int -> List (List (Int))
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
        [] -> ans
        x::kss -> chordListCreatorAppliedHelper (lo + 1) hi1 (ans + x) kss
      else  case kss of
        [] -> ans
        x::kss -> chordListCreatorAppliedHelper (lo + 1) hi (ans + x) kss

chordListCreatorApplied : KeySignatureSuggestion -> Int  -> Int
chordListCreatorApplied kss ac  =
  case ac of
    1 -> 0
    2 -> chordListCreatorAppliedHelper 0 4 0 kss
    3 -> chordListCreatorAppliedHelper 0 5 0 kss
    4 -> chordListCreatorAppliedHelper 0 7 0 kss
    _ ->    Debug.crash "tried get bad value"

chordListCreatorFindRoot : KeySignatureSuggestion -> Int -> Int
chordListCreatorFindRoot kss rc =
  chordListCreatorAppliedHelper 0 rc 0 kss

chordListCreatorAllElse : List (List (Int)) -> KeySignatureSuggestion -> Int -> Int -> Int -> Int -> Int -> Int -> List (List (Int)) -- fr = found root lots here
chordListCreatorAllElse lc kss fr app ac rc tc aoc  =
  if ac == 4 then
    case tc of
      1 -> case aoc of
          0 -> lc
          1 -> lc
          2 -> lc
          3 -> lc
          4 ->
            let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1)] in
            List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      2 -> case aoc of
          0 -> lc -- change to empty sets
          1 -> lc
          2 -> lc
          3 -> lc
          4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
                List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      3 -> case aoc of
          0 -> lc
          1 -> lc
          2 -> lc
          3 -> lc
          4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
                List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      4 -> case aoc of
          0 -> lc
          1 -> lc
          2 -> lc
          3 -> lc
          4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
                 List.append lc [temp]
          _ ->    Debug.crash "tried get bad value"
      _ ->    Debug.crash "tried get bad value"
  else
    case tc of
      1 -> case aoc of
        0 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss)] in
              List.append lc [temp]
        1 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss)] in
              List.append lc [temp]
        2 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss)] in
              List.append lc [temp]
        3 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) + 1)] in
                List.append lc [temp]
        4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1)] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      2 -> case aoc of
        0 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        1 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        2 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        3 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        4 -> let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+4) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      3 -> case aoc of
        0 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        1 -> lc
        2 -> lc
        3 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+2) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      4 -> case aoc of
        0 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), (app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        1 -> lc
        2 -> lc
        3 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss) + 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        4 ->  let temp = [(fr + app) , (app + chordListCreatorAppliedHelper 0 (rc+1) 0 kss), ((app + chordListCreatorAppliedHelper 0 (rc+3) 0 kss) - 1),  (app + chordListCreatorAppliedHelper 0 (rc+7) 0 kss)] in
              List.append lc [temp]
        _ ->    Debug.crash "tried get bad value"
      _ ->    Debug.crash "tried get bad value"









  --case len < 2
