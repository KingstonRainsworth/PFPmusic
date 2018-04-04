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
type KeySignature = A|AS|B|C|CS|D|DS|E|F|FS|G|GS  --12  no flats only sharps or pick your own
--type alias Key = { key : KeySignature}
type alias KeySignatureSuggestion = {ksp : List Int}
type alias ProbMR = { mw : Float -- melody rhythm
          ,     mh : Float
          ,     mq : Float
          ,     me : Float
          ,     ms : Float}

type alias ProbCR = { cw : Float -- chord rhythm
          ,     ch : Float
          ,     cq : Float}

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


type Dur = W|H|Q|EI|S

type alias Note = Int

type alias ChordR = List Dur

type alias Melody = List Note

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

calculateDur : Dur -> Int
calculateDur d =
  case d of
    W -> 16
    H -> 8
    Q -> 4
    EI -> 2
    S -> 1

cycleThroughR : MelodyR -> Int -> Int
cycleThroughR list num =
  case list of
    [] -> num
  --  [x] -> cycleThroughR [] (calculateDur x + num)
  --  [x, y] -> cycleThroughR y (calculateDur x + num)
    x::list -> cycleThroughR list ( calculateDur x + num)

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


getProbMR : Dur -> ProbMR -> Float
getProbMR y x = case y of
  W -> x.mw
  H -> x.mh
  Q -> x.mq
  EI -> x.me
  S -> x.ms

melodyRhythm : Seed -> MelodyR ->ProbMR -> MelodyR
melodyRhythm se mr pmr =
  let do = cycleThroughR mr 0 in
  if do < 240 then
    let pw = getProbMR W pmr in
    let ph = getProbMR H pmr in
    let pq = getProbMR Q pmr in
    let pei = getProbMR EI pmr in
    let ps = getProbMR S pmr in
    let pt = pw + ph + pq + pei + ps in
    let (s2, rn) = floatGenerator se 0.0 pt in
    if rn < pw then List.append mr [W]
      else if rn < ph then List.append mr [H]
        else if rn < pq then List.append mr [Q]
          else if rn < pei then List.append mr [EI]
            else List.append mr [S]

  else
    let  hi = (cycleThroughR mr 0 )- 240 in
    List.append mr [hi]
    -- remove last number calculate whats needed and put that in

--------------end of Melody Rhythm code Section ---------------------------------

--------------start of Chord Rhythm code section---------------------------------
getProbCR : Dur -> ProbCR -> Float
getProbCR y x = case y of
  W -> x.cw
  H -> x.ch
  Q -> x.cq

chordRhythm : Seed -> ChordR -> ProbCR -> ChordR
chordRhythm se cr pcr =
    let pw = getProbCR W pcr in
    let ph = getProbCR H pcr in
    let pq = getProbCR Q pcr in
    let pt = pw + ph + pq in -- not goin to work needs to be tail
    let (s2, rn) = floatGenerator se 0.0 pt in
    if rn < pw then List.append cr [W]
      else if rn < ph then List.append cr [H]
        else List.append cr [Q]

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


chordCreatorMain :ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> ChordR -> KeySignatureSuggestion -> Chord
chordCreatorMain paoc prc pac ptc cr kss =
  let lc = [[]] in
  let lc1 = chordListCreatorLoop kss lc 1 0 1 0 in
  chordProbMain paoc prc pac ptc cr lc1



chordProbMain :ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord ->  Int -> Int -> Int -> Int -> List Float -> ChordR -> List List Int -> List Float
chordProbMain paoc prc pac ptc ac rc tc aoc lf cr lc = case cr of
   [] -> lf
   x::cr -> chordProbMain lc paoc prc pac pt (List.append lf (chordProbHelper ))

chordProbHelper :List List Int -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> Int -> Int -> Int -> Float
chordProbHelper lc paoc prc pac ptc ac rc tc aoc =
  case lc of
    [] ->
      let p = (getProbTypeChord tc ptc )* (getProbAppliedChord ac pac) * (getProbRootChord rc prc) * (getProbAddOnChord aoc paoc) in
      p -- dont delete

chordProbListGenerator :KeySignatureSuggestion -> List Float -> List List Int -> ProbAddOnChord -> ProbRootChord -> ProbAppliedChord -> ProbTypeChord -> Int -> Int -> Int -> Int -> List Float
chordProbListGenerator kss lc paoc prc pac ptc ac rc tc aoc =
  case lc of
    [] -> lf
    x::lc -> case x of
      [] -> chordProbListGenerator lc paoc prc pac ptc ac rc tc aoc
      [_,_,_] -> chordProbLoop kss x

--this is where i left off-------------- chordProbFindMelodyNotes need to check when the chord and notes overlap then chordProbLoop
-- checks the chord its working on to see if it has, multiply one in for each note.


{-chordProbFindMelodyNotes :
chordProbFindMelodyNotes-}

chordProbLoop : ProbOutChord -> ProbInChord -> KeySignatureSuggestion -> List Int -> Float
chordProbLoop poc pic kss x =
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



chordListCreatorLoop : KeySignatureSuggestion -> List List Int ->  Int -> Int -> Int -> Int -> List List Int
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
