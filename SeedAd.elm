module SeedAd exposing (..)

import Random exposing (Generator, Seed)

type alias Model =
  {
    ogseed : Seed,
    seedcore : Seed,
    seedks : Seed,
    seednps : Seed,
    seedpao : Seed,
    seedpa : Seed,
    seedpcr : Seed,
    seedpmr : Seed,
    seedpoc : Seed,
    seedpocc : Seed,
    seedpap : Seed,
    seedpas : Seed,
    seedroot : Seed,
    seedtype : Seed
  }

initialModel : Model
initialModel =
  {
    ogseed = Random.initialSeed 0,
    seedcore = Random.initialSeed 0,
    seedks = Random.initialSeed 0,
    seednps = Random.initialSeed 0,
    seedpao = Random.initialSeed 0,
    seedpa = Random.initialSeed 0,
    seedpcr = Random.initialSeed 0,
    seedpmr = Random.initialSeed 0,
    seedpoc = Random.initialSeed 0,
    seedpocc = Random.initialSeed 0,
    seedpap = Random.initialSeed 0,
    seedpas = Random.initialSeed 0,
    seedroot = Random.initialSeed 0,
    seedtype = Random.initialSeed 0
  }

coinFlip : Generator Bool
coinFlip =
  Random.bool

boolCreator : Seed -> Generator Bool -> (Bool,Seed)
boolCreator seed bg =
  let (b,s2) = Random.step bg seed in
  (b,s2)

type Msg =
  GetSeed Int | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> (initialModel, Cmd.none)
    GetSeed iseed->
      if (model.ogseed == Random.initialSeed 0) then
      let
        ogseedn = Random.initialSeed iseed
        seedcoren = boolCreator ogseedn coinFlip |> Tuple.second
        seedksn = boolCreator seedcoren coinFlip |> Tuple.second
        seednpsn = boolCreator seedksn coinFlip |> Tuple.second
        seedpaon = boolCreator seednpsn coinFlip |> Tuple.second
        seedpan = boolCreator seedpaon coinFlip |> Tuple.second
        seedpcrn = boolCreator seedpan coinFlip |> Tuple.second
        seedpmrn = boolCreator seedpcrn coinFlip |> Tuple.second
        seedpocn = boolCreator seedpmrn coinFlip |> Tuple.second
        seedpoccn = boolCreator seedpocn coinFlip |> Tuple.second
        seedpapn = boolCreator seedpoccn coinFlip |> Tuple.second
        seedpasn = boolCreator seedpapn coinFlip |> Tuple.second
        seedrootn = boolCreator seedpasn coinFlip |> Tuple.second
        seedtypen = boolCreator seedrootn coinFlip |> Tuple.second
      in
      ({model | ogseed = seedtypen, seedcore = seedcoren , seedks = seedksn , seednps = seednpsn , seedpao = seedpaon, seedpa = seedpan , seedpcr = seedpcrn , seedpmr = seedpmrn , seedpoc = seedpocn , seedpocc = seedpoccn, seedpap = seedpapn , seedpas = seedpasn , seedroot = seedrootn, seedtype = seedtypen},Cmd.none)
      else
      let
        ogseedn = boolCreator model.ogseed coinFlip |> Tuple.second
        seedcoren = boolCreator ogseedn coinFlip |> Tuple.second
        seedksn = boolCreator seedcoren coinFlip |> Tuple.second
        seednpsn = boolCreator seedksn coinFlip |> Tuple.second
        seedpaon = boolCreator seednpsn coinFlip |> Tuple.second
        seedpan = boolCreator seedpaon coinFlip |> Tuple.second
        seedpcrn = boolCreator seedpan coinFlip |> Tuple.second
        seedpmrn = boolCreator seedpcrn coinFlip |> Tuple.second
        seedpocn = boolCreator seedpmrn coinFlip |> Tuple.second
        seedpoccn = boolCreator seedpocn coinFlip |> Tuple.second
        seedpapn = boolCreator seedpoccn coinFlip |> Tuple.second
        seedpasn = boolCreator seedpapn coinFlip |> Tuple.second
        seedrootn = boolCreator seedpasn coinFlip |> Tuple.second
        seedtypen = boolCreator seedrootn coinFlip |> Tuple.second
      in
      ({model | ogseed = seedtypen , seedcore = seedcoren , seedks = seedksn , seednps = seednpsn , seedpao = seedpaon, seedpa = seedpan , seedpcr = seedpcrn , seedpmr = seedpmrn , seedpoc = seedpocn , seedpocc = seedpoccn, seedpap = seedpapn , seedpas = seedpasn , seedroot = seedrootn, seedtype = seedtypen},Cmd.none)
      --ogseed = seedtypen,seedcore = seedcoren,seedks = seedksn,seednps = seednpsn,seedpao = seedpaon,seedpa = seedpan,seedpcr = seedpcrn,seedpmr = seedpmrn,seedpoc = seedpocn,seedpocc = seedpoccn,seedpap = seedpapn,seedpasn = seedpasnn,seedroot = seedrootn,seedtype = seedtypen},Cmd.none)
