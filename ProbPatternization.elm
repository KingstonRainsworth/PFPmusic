module ProbPatternization exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {pz4 : Float --probability that a particualr pattern will apear in a pattern
  ,pz8 : Float
  ,pz12 : Float--odd number for 3/4 of 1/2 measure
  ,pz16 : Float
  ,pz32 : Float
  ,pz64 : Float
  ,pz128 : Float }

initialModel : Model
initialModel =
  {pz4 = 0 --probability that a particualr pattern will apear in a pattern
  ,pz8 = 0
  ,pz12 = 0--odd number for 3/4 of 1/2 measure
  ,pz16 = 0
  ,pz32 = 0
  ,pz64 = 0
  ,pz128 = 0 }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Randomize

probability : Generator Float
probability =
  Random.float 0 1
floatCreator : Seed -> Generator Float -> (Float,Seed)
floatCreator seed fg =
  let (f,s2) = Random.step fg seed in
  (f,s2)

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
  Set -> (model,Cmd.none)
  Reset -> (initialModel,Cmd.none)
  Randomize ->
    let
      seed = Random.initialSeed 7
      (pz4n,s2) = floatCreator seed probability
      (pz8n,s3) = floatCreator s2 probability
      (pz12n,s4) = floatCreator s3 probability
      (pz16n,s5) = floatCreator s4 probability
      (pz32n,s6) = floatCreator s5 probability
      (pz64n,s7) = floatCreator s6 probability
      (pz128n,s8) = floatCreator s7 probability
    in
    ({model | pz4=pz4n , pz8=pz8n , pz12=pz12n , pz16=pz16n , pz32=pz32n , pz64=pz64n , pz128=pz128n },Cmd.none)
