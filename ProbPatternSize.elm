module ProbPatternSize exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {p4 : Float --probability that a particualr pattern will apear
  ,p8 : Float
  ,p12 : Float--odd number for 3/4 of 1/2 measure
  ,p16 : Float
  ,p32 : Float
  ,p64 : Float
  ,p128 : Float
  ,p256 : Float
  ,seed : Seed
  }

initialModel : Model
initialModel =
  {p4 = 0 --probability that a particualr pattern will apear
  ,p8 = 0
  ,p12 = 0--odd number for 3/4 of 1/2 measure
  ,p16 = 0
  ,p32 = 0
  ,p64 = 0
  ,p128 = 0
  ,p256 = 0
  ,seed = Random.initialSeed 1
  }

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
      (p4n,s2) = floatCreator model.seed probability
      (p8n,s3) = floatCreator s2 probability
      (p12n,s4) = floatCreator s3 probability
      (p16n,s5) = floatCreator s4 probability
      (p32n,s6) = floatCreator s5 probability
      (p64n,s7) = floatCreator s6 probability
      (p128n,s8) = floatCreator s7 probability
      (p256n,s9) = floatCreator s8 probability
    in
    ({model | p4=p4n , p8=p8n , p12=p12n , p16=p16n , p32=p32n , p64=p64n , p128=p128n , p256=p256n, seed = s9},Cmd.none)
