module ProbRoot exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  { prc1 : Float
  , prc2 : Float
  , prc3 : Float
  , prc4 : Float
  , prc5 : Float
  , prc6 : Float
  , prc7 : Float }

initialModel : Model
initialModel =
  { prc1 = 0
  , prc2 = 0
  , prc3 = 0
  , prc4 = 0
  , prc5 = 0
  , prc6 = 0
  , prc7 = 0  }

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
      seed = Random.initialSeed 9713
      (prc1n,s2) = floatCreator seed probability
      (prc2n,s3) = floatCreator s2 probability
      (prc3n,s4) = floatCreator s3 probability
      (prc4n,s5) = floatCreator s4 probability
      (prc5n,s6) = floatCreator s5 probability
      (prc6n,s7) = floatCreator s6 probability
      (prc7n,s8) = floatCreator s7 probability
    in
    ({model | prc1 = prc1n , prc2 = prc2n , prc3 = prc3n , prc4 = prc4n , prc5 = prc5n , prc6 = prc6n , prc7 = prc7n },Cmd.none)
