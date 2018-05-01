module ProbOctaveChord exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {     oc1 : Float --
  ,     oc2 : Float
  ,     oc3 : Float
  ,     oc4 : Float
  ,     oc5 : Float
  ,     seed : Seed   }

initialModel : Model
initialModel =
  {     oc1 = 0 --
  ,     oc2 = 0
  ,     oc3 = 0
  ,     oc4 = 0
  ,     oc5 = 0
  ,     seed = Random.initialSeed 43}

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
      (oc1n,s2) = floatCreator model.seed probability
      (oc2n,s3) = floatCreator s2 probability
      (oc3n,s4) = floatCreator s3 probability
      (oc4n,s5) = floatCreator s4 probability
      (oc5n,s6) = floatCreator s5 probability
    in
    ({model | oc1 = oc1n , oc2 = oc2n , oc3 = oc3n , oc4 = oc4n , oc5 = oc5n , seed = s6},Cmd.none)
