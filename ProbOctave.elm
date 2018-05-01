module ProbOctave exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {     o1 : Float --
  ,     o2 : Float
  ,     o3 : Float
  ,     o4 : Float
  ,     o5 : Float
  ,     o6 : Float
  ,     seed : Seed     }

initialModel : Model
initialModel =
  {     o1 = 0 --
  ,     o2 = 0
  ,     o3 = 0
  ,     o4 = 0
  ,     o5 = 0
  ,     o6 = 0
  ,     seed = Random.initialSeed 101}

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
      (o1n,s2) = floatCreator model.seed probability
      (o2n,s3) = floatCreator s2 probability
      (o3n,s4) = floatCreator s3 probability
      (o4n,s5) = floatCreator s4 probability
      (o5n,s6) = floatCreator s5 probability
      (o6n,s7) = floatCreator s6 probability
    in
    ({model | o1 = o1n , o2 = o2n , o3 = o3n , o4 = o4n , o5 = o5n , o6 = o6n , seed = s7},Cmd.none)
