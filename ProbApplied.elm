module ProbApplied exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {pac1 : Float
  ,pac4 : Float
  ,pac5 : Float
  ,pac7 : Float
  ,seed : Seed}

initialModel : Model
initialModel =
  {pac1 = 0
  ,pac4 = 0
  ,pac5 = 0
  ,pac7 = 0
  ,seed = Random.initialSeed 1010}

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
      (pac1n,s2) = floatCreator model.seed probability
      (pac4n,s3) = floatCreator s2 probability
      (pac5n,s4) = floatCreator s3 probability
      (pac7n,s5) = floatCreator s4 probability
    in
    ({model | pac1 = pac1n , pac4 = pac4n , pac5 = pac5n , pac7 = pac7n , seed = s5},Cmd.none)
