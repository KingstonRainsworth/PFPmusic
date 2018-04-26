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
  ,pz128 : Float}

initialModel : Model
initialModel =
  {pz4 = 0 --probability that a particualr pattern will apear in a pattern
  ,pz8 = 0
  ,pz12 = 0--odd number for 3/4 of 1/2 measure
  ,pz16 = 0
  ,pz32 = 0
  ,pz64 = 0
  ,pz128 = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (model,Cmd.none)
