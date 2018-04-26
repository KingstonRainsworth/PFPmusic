module ProbType exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {proot : Float
  ,pseveth : Float
  ,pninth  : Float
  ,peleventh : Float}

initialModel : Model
initialModel =
  {proot = 0
  ,pseveth = 0
  ,pninth  = 0
  ,peleventh = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (model,Cmd.none)
