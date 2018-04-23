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
  ,     o6 : Float    }

initialModel : Model
initialModel =
  {     o1 = 0 --
  ,     o2 = 0
  ,     o3 = 0
  ,     o4 = 0
  ,     o5 = 0
  ,     o6 = 0    }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random
