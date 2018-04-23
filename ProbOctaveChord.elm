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
  ,     oc5 : Float     }

initialModel : Model
initialModel =
  {     oc1 = 0 --
  ,     oc2 = 0
  ,     oc3 = 0
  ,     oc4 = 0
  ,     oc5 = 0    }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random
