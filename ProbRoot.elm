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
  , prc7 = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random
