module ProbMR exposing (..)
-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {     cw : Float -- celody rhythc
  ,     c2 : Float
  ,     c3 : Float
  ,     c4 : Float
  ,     c5 : Float
  ,     c6 : Float
  ,     c7 : Float
  ,     c8 : Float
  ,     c9 : Float
  ,     c10 : Float
  ,     c11 : Float
  ,     c12 : Float
  ,     c13 : Float
  ,     c14 : Float
  ,     c15 : Float
  ,     c16 : Float     }

initialModel : Model
initialModel =
{     cw = 0 -- celody rhythc
,     c2 = 0
,     c3 = 0
,     c4 = 0
,     c5 = 0
,     c6 = 0
,     c7 = 0
,     c8 = 0
,     c9 = 0
,     c10 = 0
,     c11 = 0
,     c12 = 0
,     c13 = 0
,     c14 = 0
,     c15 = 0
,     c16 = 0     }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
    Set
  | Reset
