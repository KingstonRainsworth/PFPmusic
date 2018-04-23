module ProbMR exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {     mw : Float -- melody rhythm
  ,     m2 : Float
  ,     m3 : Float
  ,     m4 : Float
  ,     m5 : Float
  ,     m6 : Float
  ,     m7 : Float
  ,     m8 : Float
  ,     m9 : Float
  ,     m10 : Float
  ,     m11 : Float
  ,     m12 : Float
  ,     m13 : Float
  ,     m14 : Float
  ,     m15 : Float
  ,     m16 : Float     }

initialModel : Model
initialModel =
{     mw = 0 -- melody rhythm
,     m2 = 0
,     m3 = 0
,     m4 = 0
,     m5 = 0
,     m6 = 0
,     m7 = 0
,     m8 = 0
,     m9 = 0
,     m10 = 0
,     m11 = 0
,     m12 = 0
,     m13 = 0
,     m14 = 0
,     m15 = 0
,     m16 = 0     }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random
