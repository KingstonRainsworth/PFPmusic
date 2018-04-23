module ProbPatternSize exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {p4 : Float --probability that a particualr pattern will apear
  ,p8 : Float
  ,p12 : Float--odd number for 3/4 of 1/2 measure
  ,p16 : Float
  ,p32 : Float
  ,p64 : Float
  ,p128 : Float
  ,p256 : Float
  }

initialModel : Model
initialModel =
  {p4 = 0 --probability that a particualr pattern will apear
  ,p8 = 0
  ,p12 = 0--odd number for 3/4 of 1/2 measure
  ,p16 = 0
  ,p32 = 0
  ,p64 = 0
  ,p128 = 0
  ,p256 = 0
  }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random
