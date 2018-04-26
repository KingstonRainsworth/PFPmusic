module Numpatsize exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {n4 : Int --number of a particualr pattern to make
  ,n8 : Int
  ,n12 : Int
  ,n16 : Int
  ,n32 : Int
  ,n64 : Int
  ,n128 : Int
  ,n256 : Int}

initialModel : Model
initialModel =
  {n4 = 0 --number of a particualr pattern to make
  ,n8 = 0
  ,n12 = 0
  ,n16 = 0
  ,n32 = 0
  ,n64 = 0
  ,n128 = 0
  ,n256 = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (model,Cmd.none)
