module KeySignature exposing (..)
-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {Key1 : Int
  ,Key2 : Int
  ,Key3 : Int
  ,Key4 : Int
  ,Key5 : Int
  ,Key6 : Int
  ,Key7 : Int
  ,Key8 : Int
  ,Key9 : Int
  ,Key10 : Int
  ,Key11 : Int
  ,Key12 : Int}

initialModel : Model
initialModel =
  {Key1 = 0
  ,Key2 = 0
  ,Key3 = 0
  ,Key4 = 0
  ,Key5 = 0
  ,Key6 = 0
  ,Key7 = 0
  ,Key8 = 0
  ,Key9 = 0
  ,Key10 = 0
  ,Key11 = 0
  ,Key12 = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
    Set
  | Reset
