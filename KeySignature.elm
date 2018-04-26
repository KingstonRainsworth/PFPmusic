module KeySignature exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {key1 : Int
  ,key2 : Int
  ,key3 : Int
  ,key4 : Int
  ,key5 : Int
  ,key6 : Int
  ,key7 : Int
  ,key8 : Int
  ,key9 : Int
  ,key10 : Int
  ,key11 : Int
  ,key12 : Int}

initialModel : Model
initialModel ={ key1 = 0
  ,key2 = 0
  ,key3 = 0
  ,key4 = 0
  ,key5 = 0
  ,key6 = 0
  ,key7 = 0
  ,key8 = 0
  ,key9 = 0
  ,key10 = 0
  ,key11 = 0
  ,key12 = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (model,Cmd.none)
