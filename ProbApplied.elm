module ProbApplied exposing (..)
-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {pac1 : Float
  ,pac4 : Float
  ,pac5 : Float
  ,pac7 : Float}

initialModel : Model
initialModel =
  {pac1 = 0
  ,pac4 = 0
  ,pac5 = 0
  ,pac7 = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (model,Cmd.none)
