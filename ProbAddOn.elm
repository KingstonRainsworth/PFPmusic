module ProbAddOn exposing (..)
-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  { pnao : Float
  , psus2 : Float
  , psus4 : Float
  , pAug  : Float
  , pDim  : Float }

initialModel : Model
initialModel =
  { pnao = 0
  , psus2 = 0
  , psus4 = 0
  , pAug  = 0
  , pDim  = 0 }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Random

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (model,Cmd.none)
