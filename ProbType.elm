module ProbType exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {proot : Float
  ,pseveth : Float
  ,pninth  : Float
  ,peleventh : Float}

initialModel : Model
initialModel =
  {proot = 0
  ,pseveth = 0
  ,pninth  = 0
  ,peleventh = 0}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Randomize Seed

probability : Generator Float
probability =
  Random.float 0 1

floatCreator : Seed -> Generator Float -> (Float,Seed)
floatCreator seed fg =
  let (f,s2) = Random.step fg seed in
  (f,s2)

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
  Set -> (model,Cmd.none)
  Reset -> (initialModel,Cmd.none)
  Randomize s->
    let
      seed = s
      (prootn,s2) = floatCreator seed probability
      (psevethn,s3) = floatCreator s2 probability
      (pninthn,s4) = floatCreator s3 probability
      (peleventhn,s5) = floatCreator s4 probability
    in
    ({model | proot = prootn , pseveth = psevethn , pninth = pninthn , peleventh = peleventhn },Cmd.none)
