module ProbAddOn exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
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
  , pDim  : Float
  , seed : Seed }

initialModel : Model
initialModel =
  { pnao = 0
  , psus2 = 0
  , psus4 = 0
  , pAug  = 0
  , pDim  = 0
  , seed = Random.initialSeed 885 }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Randomize

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
  Randomize ->
    let
      (pnaon,s2) = floatCreator model.seed probability
      (psus2n,s3) = floatCreator s2 probability
      (psus4n,s4) = floatCreator s3 probability
      (pAugn,s5) = floatCreator s4 probability
      (pDimn,s6) = floatCreator s5 probability
    in
    ({model | pnao = pnaon , psus2 = psus2n , psus4 = psus4n , pAug = pAugn , pDim = pDimn , seed = s6},Cmd.none)
