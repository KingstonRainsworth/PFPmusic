module ProbCR exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model ={
       cw : Float -- celody rhythc
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
initialModel = {
        cw = 0 -- celody rhythc
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
      seed = Random.initialSeed 75
      (cwn,s2) = floatCreator seed probability
      (c2n,s3) = floatCreator s2 probability
      (c3n,s4) = floatCreator s3 probability
      (c4n,s5) = floatCreator s4 probability
      (c5n,s6) = floatCreator s5 probability
      (c6n,s7) = floatCreator s6 probability
      (c7n,s8) = floatCreator s7 probability
      (c8n,s9) = floatCreator s8 probability
      (c9n,s10) = floatCreator s9 probability
      (c10n,s11) = floatCreator s10 probability
      (c11n,s12) = floatCreator s11 probability
      (c12n,s13) = floatCreator s12 probability
      (c13n,s14) = floatCreator s13 probability
      (c14n,s15) = floatCreator s14 probability
      (c15n,s16) = floatCreator s15 probability
      (c16n,s17) = floatCreator s16 probability
    in
    ({model | cw = cwn , c2 = c2n , c3 = c3n , c4 = c4n , c5 = c5n , c6 = c6n , c7 = c7n , c8 = c8n , c9 = c9n , c10 = c10n , c11 = c11n , c12 = c12n , c13 = c13n , c14 = c14n , c15 = c15n , c16 = c16n},Cmd.none)
