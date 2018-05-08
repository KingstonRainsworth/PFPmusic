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
  ,     m16 : Float      }

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
      (mwn,s2) = floatCreator seed probability
      (m2n,s3) = floatCreator s2 probability
      (m3n,s4) = floatCreator s3 probability
      (m4n,s5) = floatCreator s4 probability
      (m5n,s6) = floatCreator s5 probability
      (m6n,s7) = floatCreator s6 probability
      (m7n,s8) = floatCreator s7 probability
      (m8n,s9) = floatCreator s8 probability
      (m9n,s10) = floatCreator s9 probability
      (m10n,s11) = floatCreator s10 probability
      (m11n,s12) = floatCreator s11 probability
      (m12n,s13) = floatCreator s12 probability
      (m13n,s14) = floatCreator s13 probability
      (m14n,s15) = floatCreator s14 probability
      (m15n,s16) = floatCreator s15 probability
      (m16n,s17) = floatCreator s16 probability
    in
    ({model | mw = mwn , m2 = m2n , m3 = m3n , m4 = m4n , m5 = m5n , m6 = m6n , m7 = m7n , m8 = m8n , m9 = m9n , m10 = m10n , m11 = m11n , m12 = m12n , m13 = m13n , m14 = m14n , m15 = m15n , m16 = m16n},Cmd.none)
