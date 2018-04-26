module CoreVal exposing (..)

-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
--import Svg exposing (..)
--import Svg.Attributes exposing (..)
----------------------------------------------------------------------------

type alias Model =
  {
    seed : Seed ,
    ok : Float ,
    ik : Float , -- probablility that melody note is in key
    oc : Float ,
    ic : Float , -- probability chord includes notes from melody in chord
    prm : Float ,
    prc : Float ,
    sar : Bool ,
    ear : Bool ,
    pip : Float ,
    pcmc : Float ,
    cw : Float
  }

initialModel : Model
initialModel = {
  seed = Random.initialSeed 5,
  ok = 0 ,
  ik = 0 , -- probablility that melody note is in key
  oc = 0 ,
  ic = 0 , -- probability chord includes notes from melody in chord
  prm = 0 ,
  prc = 0 ,
  sar = False ,
  ear = False ,
  pip = 0 ,
  pcmc = 0 ,
  cw = 0}

type Msg =
  Set | Reset | Randomize

probability : Generator Float
probability =
  Random.float 0 1

coinFlip : Generator Bool
coinFlip =
  Random.bool

boolCreator : Seed -> Generator Bool -> (Bool,Seed)
boolCreator seed bg =
  let (b,s2) = Random.step bg seed in
  (b,s2)
floatCreator : Seed -> Generator Float -> (Float,Seed)
floatCreator seed fg =
  let (f,s2) = Random.step fg seed in
  (f,s2)
{-subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg
        ]-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Set -> (model,Cmd.none)
    Reset -> (initialModel, Cmd.none)
    Randomize ->
      let
        (okn,s2) =  (floatCreator model.seed probability)
        (ikn,s3) =  (floatCreator s2 probability)
        (ocn,s4) =  (floatCreator s3 probability)
        (icn,s5) =  (floatCreator s4 probability)
        (prmn,s6) =  (floatCreator s5 probability)
        (prcn,s7) =  (floatCreator s6 probability)
        (sarn,s8) =  (boolCreator s7 coinFlip)
        (earn,s9) =  (boolCreator s8 coinFlip)
        (pipn,s10) =  (floatCreator s9 probability)
        (pcmcm,s11) =  (floatCreator s10 probability)
        (cwn,s12) =  (floatCreator s11 probability)
      in
      ({model | seed=s12 , ok=okn, ik=ikn , oc=ocn , ic=icn, prm=prmn , prc=prcn , sar=sarn , ear=earn , pip=pipn , pcmc=pcmcm , cw=cwn},Cmd.none)

{-view : Model -> Html Msg
view model =
  Html.div []
  [Html.p [] [Html.text (toString model)]]-}
