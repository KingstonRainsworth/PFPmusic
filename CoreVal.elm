module CoreVal exposing (..)

-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
--import Svg exposing (..)
--import Svg.Attributes exposing (..)
----------------------------------------------------------------------------

type alias Model =
  {
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
  ok = 0 ,
  ik = 0 , -- probablility that melody note is in key
  oc = 0 ,
  ic = 0 , -- probability chord includes notes from melody in chord
  prm = 0 ,
  prc = 0 ,
  sar = 0 ,
  ear = 0,
  pip = 0 ,
  pcmc = 0 ,
  cw = 0
}

type Msg =
  Set | Reset

{-subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg
        ]-}
