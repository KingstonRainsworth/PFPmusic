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
  Set | Reset | Random

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
    Random ->
      let
        seedn = Tuple.second (floatCreator model.seed probability)
        okn = Tuple.first (floatCreator model.seed probability)
        ikn = Tuple.first (floatCreator model.seed probability)
        ocn = Tuple.first (floatCreator model.seed probability)
        icn = Tuple.first (floatCreator model.seed probability)
        prmn = Tuple.first (floatCreator model.seed probability)
        prcn = Tuple.first (floatCreator model.seed probability)
        sarn = Tuple.first (boolCreator model.seed coinFlip)
        earn = Tuple.first (boolCreator model.seed coinFlip)
        pipn = Tuple.first (floatCreator model.seed probability)
        pcmcm = Tuple.first (floatCreator model.seed probability)
        cwn = Tuple.first (floatCreator model.seed probability)
      in
      ({model | seed=seedn , ok=okn, ik=ikn , oc=ocn , ic=icn, prm=prmn , prc=prcn , sar=sarn , ear=earn , pip=pipn , pcmc=pcmcm , cw=cwn},Cmd.none)

view : Model -> Html Msg
view model =
  Html.div []
  [Html.p [] [Html.text (toString model)]]
