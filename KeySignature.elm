module KeySignature exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import MusicMakerProject as MMP
import Keyboard
import Mouse
----------------------------------------------------------------------------
type alias Model = MMP.KeySignature

initialModel : Model
initialModel = MMP.A

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Randomize

intCreator : Seed -> Generator Int -> (Int,Seed)
intCreator seed ig =
  let (i,s2) = Random.step ig seed in (i,s2)

intGen : Generator Int
intGen =
  Random.int 1 12

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
  Set -> (model,Cmd.none)
  Reset -> (initialModel,Cmd.none)
  Randomize ->
    let
      seed = Random.initialSeed 132
      (key1n,s2) = intCreator seed intGen
    in case key1n of
      1 -> (MMP.A,Cmd.none)
      2 -> (MMP.AS,Cmd.none)
      3 -> (MMP.B,Cmd.none)
      4 -> (MMP.C,Cmd.none)
      5 -> (MMP.CS,Cmd.none)
      6 -> (MMP.D,Cmd.none)
      7 -> (MMP.DS,Cmd.none)
      8 -> (MMP.E,Cmd.none)
      9 -> (MMP.F,Cmd.none)
      10 -> (MMP.FS,Cmd.none)
      11 -> (MMP.G,Cmd.none)
      12 -> (MMP.GS,Cmd.none)
      _ -> (MMP.A,Cmd.none)
