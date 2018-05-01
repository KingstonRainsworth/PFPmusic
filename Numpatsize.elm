module Numpatsize exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {n4 : Int --number of a particualr pattern to make
  ,n8 : Int
  ,n12 : Int
  ,n16 : Int
  ,n32 : Int
  ,n64 : Int
  ,n128 : Int
  ,n256 : Int
  ,seed : Seed}

initialModel : Model
initialModel =
  {n4 = 0 --number of a particualr pattern to make
  ,n8 = 0
  ,n12 = 0
  ,n16 = 0
  ,n32 = 0
  ,n64 = 0
  ,n128 = 0
  ,n256 = 0
  ,seed = Random.initialSeed 8}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

intGenerator : Generator Int
intGenerator =
  Random.int 0 5
intCreator : Seed -> Generator Int -> (Int,Seed)
intCreator seed ig =
  let (i,s2) = Random.step ig seed in
  (i,s2)

type Msg =
  Set | Reset | Randomize

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
  Set -> (model,Cmd.none)
  Reset -> (initialModel,Cmd.none)
  Randomize ->
    let
      (n4n,s2) = intCreator model.seed intGenerator
      (n8n,s3) = intCreator s2 intGenerator
      (n12n,s4) = intCreator s3 intGenerator
      (n16n,s5) = intCreator s4 intGenerator
      (n32n,s6) = intCreator s5 intGenerator
      (n64n,s7) = intCreator s6 intGenerator
      (n128n,s8) = intCreator s7 intGenerator
      (n256n,s9) = intCreator s8 intGenerator
    in
    ({model | n4=n4n , n8=n8n , n12=n12n , n16=n16n , n32=n32n , n64=n64n , n128=n128n , n256=n256n, seed = s9},Cmd.none)
