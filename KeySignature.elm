module KeySignature exposing (..)
-- Set up package, not all are needed -----------------------------------------

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Keyboard
import Mouse
----------------------------------------------------------------------------

type alias Model =
  {key1 : Int
  ,key2 : Int
  ,key3 : Int
  ,key4 : Int
  ,key5 : Int
  ,key6 : Int
  ,key7 : Int
  ,key8 : Int
  ,key9 : Int
  ,key10 : Int
  ,key11 : Int
  ,key12 : Int
  ,seed : Seed}

initialModel : Model
initialModel ={ key1 = 0
  ,key2 = 0
  ,key3 = 0
  ,key4 = 0
  ,key5 = 0
  ,key6 = 0
  ,key7 = 0
  ,key8 = 0
  ,key9 = 0
  ,key10 = 0
  ,key11 = 0
  ,key12 = 0
  ,seed = Random.initialSeed 19}

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Randomize

intGenerator : Generator Int
intGenerator =
  Random.int 0 5

intCreator : Seed -> Generator Int -> (Int,Seed)
intCreator seed ig =
  let (i,s2) = Random.step ig seed in
  (i,s2)

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
  Set -> (model,Cmd.none)
  Reset -> (initialModel,Cmd.none)
  Randomize ->
    let
      (key1n,s2) = intCreator model.seed intGenerator
      (key2n,s3) = intCreator s2 intGenerator
      (key3n,s4) = intCreator s3 intGenerator
      (key4n,s5) = intCreator s4 intGenerator
      (key5n,s6) = intCreator s5 intGenerator
      (key6n,s7) = intCreator s6 intGenerator
      (key7n,s8) = intCreator s7 intGenerator
      (key8n,s9) = intCreator s8 intGenerator
      (key9n,s10) = intCreator s9 intGenerator
      (key10n,s11) = intCreator s10 intGenerator
      (key11n,s12) = intCreator s11 intGenerator
      (key12n,s13) = intCreator s12 intGenerator
    in
    ({model | key1 = key1n , key2 = key2n , key3 = key3n , key4 = key4n , key5 = key5n , key6 = key6n , key7 = key7n , key8 = key8n , key9 = key9n , key10 = key10n , key11 = key11n , key12 = key12n , seed = s13},Cmd.none)
