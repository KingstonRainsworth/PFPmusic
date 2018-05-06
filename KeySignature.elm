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
  {key1 : Bool
  ,key2 : Bool
  ,key3 : Bool
  ,key4 : Bool
  ,key5 : Bool
  ,key6 : Bool
  ,key7 : Bool
  ,key8 : Bool
  ,key9 : Bool
  ,key10 : Bool
  ,key11 : Bool
  ,key12 : Bool
  }

initialModel : Model
initialModel ={ key1 = False
  ,key2 = False
  ,key3 = False
  ,key4 = False
  ,key5 = False
  ,key6 = False
  ,key7 = False
  ,key8 = False
  ,key9 = False
  ,key10 = False
  ,key11 = False
  ,key12 = False
  }

init : (Model,Cmd Msg)
init = (initialModel, Cmd.none)

type Msg =
  Set | Reset | Randomize

boolCreator : Seed -> Generator Bool -> (Bool,Seed)
boolCreator seed bg =
  let (b,s2) = Random.step bg seed in
  (b,s2)

coinFlip : Generator Bool
coinFlip =
  Random.bool

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
  Set -> (model,Cmd.none)
  Reset -> (initialModel,Cmd.none)
  Randomize ->
    let
      seed = Random.initialSeed 132
      (key1n,s2) = boolCreator seed coinFlip
      (key2n,s3) = boolCreator s2 coinFlip
      (key3n,s4) = boolCreator s3 coinFlip
      (key4n,s5) = boolCreator s4 coinFlip
      (key5n,s6) = boolCreator s5 coinFlip
      (key6n,s7) = boolCreator s6 coinFlip
      (key7n,s8) = boolCreator s7 coinFlip
      (key8n,s9) = boolCreator s8 coinFlip
      (key9n,s10) = boolCreator s9 coinFlip
      (key10n,s11) = boolCreator s10 coinFlip
      (key11n,s12) = boolCreator s11 coinFlip
      (key12n,s13) = boolCreator s12 coinFlip
    in
    ({model | key1 = key1n , key2 = key2n , key3 = key3n , key4 = key4n , key5 = key5n , key6 = key6n , key7 = key7n , key8 = key8n , key9 = key9n , key10 = key10n , key11 = key11n , key12 = key12n},Cmd.none)
