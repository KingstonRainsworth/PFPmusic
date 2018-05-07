module ResultS exposing (..)

import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)

type alias Model =
  {
    chordr : List Int ,
    melody : List Int ,
    chord : List (List Int) ,
    melodyr : List Int
  }

initialModel : Model
initialModel = {
  chordr = [],
  melody = [],
  chord = [],
  melodyr = [] }

type Msg =
  Set (List Int, List Int, List (List Int), List Int) | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Reset -> (initialModel, Cmd.none)
  Set (cr,m,c,mr) ->
    ({model | chordr = cr, melody = m, chord = c, melodyr = mr},Cmd.none)
