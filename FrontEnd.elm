module FrontEnd exposing (..)

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

--main : Program Never Model Msg
main =
    Html.beginnerProgram
      { model  = initialModel
      , view = view
      , update = update
      --, subscriptions = subscriptions
      }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel = {value1 = 0, value2 = 0, value3 = 0, value4 =0}

{-subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg
        ]-}

type alias Model =
  { value1 : Float
    , value2 : Float
    , value3 : Float
    , value4 : Float
  }

type Msg
  = Prop1 String |
    Prop2 String |
    Prop3 String |
    Prop4 String |
    Reset | Generate |
    MouseMsg Mouse.Position |
    KeyMsg Keyboard.KeyCode

update : Msg -> Model -> Model
update msg model = case msg of
  --Prop1 prop1 -> ({model | value1 = Result.withDefault 0 (String.toFloat prop1)}, Cmd.none)
  Prop1 prop1 -> {model | value1 = Result.withDefault 0 (String.toFloat prop1)}
  Prop2 prop2 -> {model | value2 = Result.withDefault 0 (String.toFloat prop2)}
  Prop3 prop3 -> {model | value3 = Result.withDefault 0 (String.toFloat prop3)}
  Prop4 prop4 -> {model | value4 = Result.withDefault 0 (String.toFloat prop4)}
  Reset -> initialModel
  _ -> Debug.crash "TODO"

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "value1", onInput Prop1 ] []
    , input [ type_ "text", placeholder "value2", onInput Prop2 ] []
    , input [ type_ "text", placeholder "value3", onInput Prop3 ] []
    , input [ type_ "text", placeholder "value4", onInput Prop4 ] []
    , button [ onClick Reset ] [ text "Reset" ]
    , button [onClick Generate] [text "Generate"]
    , viewCheck model
    ]

viewCheck : Model -> Html Msg
viewCheck model =
  let v1 = model.value1 in
  let v2 = model.value2 in
  let v3 = model.value3 in
  let v4 = model.value4 in
  if (v1 < 0 || v1 > 1 || v2 < 0 || v2 > 1 || v3 < 0 || v3 > 1 || v4 < 0  || v4 > 1) then
    div [ style [("color", "red")] ] [ text "input not a probability" ]
  else
    div [ style [("color", "green")]] [text "OK"]
