module SeedAd exposing (..)

import Html exposing (..)
import Time exposing (..)
import Task exposing (..)
import Random exposing (Generator, Seed)

type alias Model = Float

type Msg
    = OnTime Time

initialModel = (0,getTime)

update : Msg -> Model -> (Model, Cmd msg)
update (OnTime t) model =
    (t, Cmd.none)

getTime =
    Task.perform OnTime Time.now

getTimeF : Model -> Int
getTimeF = round(initialModel)

view: Model -> Html Msg
view model =
    Html.div [] [Html.text (toString model)]

main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
