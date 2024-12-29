module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)





main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



type alias Model =
    { greet : String }


init : Model
init =
    { greet = "Hello, World!" }



-- UPDATE


type Msg
    = Hello String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Hello greet ->
            { model | greet = greet }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.greet ]
        , button [ onClick (Hello "Hello, again!") ] [ text "Click me!" ]
        ]
