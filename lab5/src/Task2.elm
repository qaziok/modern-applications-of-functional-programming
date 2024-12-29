module Task2 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = ToggleDetails


main : Program () Bool Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Bool
init =
    False


update : Msg -> Bool -> Bool
update _ details =
    not details


baseStyle : List (Attribute msg)
baseStyle =
    [ style "display" "grid"
    , style "grid-template-columns" "1"
    , style "justify-items" "center"
    ]


view : Bool -> Html Msg
view details =
    div
        baseStyle
        [ h1 [] [ text "Image Gallery" ]
        , button [ onClick ToggleDetails ] [ text "Toggle Details" ]
        , viewPhotoAndDetails "https://www.w3schools.com/w3images/fjords.jpg" "Fjords" details
        , viewPhotoAndDetails "https://www.w3schools.com/w3images/forest.jpg" "Forest" details
        , viewPhotoAndDetails "https://www.w3schools.com/w3images/mountains.jpg" "Mountains" details
        ]


viewPhotoAndDetails : String -> String -> Bool -> Html msg
viewPhotoAndDetails image title details =
    div
        baseStyle
        [ h1 [] [ text title ]
        , img [ src image, alt title ] []
        , a
            [ href image
            , style "display"
                (if details then
                    "inline"

                 else
                    "none"
                )
            ]
            [ text image ]
        ]
