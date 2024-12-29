module Task1 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


baseStyle : List (Attribute msg)
baseStyle =
    [ style "display" "grid"
    , style "grid-template-columns" "1"
    , style "justify-items" "center"
    ]


viewPhotoAndDetails : String -> String -> Html.Html msg
viewPhotoAndDetails image title =
    div
        baseStyle
        [ h1 [] [ text title ]
        , img [ src image, alt title ] []
        , a [ href image ] [ text image ]
        ]


main : Html.Html msg
main =
    div
        baseStyle
        [ h1 [] [ text "Image Gallery" ]
        , viewPhotoAndDetails "https://www.w3schools.com/w3images/fjords.jpg" "Fjords"
        , viewPhotoAndDetails "https://www.w3schools.com/w3images/forest.jpg" "Forest"
        , viewPhotoAndDetails "https://www.w3schools.com/w3images/mountains.jpg" "Mountains"
        ]
