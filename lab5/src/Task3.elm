module Task3 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = ToggleDetails Int
    | ToggleFavorites


type alias Model =
    { images : List Image
    , onlyFavorites : Bool
    }


type alias Image =
    { id : Int
    , image : String
    , title : String
    , details : Bool
    , description : String
    , favorite : Bool
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { images =
        [ { id = 1
          , image = "https://www.w3schools.com/w3images/fjords.jpg"
          , title = "Fjords"
          , details = False
          , description = "Norway"
          , favorite = False
          }
        , { id = 2
          , image = "https://www.w3schools.com/w3images/forest.jpg"
          , title = "Forest"
          , details = False
          , description = "Sweden"
          , favorite = True
          }
        , { id = 3
          , image = "https://www.w3schools.com/w3images/mountains.jpg"
          , title = "Mountains"
          , details = False
          , description = "Finland"
          , favorite = False
          }
        ]
    , onlyFavorites = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDetails id ->
            { images = List.map (toggleDetails id) model.images, onlyFavorites = model.onlyFavorites }

        ToggleFavorites ->
            { images = model.images, onlyFavorites = not model.onlyFavorites }


toggleDetails : Int -> Image -> Image
toggleDetails id image =
    if image.id == id then
        { image | details = not image.details }

    else
        image


baseStyle : List (Attribute msg)
baseStyle =
    [ style "display" "grid"
    , style "grid-template-columns" "1"
    , style "justify-items" "center"
    ]


viewPhotoAndDetails : Bool -> Image -> Html Msg
viewPhotoAndDetails onlyFavorites image =
    div
        (baseStyle ++ [ style "display" (visibleFav image onlyFavorites) ])
        [ h1 []
            [ text image.title
            , text
                (if image.favorite then
                    " ★"

                 else
                    ""
                )
            ]
        , img [ src image.image, alt image.title ] []
        , button [ onClick (ToggleDetails image.id) ] [ text "Toggle Details" ]
        , p [ style "display" (visibleDetails image) ] [ text image.description ]
        ]


visibleDetails : Image -> String
visibleDetails image =
    if image.details then
        "inline"

    else
        "none"


visibleFav : Image -> Bool -> String
visibleFav image onlyFavorites =
    if onlyFavorites then
        if image.favorite then
            "grid"

        else
            "none"

    else
        "grid"


view : Model -> Html Msg
view model =
    div
        baseStyle
        [ h1 []
            [ text "Image Gallery" ]
        , button [ onClick ToggleFavorites ]
            [ text "Toggle Favorites" ]
        , div [] (List.map (viewPhotoAndDetails model.onlyFavorites) model.images)
        ]
