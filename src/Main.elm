module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, h2, img, p, text)
import Html.Attributes exposing (class, src)



---- MODEL ----


type alias Model =
    { navItems : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { navItems = [ "News", "Jobs", "Settings" ]
      }
    , Cmd.none
    )



---- HELPER FUNCTIONS -----


navItems : String -> Html Msg
navItems item =
    p [ class "nav-item" ] [ text item ]



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "navbar" ]
            [ h1 [] [ text "Hack Yer News" ]
            , div [ class "nav-item-container" ] (List.map navItems model.navItems)
            ]
        , img [ src "/logo.svg" ] []
        , h1 [] [ text "Hack Yer News" ]
        , div [ class "main" ]
            [ div [ class "filter-container" ] [ text "Filter Container" ]
            , div [ class "news-container" ] [ text "News Container" ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
