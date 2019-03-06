module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import HNApi exposing (fetchFakeNews)
import Html exposing (Html, button, div, h1, h2, h4, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List.Extra exposing (unique)



---- MODEL ----


type alias FakeNews =
    { title : String, tag : String }


type alias Model =
    { navItems : List String
    , activeTags : List String
    , fakeNews : List FakeNews
    }


init : ( Model, Cmd Msg )
init =
    ( { navItems = [ "News", "Jobs", "Settings" ]
      , fakeNews = fetchFakeNews
      , activeTags = []
      }
    , Cmd.none
    )



---- HELPER FUNCTIONS -----


renderTags : String -> Html Msg
renderTags tag =
    button [ onClick (FilterNews tag) ] [ h4 [ class "pill" ] [ text tag ] ]


renderNewsFeed : FakeNews -> Html Msg
renderNewsFeed news =
    div [ class "news-item" ]
        [ h1 [ class "news-title" ] [ text news.title ]
        ]


manageActiveTags tag activeTags =
    if List.member tag activeTags then
        List.filter (\item -> tag /= item) activeTags

    else
        tag :: activeTags


navItems : String -> Html Msg
navItems item =
    p [ class "nav-item" ] [ text item ]



-- filterNews tag model =
--     List.filter (\news -> news.tag == tag) model.fakeNews
---- UPDATE ----


type Msg
    = NoOp
    | FilterNews String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FilterNews tag ->
            ( { model | activeTags = manageActiveTags tag model.activeTags }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        allTags =
            List.map (\news -> news.tag) model.fakeNews
                |> unique

        articles =
            if List.length model.activeTags == 0 then
                model.fakeNews

            else
                List.filter (\article -> List.member article.tag model.activeTags) model.fakeNews
    in
    div []
        [ div [ class "navbar" ]
            [ h1 [] [ text "Hack Yer News" ]
            , div [ class "nav-item-container" ] (List.map navItems model.navItems)
            ]
        , img [ src "/logo.svg" ] []
        , h1 [] [ text "Hack Yer News" ]
        , div [ class "main" ]
            [ div [ class "filter-container" ]
                [ text "Filter Container"
                , div [ class "pill-container" ] (List.map renderTags allTags)
                ]
            , div [ class "news-container" ]
                [ text "News Container"
                , div [] (List.map renderNewsFeed articles)
                ]
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
