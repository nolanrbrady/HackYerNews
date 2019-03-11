module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import HNApi exposing (fetchFakeNews)
import Html exposing (Html, a, button, div, h1, h2, h4, i, img, input, p, span, text)
import Html.Attributes exposing (class, href, src, target, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, map4, string)
import List.Extra exposing (unique)
import Toasty
import Toasty.Defaults



---- MODEL ----


type alias FakeNews =
    { title : String, tag : String }



-- type alias Story =
--   { by : String
--   , descendants : Int
--   , id : Int
--   , kids : List ( Maybe Int )
--   , score : Int
--   , time : Int
--   , title : String
--   , type :: String
--   , url : String
--   }


type alias Story =
    { title : String
    , url : String
    , score : Int
    , by : String
    }


storyDecoder : Decoder Story
storyDecoder =
    map4 Story
        (field "title" string)
        (field "url" string)
        (field "score" int)
        (field "by" string)


type alias Model =
    { navItems : List String
    , activeTags : List String
    , fakeNews : List FakeNews
    , articleIds : List Int
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    , stories : List Story
    }


init : ( Model, Cmd Msg )
init =
    ( { navItems = [ "News", "Jobs", "Settings" ]
      , fakeNews = fetchFakeNews
      , activeTags = []
      , articleIds = []
      , toasties = Toasty.initialState
      , stories = []
      }
    , fetchArticleIds
    )



---- HELPER FUNCTIONS -----


fetchArticleIds =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"
        , expect = Http.expectJson GotArticleIds (list int)
        }


fetchArticle : Int -> Cmd Msg
fetchArticle id =
    let
        baseUrl =
            "https://hacker-news.firebaseio.com/v0/item/"
    in
    Http.get
        { url = baseUrl ++ String.fromInt id ++ ".json?print=pretty"
        , expect = Http.expectJson GotStory storyDecoder
        }


displayTags : String -> Model -> Html Msg
displayTags tag model =
    if List.member tag model.activeTags then
        button [ onClick (FilterNews tag) ] [ h4 [ class "pill" ] [ text tag ] ]

    else
        button [ onClick (FilterNews tag) ] [ h4 [ class "active-pill" ] [ text tag ] ]


renderTags allTags model =
    div [ class "pill-container" ] (List.map (\tag -> displayTags tag model) allTags)


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
    | FetchArticleIds
    | GotArticleIds (Result Http.Error (List Int))
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | GetStory Int
    | GotStory (Result Http.Error Story)


myConfig : Toasty.Config Msg
myConfig =
    Toasty.Defaults.config
        |> Toasty.delay 5000


addToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToast myConfig ToastyMsg toast ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToastyMsg subMsg ->
            Toasty.update Toasty.config ToastyMsg subMsg model

        FilterNews tag ->
            ( { model | activeTags = manageActiveTags tag model.activeTags }, Cmd.none )

        FetchArticleIds ->
            ( model, fetchArticleIds )

        GotArticleIds result ->
            case result of
                Ok ids ->
                    ( { model | articleIds = ids }, Cmd.batch (List.map (\id -> fetchArticle id) ids) )

                -- |> addToast (Toasty.Defaults.Success "Allright!" "Top Articles Fetched")
                Err _ ->
                    ( model, Cmd.none )
                        |> addToast (Toasty.Defaults.Error "Oh no!" "Could not fetch top articles. Please try again!")

        GetStory storyId ->
            ( model, fetchArticle storyId )

        GotStory result ->
            case result of
                Ok story ->
                    ( { model | stories = story :: model.stories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
                        |> addToast (Toasty.Defaults.Error "Oh no!" "Could not fetch article. Please try again!")



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        allTags =
            List.map (\news -> news.tag) model.fakeNews
                |> unique

        fakeArticles =
            if List.length model.activeTags == 0 then
                model.fakeNews

            else
                List.filter (\article -> List.member article.tag model.activeTags) model.fakeNews

        stories =
            List.map GetStory model.articleIds
    in
    div []
        [ Toasty.view myConfig Toasty.Defaults.view ToastyMsg model.toasties
        , div [ class "navbar" ]
            [ h1 [] [ text "Hack Yer News" ]
            , div [ class "nav-item-container" ] (List.map navItems model.navItems)
            ]
        , img [ src "/logo.svg" ] []
        , h1 [] [ text "Hack Yer News" ]
        , div [ class "main" ]
            [ div [ class "filter-container" ]
                [ h2 [ class "news-title" ] [ text "Filter Container" ]
                , renderTags allTags model
                , div [] [ input [ type_ "radio" ] [] ]
                ]
            , div [ class "news-container" ]
                [ h2 [ class "news-title" ] [ text "News Container" ]
                , div [] (List.map (\story -> renderStories story) model.stories)
                ]
            ]
        ]


renderStories : Story -> Html Msg
renderStories story =
    div [ class "news" ]
        [ h4 [] [ a [ href story.url, target "_blank" ] [ text story.title ] ]
        , div [ class "news-info" ]
            [ p [] [ text ("Score: " ++ String.fromInt story.score) ]
            , p [ class "margin-left" ] [ text ("By: " ++ story.by) ]
            ]
        , span [ class "divider" ] []
        ]


renderToast : String -> Html Msg
renderToast toast =
    div [] [ text toast ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
