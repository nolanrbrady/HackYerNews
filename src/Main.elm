module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import HNApi exposing (fetchFakeNews)
import Html exposing (Html, button, div, h1, h2, h4, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List.Extra exposing (unique)
import Http
import Json.Decode exposing (Decoder, int, list, string, field)
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
--   -- , type :: String
--   , url : String
--   }

type alias Story =
  { title : String
  }

storyDecoder : Decoder Story
storyDecoder =
    Json.Decode.map Story ( field "title" string )

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
    , Cmd.none
    )



---- HELPER FUNCTIONS -----

fetchArticleIds =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"
        , expect = Http.expectJson GotArticleIds (list int)
        }
fetchArticle: Maybe Int -> Cmd Msg
fetchArticle storyId =
    case storyId of
        Nothing ->
            Cmd.none
        Just id ->
            let baseUrl = "https://hacker-news.firebaseio.com/v0/item/"
            in
                Http.get
                  { url = baseUrl ++ String.fromInt id ++ ".json?print=pretty"
                  , expect = Http.expectJson GotStory storyDecoder
                  }

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
            (model, fetchArticleIds)
        GotArticleIds result ->
            case result of
                Ok ids ->
                    ( {model | articleIds = ids}, fetchArticle (List.head ids))
                    -- |> addToast (Toasty.Defaults.Success "Allright!" "Top Articles Fetched")
                Err _  ->
                    (model, Cmd.none)
                    |> addToast (Toasty.Defaults.Error "Oh no!" "Could not fetch top articles. Please try again!")
        GetStory storyId ->
            (model, fetchArticle ( Just storyId ))
        GotStory result ->
            case result of
                Ok story ->
                  ({model | stories = [ story ]}, Cmd.none)
                Err _  ->
                    (model, Cmd.none)
                    |> addToast (Toasty.Defaults.Error "Oh no!" "Could not fetch article. Please try again!")



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
        [ Toasty.view myConfig Toasty.Defaults.view ToastyMsg model.toasties
        , div [ class "navbar" ]
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
                , renderStories model.stories
                ]
            ]
        , div [] 
              [ button [ onClick (FetchArticleIds) ] [text "Fetch Article Id"]
              , text <| String.join " " <| List.map String.fromInt model.articleIds
              ]
        ]

renderStories : List Story -> Html Msg
renderStories stories =
    case List.head stories of
        Just story ->
            div [] [ text story.title ]
        Nothing ->
            div [] []

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
