module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import ExplorerView
import Lesson
import PageTabs
import Route
import Type
    exposing
        ( InsertionMode(..)
        , LessonPage(..)
        , Model
        , Msg(..)
        , Page(..)
        )
import Url
    exposing
        ( Url
        )



-- Url stuff


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    UrlRequested


onUrlChange : Url -> Msg
onUrlChange url =
    UrlChanged url



-- MODEL / INIT


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initModel url key, Cmd.none )


initModel : Url -> Navigation.Key -> Model
initModel url key =
    let
        page =
            Route.pageFromUrl url
                |> Maybe.withDefault initPage

        model =
            { page = page
            , key = key
            }
    in
    model


initPage : Page
initPage =
    initExplorer


initExplorer : Page
initExplorer =
    let
        rangeSpec =
            { n = 16
            , insertionMode = InsertForward
            }
    in
    Explorer rangeSpec


initLesson : Page
initLesson =
    Lesson AllFourTrees



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowTree rangeSpec ->
            let
                page =
                    Explorer rangeSpec

                model_ =
                    { model
                        | page = page
                    }

                route =
                    "#" ++ Route.pageSlug page

                pushCmd =
                    Navigation.pushUrl model.key route
            in
            ( model_, pushCmd )

        SetPage page ->
            let
                model_ =
                    { model
                        | page = page
                    }

                route =
                    "#" ++ Route.pageSlug page

                pushCmd =
                    Navigation.pushUrl model.key route
            in
            ( model_, pushCmd )

        UrlRequested ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( Route.modelFromUrl url initPage model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


tabConfigs : List ( String, Msg )
tabConfigs =
    [ ( "Explorer", SetPage initExplorer )
    , ( "Lesson", SetPage initLesson )
    ]


view : Model -> Browser.Document Msg
view model =
    let
        subView =
            case model.page of
                Explorer rangeSpec ->
                    ExplorerView.view rangeSpec

                Lesson lesson ->
                    Lesson.view lesson

        body =
            [ PageTabs.makeTabs tabConfigs
            , subView
            ]
    in
    { title = "RedBlack Trees from Elm"
    , body = body
    }
