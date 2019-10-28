module Main exposing (main)

import Browser
import ExplorerView
import Html
    exposing
        ( Html
        , div
        )
import Html.Attributes
    exposing
        ( style
        )
import Html.Events
    exposing
        ( onClick
        )
import SmallTreeView
import Type
    exposing
        ( InsertionMode(..)
        , Model
        , Msg(..)
        , Page(..)
        , SmallTreeLesson(..)
        )



-- MODEL / INIT


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        page =
            initExplorer

        model =
            { page = page
            }
    in
    ( model, Cmd.none )


initExplorer : Page
initExplorer =
    let
        rangeSpec =
            { n = 16
            , insertionMode = InsertForward
            }
    in
    Explorer rangeSpec


initSmallTree : Page
initSmallTree =
    SmallTree AllFourTrees



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        model_ =
            case msg of
                ShowTree rangeSpec ->
                    { model
                        | page = Explorer rangeSpec
                    }

                SetPage page ->
                    { model
                        | page = page
                    }
    in
    ( model_, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        subView =
            case model.page of
                Explorer rangeSpec ->
                    ExplorerView.view rangeSpec

                SmallTree lesson ->
                    SmallTreeView.view lesson

        body =
            [ pageTabs model.page
            , subView
            ]
    in
    { title = "RedBlack Trees from Elm"
    , body = body
    }


pageTabs : Page -> Html Msg
pageTabs activePage =
    let
        explorerLabel =
            "Explorer"

        smallTreesLabel =
            "Small trees"

        tabConfigs =
            [ ( explorerLabel, SetPage initExplorer )
            , ( smallTreesLabel, SetPage initSmallTree )
            ]

        activeLabel =
            case activePage of
                Explorer _ ->
                    explorerLabel

                SmallTree _ ->
                    smallTreesLabel
    in
    makeTabs tabConfigs activeLabel


makeTabs : List ( String, Msg ) -> String -> Html Msg
makeTabs tabConfigs activeLabel =
    let
        makeTab ( label, cmd ) =
            let
                disabled =
                    label == activeLabel
            in
            Html.button
                [ Html.Attributes.disabled disabled
                , onClick cmd
                ]
                [ Html.text label
                ]
    in
    tabConfigs
        |> List.map makeTab
        |> div [ style "padding" "10px" ]
