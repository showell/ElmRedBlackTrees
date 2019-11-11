module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
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
import RangeList
import SmallTreeView
import Type
    exposing
        ( InsertionMode(..)
        , Model
        , Msg(..)
        , Page(..)
        , SmallTreeLesson(..)
        )
import Url
    exposing
        ( Url
        )



-- Url stuff


onUrlRequest req =
    UrlRequested


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
            pageFromUrl url
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


initSmallTree : Page
initSmallTree =
    SmallTree AllFourTrees



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
                    "#" ++ pageSlug page

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
                    "#" ++ pageSlug page

                pushCmd =
                    Navigation.pushUrl model.key route
            in
            ( model_, pushCmd )

        UrlRequested ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( modelFromUrl url model, Cmd.none )


pageSlug : Page -> String
pageSlug page =
    case page of
        Explorer rangeSpec ->
            "tree/" ++ RangeList.toSlug rangeSpec

        SmallTree lesson ->
            case lesson of
                AllFourTrees ->
                    "allfour"

                SimplifiedFourTrees ->
                    "simple4"

                ExtendList ->
                    "extend"

                AllFiveTrees ->
                    "allfive"


pageFromUrl : Url -> Maybe Page
pageFromUrl url =
    let
        parsePage frag =
            case frag of
                "allfour" ->
                    AllFourTrees
                        |> SmallTree
                        |> Just

                "simple4" ->
                    SimplifiedFourTrees
                        |> SmallTree
                        |> Just

                "extend" ->
                    ExtendList
                        |> SmallTree
                        |> Just

                "allfive" ->
                    AllFiveTrees
                        |> SmallTree
                        |> Just

                _ ->
                    Nothing

        parseTree frag =
            case RangeList.fromSlug "tree" frag of
                Just spec ->
                    Explorer spec
                        |> Just

                Nothing ->
                    Nothing
    in
    case url.fragment of
        Just frag ->
            oneOf frag [ parseTree, parsePage ]

        Nothing ->
            Nothing


modelFromUrl : Url -> Model -> Model
modelFromUrl url model =
    pageFromUrl url
        |> Maybe.withDefault initPage
        |> (\p -> { model | page = p })


oneOf : a -> List (a -> Maybe b) -> Maybe b
oneOf arg fList =
    -- given an arg and list of functions, apply
    -- functions in order until you get a `Just`
    -- result
    case fList of
        [] ->
            Nothing

        f :: rest ->
            case f arg of
                Just v ->
                    Just v

                Nothing ->
                    oneOf arg rest



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
