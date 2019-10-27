module Main exposing (main)

import Browser
import ExplorerView exposing (view)
import Type
    exposing
        ( InsertionMode(..)
        , Model
        , Msg(..)
        , Page(..)
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
        rangeSpec =
            { n = 16
            , insertionMode = InsertForward
            }

        page =
            Explorer rangeSpec

        model =
            { page = page
            }
    in
    ( model, Cmd.none )



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
        bodyContents =
            case model.page of
                Explorer rangeSpec ->
                    ExplorerView.view rangeSpec
    in
    { title = "RedBlack Trees from Elm"
    , body = [ bodyContents ]
    }
