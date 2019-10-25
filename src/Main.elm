module Main exposing (main)

import Browser
import Type
    exposing
        ( InsertionMode(..)
        , Model
        , Msg(..)
        )
import View exposing (view)



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
            { n = 10
            , insertionMode = InsertForward
            }

        model =
            { rangeSpec = rangeSpec
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
                        | rangeSpec = rangeSpec
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
    { title = "RedBlack Trees from Elm"
    , body = [ View.view model ]
    }
