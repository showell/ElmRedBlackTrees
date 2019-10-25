module Main exposing (main)

import Browser
import DictView exposing (show)
import Type exposing (Model, Msg(..))



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
        model =
            { activeTreeSize = 15
            }
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        model_ =
            case msg of
                ShowTree n ->
                    { model
                        | activeTreeSize = n
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
    , body = [ DictView.show model ]
    }
