module Main exposing (main)

import Browser
import DictHelper exposing (..)
import Html

-- MODEL / INIT

type alias Model =
    { title : String
    }

type Msg =
    PlaceHolder

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
            { title = "RedBlack Trees From Elm"
            }
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


show =
    DictHelper.show
        |> Html.text
        |> List.singleton
        |> Html.pre []


view : Model -> Browser.Document Msg
view model =
    { title = model.title
    , body = [ show ]
    }
