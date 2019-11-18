module Route exposing
    ( modelFromUrl
    , pageFromUrl
    , pageSlug
    )

import RangeList
import Type
    exposing
        ( InsertionMode(..)
        , LessonPage(..)
        , Model
        , Page(..)
        )
import Url
    exposing
        ( Url
        )


pageSlug : Page -> String
pageSlug page =
    case page of
        Explorer rangeSpec ->
            "tree/" ++ RangeList.toSlug rangeSpec

        Lesson lesson ->
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
        parsePage : String -> Maybe Page
        parsePage frag =
            case frag of
                "allfour" ->
                    AllFourTrees
                        |> Lesson
                        |> Just

                "simple4" ->
                    SimplifiedFourTrees
                        |> Lesson
                        |> Just

                "extend" ->
                    ExtendList
                        |> Lesson
                        |> Just

                "allfive" ->
                    AllFiveTrees
                        |> Lesson
                        |> Just

                _ ->
                    Nothing

        parseTree : String -> Maybe Page
        parseTree frag =
            RangeList.fromSlug "tree" frag
                |> Maybe.map Explorer
    in
    url.fragment
        |> Maybe.andThen (oneOf [ parseTree, parsePage ])


modelFromUrl : Url -> Page -> Model -> Model
modelFromUrl url initPage model =
    pageFromUrl url
        |> Maybe.withDefault initPage
        |> (\p -> { model | page = p })


oneOf : List (a -> Maybe b) -> a -> Maybe b
oneOf fList arg =
    -- given a list of functions and an arg, apply
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
                    oneOf rest arg
