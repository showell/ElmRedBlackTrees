module TreeSummary exposing
    ( TreeSummary(..)
    , arithmeticBreakdown
    , description
    , toCountList
    )

import Html
    exposing
        ( Html
        , div
        , span
        )
import Html.Attributes
    exposing
        ( style
        )
import Type
    exposing
        ( Msg(..)
        )


type TreeSummary
    = Broken
    | Nada
    | Tree1 Int
    | Tree2 Int Int
    | Tree3 Int Int Int


toCountList : TreeSummary -> List Int
toCountList treeSummary =
    case treeSummary of
        Tree1 n1 ->
            [ n1 ]

        Tree2 n1 n2 ->
            [ n1, n2 ]

        Tree3 n1 n2 n3 ->
            [ n1, n2, n3 ]

        _ ->
            []


description : TreeSummary -> String
description treeSummary =
    case treeSummary of
        Tree1 count1 ->
            String.fromInt count1

        Tree2 count1 count2 ->
            String.fromInt count1
                ++ " "
                ++ String.fromInt count2

        Tree3 count1 count2 count3 ->
            String.fromInt count1
                ++ " "
                ++ String.fromInt count2
                ++ " "
                ++ String.fromInt count3

        Nada ->
            "nada"

        Broken ->
            "broken"


coloredText : String -> String -> Html Msg
coloredText color text =
    span [ style "color" color ] [ Html.text text ]


coloredInt : String -> Int -> Html Msg
coloredInt color n =
    coloredText color (String.fromInt n)


arithmeticBreakdown : TreeSummary -> Html Msg
arithmeticBreakdown treeSummary =
    let
        equal =
            coloredText "blue" " = "

        plus =
            coloredText "blue" " + "
    in
    case treeSummary of
        Tree2 n1 n2 ->
            [ coloredInt "blue" (n1 + n2 + 1)
            , equal
            , coloredInt "black" n1
            , plus
            , coloredInt "black" 1
            , plus
            , coloredInt "black" n2
            ]
                |> div []

        Tree3 n1 n2 n3 ->
            [ coloredInt "blue" (n1 + n2 + n3 + 2)
            , equal
            , coloredInt "black" n1
            , plus
            , coloredInt "red" 1
            , plus
            , coloredInt "black" n2
            , plus
            , coloredInt "black" 1
            , plus
            , coloredInt "black" n3
            ]
                |> div []

        _ ->
            div [] []
