module DictView exposing (..)

import DictHelper exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


viewCounts : List Int -> String
viewCounts counts =
    counts
        |> List.map String.fromInt
        |> String.join ", "


viewDescription : DescribeTree -> String
viewDescription treeDesc =
    let
        s =
            case treeDesc of
                Tree1 count1 ->
                    viewCounts [ count1 ]

                Tree2 count1 count2 ->
                    viewCounts [ count1, count2 ]

                Tree3 count1 count2 count3 ->
                    viewCounts [ count1, count2, count3 ]

                _ ->
                    "not implemented"
    in
    s


viewList : List Int -> String
viewList lst =
    let
        min =
            lst
                |> List.minimum
                |> Maybe.withDefault 0
                |> String.fromInt

        max =
            lst
                |> List.maximum
                |> Maybe.withDefault 0
                |> String.fromInt
    in
    min ++ ".." ++ max


show =
    let
        lists : List (List Int)
        lists =
            List.range 3 200
                |> List.map (List.range 1)

        formatCell : String -> Html msg
        formatCell item =
            item
                |> Html.text
                |> List.singleton
                |> td
                    [ style "padding-left" "10px"
                    , style "text-align" "center"
                    ]

        formatRow : List String -> Html msg
        formatRow items =
            items
                |> List.map formatCell
                |> tr []
    in
    lists
        |> List.map
            (\lst ->
                [ viewList lst, viewDescription (listToDescription lst) ]
            )
        |> List.map formatRow
        |> table []
