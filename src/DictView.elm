module DictView exposing (..)

import BinaryTree exposing (height)
import DictHelper exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


viewDescription : DescribeTree -> String
viewDescription treeDesc =
    case treeDesc of
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


viewList : List Int -> String
viewList lst =
    lst
        |> List.length
        |> String.fromInt


show =
    let
        lists : List (List Int)
        lists =
            List.range 1 200
                |> List.map (List.range 1)

        height lst =
            lst
                |> listToStats
                |> BinaryTree.height
                |> String.fromInt

        cells lst =
            [ viewList lst
            , height lst
            , viewDescription (listToDescription lst)
            ]

        formatCell : String -> Html msg
        formatCell item =
            item
                |> Html.text
                |> List.singleton
                |> td
                    [ style "padding-left" "10px"
                    ]

        formatRow : List String -> Html msg
        formatRow items =
            items
                |> List.map formatCell
                |> tr []
    in
    lists
        |> List.map cells
        |> List.map formatRow
        |> table []
