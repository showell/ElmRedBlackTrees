module DictView exposing (show)

import BinaryTree
import DictHelper
    exposing
        ( DescribeTree(..)
        , listToStats
        , statsToDescription
        )
import Html
    exposing
        ( Html
        , table
        , td
        , tr
        )
import Html.Attributes
    exposing
        ( style
        )


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


show : Html msg
show =
    let
        lists : List (List Int)
        lists =
            List.range 1 200
                |> List.map (List.range 1)

        trees =
            lists
                |> List.map listToStats

        size tree =
            tree
                |> BinaryTree.size
                |> String.fromInt

        height tree =
            tree
                |> BinaryTree.height
                |> String.fromInt

        cells stats =
            [ size stats
            , height stats
            , viewDescription (statsToDescription stats)
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
    trees
        |> List.map cells
        |> List.map formatRow
        |> table []
