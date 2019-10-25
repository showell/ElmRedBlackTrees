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
        , button
        , div
        , table
        , td
        , tr
        )
import Html.Attributes
    exposing
        ( style
        )
import Html.Events
    exposing
        ( onClick
        )
import TreeDiagram
import Type
    exposing
        ( Model
        , Msg(..)
        )


description : DescribeTree -> String
description treeDesc =
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


show : Model -> Html Msg
show model =
    let
        leftSide =
            treeTable

        rightSide =
            treeDiagram model.activeTreeSize

        leftCss =
            [ style "height" "100vh"
            , style "overflow-y" "auto"
            , style "padding-right" "100px"
            ]
    in
    div [ style "display" "flex", style "flex-direction" "row" ]
        [ div leftCss [ leftSide ]
        , div [ style "padding" "20px" ] [ rightSide ]
        ]


treeDiagram : Int -> Html Msg
treeDiagram n =
    let
        stats =
            List.range 1 n
                |> listToStats
    in
    stats
        |> TreeDiagram.diagramView


treeTable : Html Msg
treeTable =
    let
        lists : List (List Int)
        lists =
            List.range 1 100
                |> List.map (List.range 1)

        trees =
            lists
                |> List.map listToStats

        size tree =
            tree
                |> BinaryTree.size
                |> String.fromInt
                |> Html.text

        height tree =
            tree
                |> BinaryTree.height
                |> String.fromInt
                |> Html.text

        getButton stats =
            let
                n =
                    BinaryTree.size stats
            in
            button [ onClick (ShowTree n) ] [ Html.text "show" ]

        cells stats =
            [ size stats
            , height stats
            , description (statsToDescription stats) |> Html.text
            , getButton stats
            ]

        formatCell : Html msg -> Html msg
        formatCell item =
            item
                |> List.singleton
                |> td
                    [ style "padding-left" "10px"
                    , style "white-space" "nowrap"
                    ]

        formatRow : List (Html msg) -> Html msg
        formatRow items =
            items
                |> List.map formatCell
                |> tr []
    in
    trees
        |> List.map cells
        |> List.map formatRow
        |> table []
