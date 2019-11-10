module StatsTreeDiagram exposing (treeDiagram)

import BinaryTree
    exposing
        ( BinaryTree(..)
        )
import BinaryTreeDiagram
import Html exposing (Html)
import Type
    exposing
        ( Msg(..)
        , StatsInfo
        , StatsTree
        )


convertTree : BinaryTree v -> BinaryTreeDiagram.BinaryTree v
convertTree tree =
    case tree of
        Empty ->
            BinaryTreeDiagram.Empty

        Node v left right ->
            BinaryTreeDiagram.Node
                v
                (convertTree left)
                (convertTree right)


getNodeColor : StatsInfo -> String
getNodeColor statsInfo =
    case statsInfo.color of
        "R" ->
            "red"

        "B" ->
            "black"

        _ ->
            "yellow"


getNodeText : StatsInfo -> String
getNodeText statsInfo =
    statsInfo.n
        |> String.fromInt


treeDiagram : StatsTree -> Html Msg
treeDiagram stats =
    stats
        |> convertTree
        |> BinaryTreeDiagram.diagramView getNodeColor getNodeText
