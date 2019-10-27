module StatsTreeDiagram exposing (treeDiagram)

import Html exposing (Html)
import TreeDiagram
import Type
    exposing
        ( Msg(..)
        , StatsInfo
        , StatsTree
        )


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
        |> TreeDiagram.diagramView getNodeColor getNodeText
