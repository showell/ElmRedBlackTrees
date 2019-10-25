module View exposing (view)

import BinaryTree
import DictHelper
    exposing
        ( DescribeTree(..)
        , StatsInfo
        , StatsTree
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
        , th
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


view : Model -> Html Msg
view model =
    let
        leftSide =
            treeTable model.activeTreeSize

        rightSide =
            treeDiagram model.activeTreeSize

        leftCss =
            [ style "height" "100vh"
            , style "overflow-y" "auto"
            , style "padding-right" "100px"
            , style "min-width" "200px"
            ]

        rightCss =
            [ style "padding" "20px"
            ]
    in
    div [ style "display" "flex", style "flex-direction" "row" ]
        [ div leftCss [ leftSide ]
        , div rightCss [ rightSide ]
        ]


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


getNodeColor : StatsInfo -> String
getNodeColor statsNode =
    case statsNode.color of
        "R" ->
            "red"

        "B" ->
            "black"

        _ ->
            "yellow"


treeDiagram : Int -> Html Msg
treeDiagram n =
    let
        stats =
            List.range 1 n
                |> listToStats
    in
    stats
        |> TreeDiagram.diagramView getNodeColor


treeTable : Int -> Html Msg
treeTable activeTreeSize =
    let
        listTups =
            List.range 1 63
                |> List.map (\n -> ( n, List.range 1 n ))

        treeTups =
            listTups
                |> List.map (\( n, list ) -> ( n, listToStats list ))

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

        header s =
            th [] [ Html.text s ]

        headerRow =
            [ "size"
            , "height"
            , "black subtree sizes"
            , "action"
            ]
                |> List.map header
                |> tr []

        formatCell : Html msg -> Html msg
        formatCell item =
            item
                |> List.singleton
                |> td
                    [ style "padding-left" "10px"
                    , style "white-space" "nowrap"
                    , style "text-align" "center"
                    ]

        formatRow : ( Int, StatsTree ) -> Html Msg
        formatRow ( n, stats ) =
            let
                css =
                    if n == activeTreeSize then
                        [ style "background" "lightgreen"
                        ]

                    else
                        []
            in
            stats
                |> cells
                |> List.map formatCell
                |> tr css
    in
    treeTups
        |> List.map formatRow
        |> (\rows -> table [] (headerRow :: rows))
