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
        n =
            model.activeTreeSize

        leftSide =
            [ treeTable n
            ]

        rightSide =
            [ diagramHeading n
            , plusMinusButtons n
            ]
                ++ subTreeButtons n
                ++ [ treeDiagram n
                   ]

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
        [ div leftCss leftSide
        , div rightCss rightSide
        ]


diagramHeading : Int -> Html Msg
diagramHeading n =
    div
        [ style "font-size" "120%"
        , style "padding" "5px"
        ]
        [ Html.text "RedBlack tree for "
        , Html.b [] [ Html.text (String.fromInt n) ]
        , Html.text " elements"
        ]


showTreeButton n label =
    button [ onClick (ShowTree n) ] [ Html.text label ]


showTreeNumButton n =
    showTreeButton n (String.fromInt n)


plusMinusButtons : Int -> Html Msg
plusMinusButtons nCurrent =
    let
        lessButtons =
            if nCurrent == 1 then
                []

            else
                [ showTreeButton (nCurrent - 1) "less" ]

        moreButtons =
            -- We don't limit this
            [ showTreeButton (nCurrent + 1) "more" ]

        cannedButtons =
            [ 3, 7, 15, 31, 63 ]
                |> List.map showTreeNumButton

        buttons =
            lessButtons ++ moreButtons ++ cannedButtons
    in
    div [] buttons


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


arithmeticBreakdown : DescribeTree -> String
arithmeticBreakdown treeDesc =
    case treeDesc of
        Tree2 n1 n2 ->
            String.fromInt (n1 + n2 + 1)
                ++ " = "
                ++ String.fromInt n1
                ++ " + 1 + "
                ++ String.fromInt n2

        Tree3 n1 n2 n3 ->
            String.fromInt (n1 + n2 + n3 + 2)
                ++ " = "
                ++ String.fromInt n1
                ++ " + 1 + "
                ++ String.fromInt n2
                ++ " + 1 + "
                ++ String.fromInt n3

        _ ->
            ""


subTreeButtons : Int -> List (Html Msg)
subTreeButtons n =
    let
        treeDesc =
            List.range 1 n
                |> listToStats
                |> statsToDescription

        counts =
            case treeDesc of
                Tree1 n1 ->
                    [ n1 ]

                Tree2 n1 n2 ->
                    [ n1, n2 ]

                Tree3 n1 n2 n3 ->
                    [ n1, n2, n3 ]

                _ ->
                    []

        buttons =
            counts
                |> List.map showTreeNumButton
    in
    if List.isEmpty buttons then
        []

    else
        [ Html.text "Subtrees: " ]
            ++ buttons
            ++ [ div [] [ Html.text (arithmeticBreakdown treeDesc) ] ]


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

        cells n stats =
            [ size stats
            , height stats
            , description (statsToDescription stats) |> Html.text
            , showTreeButton n "show"
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
                |> cells n
                |> List.map formatCell
                |> tr css
    in
    treeTups
        |> List.map formatRow
        |> (\rows -> table [] (headerRow :: rows))
