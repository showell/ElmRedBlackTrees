module View exposing (view)

import BinaryTree
import DictHelper
    exposing
        ( DescribeTree(..)
        , StatsInfo
        , StatsTree
        , dictToStats
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
import RangeList
import TreeDiagram
import Type
    exposing
        ( InsertionMode(..)
        , Model
        , Msg(..)
        , RangeSpec
        )


view : Model -> Html Msg
view model =
    let
        rangeSpec =
            model.rangeSpec

        leftSide =
            [ treeTable rangeSpec
            ]

        rightSide =
            [ diagramHeading rangeSpec
            , plusMinusButtons rangeSpec
            ]
                ++ subTreeButtons rangeSpec
                ++ [ treeDiagram rangeSpec
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


diagramHeading : RangeSpec -> Html Msg
diagramHeading spec =
    div []
        [ div
            [ style "font-size" "120%"
            , style "padding" "5px"
            ]
            [ Html.text "RedBlack tree for "
            , Html.b [] [ Html.text (String.fromInt spec.n) ]
            , Html.text " elements"
            ]
        , div [ style "padding" "5px" ]
            [ Html.text (RangeList.toString spec)
            , button [ onClick (ShowTree (RangeList.flip spec)) ] [ Html.text "flip" ]
            ]
        ]


showTreeButton : RangeSpec -> String -> Html Msg
showTreeButton spec label =
    button [ onClick (ShowTree spec) ] [ Html.text label ]


showTreeNumButton : RangeSpec -> Html Msg
showTreeNumButton spec =
    showTreeButton spec (String.fromInt spec.n)


plusMinusButtons : RangeSpec -> Html Msg
plusMinusButtons spec =
    let
        lessButtons =
            if spec.n == 1 then
                []

            else
                [ showTreeButton (RangeList.decr spec) "less" ]

        moreButtons =
            -- We don't limit this
            [ showTreeButton (RangeList.incr spec) "more" ]

        cannedButtons =
            [ 3, 7, 15, 31, 63 ]
                |> List.map (RangeList.setN spec)
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


subTreeButtons : RangeSpec -> List (Html Msg)
subTreeButtons spec =
    let
        treeDesc =
            spec
                |> RangeList.toDict
                |> dictToStats
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
                |> List.map (RangeList.setN spec)
                |> List.map showTreeNumButton
    in
    if List.isEmpty buttons then
        []

    else
        Html.text "Subtrees: "
            :: buttons
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


specToStats : RangeSpec -> StatsTree
specToStats spec =
    spec
        |> RangeList.toDict
        |> dictToStats


treeDiagram : RangeSpec -> Html Msg
treeDiagram spec =
    spec
        |> specToStats
        |> TreeDiagram.diagramView getNodeColor


treeTable : RangeSpec -> Html Msg
treeTable currSpec =
    let
        insertionMode =
            currSpec.insertionMode

        allSpecs =
            List.range 1 63
                |> List.map (\n -> { n = n, insertionMode = insertionMode })

        specStatTups =
            allSpecs
                |> List.map (\spec -> ( spec, specToStats spec ))

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

        cells spec stats =
            [ size stats
            , height stats
            , description (statsToDescription stats) |> Html.text
            , showTreeButton spec "show"
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

        formatRow : ( RangeSpec, StatsTree ) -> Html Msg
        formatRow ( spec, stats ) =
            let
                css =
                    if spec.n == currSpec.n then
                        [ style "background" "lightgreen"
                        ]

                    else
                        []
            in
            cells spec stats
                |> List.map formatCell
                |> tr css
    in
    specStatTups
        |> List.map formatRow
        |> (\rows -> table [] (headerRow :: rows))
