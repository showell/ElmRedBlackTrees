module View exposing (view)

import BinaryTree
import DictHelper
    exposing
        ( dictToStats
        , statsToSummary
        )
import Html
    exposing
        ( Html
        , div
        , span
        )
import Html.Attributes
    exposing
        ( disabled
        , style
        )
import Html.Events
    exposing
        ( onClick
        )
import RangeList
import TreeDiagram
import TreeSummary
    exposing
        ( TreeSummary(..)
        )
import Type
    exposing
        ( InsertionMode(..)
        , Model
        , Msg(..)
        , RangeSpec
        , StatsInfo
        , StatsTree
        )


view : Model -> Html Msg
view model =
    let
        rangeSpec =
            model.rangeSpec

        treeView =
            div [ style "max-width" "800px" ]
                [ treeDiagram rangeSpec ]

        contents =
            [ cannedButtons rangeSpec
            , plusMinusButtons rangeSpec
            , Html.hr [] []
            , diagramHeading rangeSpec
            ]
                ++ subTreeButtons rangeSpec
                ++ [ treeView ]

        css =
            [ style "padding" "20px"
            ]
    in
    div css contents


diagramHeading : RangeSpec -> Html Msg
diagramHeading spec =
    let
        flipButton =
            Html.button
                [ onClick (ShowTree (RangeList.flip spec)) ]
                [ Html.text "flip" ]
    in
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
            , flipButton
            ]
        ]


showTreeButton : RangeSpec -> String -> Html Msg
showTreeButton spec label =
    Html.button [ onClick (ShowTree spec) ] [ Html.text label ]


showTreeNumButton : RangeSpec -> Html Msg
showTreeNumButton spec =
    showTreeButton spec (String.fromInt spec.n)


plusMinusButtons : RangeSpec -> Html Msg
plusMinusButtons spec =
    let
        lessButton =
            let
                attrs =
                    if spec.n == 1 then
                        [ disabled True ]

                    else
                        [ onClick (ShowTree (RangeList.decr spec)) ]
            in
            Html.button attrs [ Html.text "less" ]

        moreButton =
            -- We don't limit this
            showTreeButton (RangeList.incr spec) "more"

        buttons =
            [ lessButton, moreButton ]
    in
    div [] buttons


cannedButtons : RangeSpec -> Html Msg
cannedButtons spec =
    [ 2, 4, 8, 16, 32, 64, 128 ]
        |> List.map (RangeList.setN spec)
        |> List.map showTreeNumButton
        |> div []


subTreeButtons : RangeSpec -> List (Html Msg)
subTreeButtons spec =
    let
        stats =
            spec
                |> RangeList.toDict
                |> dictToStats

        treeSummary =
            stats
                |> statsToSummary

        countList =
            treeSummary
                |> TreeSummary.toCountList

        buttons =
            countList
                |> List.map (RangeList.setN spec)
                |> List.map showTreeNumButton

        breakDowns =
            TreeSummary.arithmeticBreakdown treeSummary
                :: binaryBreakdown stats
    in
    if List.isEmpty buttons then
        []

    else
        Html.text "Subtrees: "
            :: buttons
            ++ breakDowns


toBinaryList : Int -> List Int
toBinaryList num =
    let
        f n powerOfTwo =
            if n == 0 then
                []

            else
                let
                    head =
                        (n |> modBy 2) * powerOfTwo

                    rest =
                        f (n // 2) (powerOfTwo * 2)
                in
                head :: rest
    in
    f num 1
        |> List.filter (\bit -> bit /= 0)
        |> List.reverse


binaryBreakdown : StatsTree -> List (Html Msg)
binaryBreakdown stats =
    case stats of
        BinaryTree.Empty ->
            []

        BinaryTree.Node data _ _ ->
            let
                blackCount =
                    2 ^ data.blackDepth - 1

                redCounts =
                    data.size
                        - blackCount
                        |> toBinaryList

                coloredText color text =
                    span [ style "color" color ] [ Html.text text ]

                redFrag n =
                    [ coloredText "blue" " + "
                    , coloredText "red" (String.fromInt n)
                    ]

                redBreakdown =
                    redCounts
                        |> List.map redFrag
                        |> List.concat

                content =
                    coloredText "blue" (String.fromInt data.size ++ " = ")
                        :: coloredText "black" (String.fromInt blackCount)
                        :: redBreakdown
            in
            div [] content
                |> List.singleton


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


specToStats : RangeSpec -> StatsTree
specToStats spec =
    spec
        |> RangeList.toDict
        |> dictToStats


treeDiagram : RangeSpec -> Html Msg
treeDiagram spec =
    spec
        |> specToStats
        |> TreeDiagram.diagramView getNodeColor getNodeText
