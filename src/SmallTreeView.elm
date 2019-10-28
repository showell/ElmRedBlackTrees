module SmallTreeView exposing (view)

import DictHelper
import Html
    exposing
        ( Html
        , div
        )
import Html.Attributes exposing (style)
import Html.Events
    exposing
        ( onClick
        )
import List.Extra
import ListUtil
import StatsTreeDiagram
import Type
    exposing
        ( Msg(..)
        , Page(..)
        , SmallTreeLesson(..)
        )


view : SmallTreeLesson -> Html Msg
view lesson =
    case lesson of
        AllFourTrees ->
            viewAllFourTrees

        SimplifiedFourTrees ->
            viewSimplifiedFourTrees


viewAllFourTrees : Html Msg
viewAllFourTrees =
    let
        allLists =
            4
                |> List.range 1
                |> List.Extra.permutations

        text =
            """
            When you create RedBlack trees, the order that you add
            elements to the tree will influence the shape of the tree,
            depending on the algorithm.  In this app we are using the
            Elm implementation of Dict to generate the trees.

            Even though there are N! different ways to insert N
            elements into a search tree, you generally only get one
            or two possibilities.  Below we demonstrate this for
            the N=4 case. Let's simplify...
        """
    in
    [ introText text
    , nextButton SimplifiedFourTrees
    , viewTreeTable allLists
    ]
        |> div []


nextButton : SmallTreeLesson -> Html Msg
nextButton lesson =
    Html.button
        [ onClick (SetPage (SmallTree lesson))
        , style "margin-bottom" "5px"
        ]
        [ Html.text "Next" ]


introText : String -> Html Msg
introText text =
    text
        |> String.split "\n\n"
        |> List.map Html.text
        |> List.map List.singleton
        |> List.map (Html.p [])
        |> div [ style "max-width" "500px" ]


viewSimplifiedFourTrees : Html Msg
viewSimplifiedFourTrees =
    let
        allLists =
            [ [ 1, 2, 3, 4 ]
            , [ 4, 3, 2, 1 ]
            ]

        text =
            """
            At least for N=4, we can ignore all but two
            permutations of how we add elements to the list.
            Before inserting a fifth element, let's just
            consider the lists below (forward insertion and
            reverse insertion).

            (under construction)
            """
    in
    [ introText text
    , viewTreeTable allLists
    ]
        |> div []


viewTreeTable : List (List Int) -> Html Msg
viewTreeTable allLists =
    let
        treeTups =
            allLists
                |> List.map (\lst -> ( lst, DictHelper.listToStats lst ))

        groupTups =
            treeTups
                |> ListUtil.combineEqual DictHelper.eqStatsTree

        makeRow ( lists, stats ) =
            let
                listDiv lst =
                    lst
                        |> Debug.toString
                        |> Html.text
                        |> List.singleton
                        |> div []

                listCell =
                    lists
                        |> List.map listDiv
                        |> Html.td [ style "padding" "7px" ]

                diagramCell =
                    stats
                        |> StatsTreeDiagram.treeDiagram
                        |> List.singleton
                        |> div [ style "max-width" "500px" ]
                        |> List.singleton
                        |> Html.td
                            [ style "border-top" "1px solid blue"
                            , style "border-bottom" "1px solid blue"
                            , style "padding-top" "10px"
                            ]
            in
            [ listCell, diagramCell ]
                |> Html.tr [ style "vertical-align" "center" ]
    in
    groupTups
        |> List.map makeRow
        |> Html.table [ style "border-collapse" "collapse" ]
