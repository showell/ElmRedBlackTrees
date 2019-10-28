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

        ExtendList ->
            viewExtendList


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
            """
    in
    [ introText text
    , nextButton ExtendList
    , viewTreeTable allLists
    ]
        |> div []


viewExtendList : Html Msg
viewExtendList =
    let
        text1 =
            """
            If we want to extend our search trees from N=4
            to N=5, here is one way to make all possible extended
            lists, starting from [4, 3, 2, 1]:
            """

        startList =
            List.range 1 4
                |> List.reverse

        newLists =
            permuteIntList startList

        text2 =
            """
            Let's just normalize them, though:
            """

        niceLists =
            newLists
                |> List.map normalizedList
    in
    [ introText text1
    , newLists |> showLists
    , introText text2
    , niceLists |> showLists
    , Html.text "(under construction)"
    ]
        |> div []


permuteIntList : List Int -> List (List Float)
permuteIntList startList =
    -- assumes positive integers
    let
        startFloatList =
            startList
                |> List.map toFloat

        newElements =
            startList
                |> List.sort
                |> List.map toFloat
                |> List.map ((+) 0.5)
                |> (::) 0.5
    in
    newElements
        |> List.map List.singleton
        |> List.map ((++) startFloatList)


showLists : List a -> Html Msg
showLists lists =
    let
        listItem lst =
            lst
                |> Debug.toString
                |> Html.text
                |> List.singleton
                |> Html.li []
    in
    lists
        |> List.map listItem
        |> Html.ul []


normalizedList : List comparable -> List Int
normalizedList list =
    -- convert [ 150, 7, 42, 900] to [3, 1, 2, 4]
    list
        |> List.indexedMap Tuple.pair
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.indexedMap Tuple.pair
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.map ((+) 1)


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
