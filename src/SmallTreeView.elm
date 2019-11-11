module SmallTreeView exposing (view)

import Dict
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
import MeCodeGen
import MeFloat
import MeInt
import MeList
import MeNumber
import MeRunTime
import MeTuple
import MeType
    exposing
        ( Expr(..)
        )
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

        AllFiveTrees ->
            viewAllFiveTrees


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
            startList
                |> getPermutedLists

        text2 =
            """
            Let's just convert them to equivalent "ranked" lists:
            """

        niceLists =
            newLists
                |> List.map getRanks

        codeSamples =
            [ ( "permutedLists", permutedLists )
            , ( "ranks", ranks )
            ]
                |> Dict.fromList
                |> MeCodeGen.fromContext
                |> Html.text
                |> List.singleton
                |> Html.pre []
    in
    [ introText text1
    , newLists |> showLists
    , introText text2
    , niceLists |> showLists
    , nextButton AllFiveTrees
    , Html.hr [] []
    , Html.text "The above lists were produced with code like below:"
    , codeSamples
    ]
        |> div []


viewAllFiveTrees : Html Msg
viewAllFiveTrees =
    let
        allLists =
            [ [ 1, 2, 3, 4 ]
            , [ 4, 3, 2, 1 ]
            ]
                |> nextGeneration

        text =
            """
            Note that for n=5, we get two possible trees again.
            This time they aren't even exactly symmetrical, but
            they're both the same depth.
        """
    in
    [ introText text
    , viewTreeTable allLists
    ]
        |> div []


nextGeneration : List (List Int) -> List (List Int)
nextGeneration lsts =
    let
        f lst =
            lst
                |> getPermutedLists
                |> List.map getRanks
    in
    lsts
        |> List.map f
        |> List.concat


permutedLists : Expr
permutedLists =
    let
        startList =
            PipeLine
                (VarName "lst")
                [ F1 MeList.map MeInt.toFloat
                ]

        newElements =
            PipeLine
                (VarName "startList")
                [ MeList.sortFloat
                , F1 MeList.map (LambdaLeft "n" MeNumber.plus (MeFloat.init 0.5))
                , LambdaRight (MeFloat.init 0.5) MeList.cons "items"
                ]
    in
    Function [ "lst" ] <|
        LetIn
            [ ( "startList", startList )
            , ( "newElements", newElements )
            ]
            (PipeLine
                (VarName "newElements")
                [ F1 MeList.map MeList.singleton
                , F1 MeList.map
                    (LambdaRight (VarName "startList") MeList.plus "x")
                ]
            )


getPermutedLists : List Int -> List (List Float)
getPermutedLists lst =
    let
        context =
            [ ( "lst", MeList.initInts lst ) ]
                |> Dict.fromList

        toFloatList : Expr -> Result String (List Float)
        toFloatList lstExpr =
            lstExpr
                |> MeRunTime.getFinalValue
                |> MeList.toList MeFloat.toFloat
    in
    MeRunTime.compute context permutedLists
        |> MeRunTime.getFinalValue
        |> MeList.toList toFloatList
        |> Result.withDefault []


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


ranks : Expr
ranks =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ F1 MeList.indexedMap MeTuple.pair
            , F1 MeList.sortByFloat MeTuple.second
            , F1 MeList.map MeTuple.first
            , F1 MeList.indexedMap MeTuple.pair
            , F1 MeList.sortByInt MeTuple.second
            , F1 MeList.map MeTuple.first
            , F1 MeList.map (LambdaLeft "n" MeNumber.plus (MeInt.init 1))
            ]


getRanks : List Float -> List Int
getRanks lst =
    let
        context =
            [ ( "lst", MeList.initFloats lst ) ]
                |> Dict.fromList
    in
    MeRunTime.compute context ranks
        |> MeRunTime.getFinalValue
        |> MeList.toListInts
        |> Result.withDefault []


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
