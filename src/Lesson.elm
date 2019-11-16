module Lesson exposing (view)

import Dict
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
import TreeTable
import Type
    exposing
        ( LessonPage(..)
        , Msg(..)
        , Page(..)
        )


view : LessonPage -> Html Msg
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
    , TreeTable.view allLists
    ]
        |> div []


nextButton : LessonPage -> Html Msg
nextButton lesson =
    Html.button
        [ onClick (SetPage (Lesson lesson))
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
    , TreeTable.view allLists
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
        fourLists =
            [ [ 1, 2, 3, 4 ]
            , [ 4, 3, 2, 1 ]
            ]

        fiveLists =
            fourLists
                |> nextGeneration

        topText =
            """
            Note that for n=5, we get two possible trees again.
            This time they aren't exactly symmetrical, but
            they're both the same depth.
        """

        bottomText =
            """
            If you look at the trees for n=4, maybe you can see which
            "missing" spots get filled in above.  (Don't be too
            distracted by red vs. black at this point.)
            The algorithm that we are looking at generally tries to
            lean trees toward the left.
        """
    in
    [ introText topText
    , TreeTable.view fiveLists
    , introText bottomText
    , TreeTable.view fourLists
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
                [ A1 MeList.map MeInt.toFloat
                ]

        newElements =
            PipeLine
                (VarName "startList")
                [ MeList.sort
                , A1
                    MeList.map
                    (F1 "n"
                        (Infix (VarName "n") MeNumber.plus (MeFloat.init 0.5))
                    )
                , F1 "items"
                    (Infix (MeFloat.init 0.5) MeList.cons (VarName "items"))
                ]
    in
    F1 "lst" <|
        LetIn
            [ ( "startList", startList )
            , ( "newElements", newElements )
            ]
            (PipeLine
                (VarName "newElements")
                [ A1 MeList.map MeList.singleton
                , A1 MeList.map
                    (F1 "x"
                        (Infix (VarName "startList") MeList.append (VarName "x"))
                    )
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
    let
        addOne =
            F1
                "n"
                (Infix (VarName "n") MeNumber.plus (MeInt.init 1))
    in
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A1 MeList.indexedMap MeTuple.pair
            , A1 MeList.sortBy MeTuple.second
            , A1 MeList.map MeTuple.first
            , A1 MeList.indexedMap MeTuple.pair
            , A1 MeList.sortBy MeTuple.second
            , A1 MeList.map MeTuple.first
            , A1 MeList.map addOne
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
