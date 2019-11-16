module Lesson exposing (view)

import GenerateLists
import Html
    exposing
        ( Html
        , div
        )
import List.Extra
import TreeTable
import Type
    exposing
        ( LessonPage(..)
        , Msg(..)
        , Page(..)
        )
import Widget
    exposing
        ( introText
        , nextButton
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
                |> GenerateLists.getPermutedLists

        text2 =
            """
            Let's just convert them to equivalent "ranked" lists:
            """

        niceLists =
            newLists
                |> List.map GenerateLists.getRanks
    in
    [ introText text1
    , newLists |> GenerateLists.showLists
    , introText text2
    , niceLists |> GenerateLists.showLists
    , nextButton AllFiveTrees
    , Html.hr [] []
    , Html.text "The above lists were produced with code like below:"
    , GenerateLists.codeSamples
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
                |> GenerateLists.nextGeneration

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
