module GenerateLists exposing
    ( codeSamples
    , getPermutedLists
    , getRanks
    , nextGeneration
    , showLists
    )

import Dict
import Html exposing (Html)
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


codeSamples : Html msg
codeSamples =
    [ ( "permutedLists", permutedLists )
    , ( "ranks", ranks )
    ]
        |> Dict.fromList
        |> MeCodeGen.fromContext
        |> Html.text
        |> List.singleton
        |> Html.pre []


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


showLists : List a -> Html msg
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
