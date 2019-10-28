module MeSnippet exposing (testData)

import MeElmCode
import MeFloat
import MeInt
import MeList
import MeNumber
import MeRunTime
import MeTuple
import MeType


permuteFloats : MeType.Expr -> MeType.Expr
permuteFloats lst =
    MeType.PipeLine lst
        [ MeList.map MeInt.toFloat
        , MeList.map (MeType.Curry MeNumber.plus (MeFloat.init 0.5))
        ]


permuteFloatStrings =
    let
        inData =
            [ 1, 2, 3, 4, 5 ]

        inVal =
            inData
                |> MeList.initInts

        code =
            inVal
                |> permuteFloats
                |> MeElmCode.toElmCode

        outVal =
            inVal
                |> permuteFloats
                |> MeRunTime.computeVal

        outConvert =
            MeList.toList MeFloat.toFloat

        output =
            case outConvert outVal of
                Ok v ->
                    Debug.toString v

                Err err ->
                    err
    in
    [ code
    , output
    ]


normalize : MeType.Expr -> MeType.Expr
normalize lst =
    let
        one =
            MeInt.init 1

        incr =
            MeType.Var "incr" (MeType.Curry MeNumber.plus one)
    in
    MeType.PipeLine lst
        [ MeList.indexedMap MeTuple.pair
        , MeList.sortBy MeInt.toInt 0 MeTuple.second
        , MeList.map MeTuple.first
        , MeList.indexedMap MeTuple.pair
        , MeList.sortBy MeInt.toInt 0 MeTuple.second
        , MeList.map MeTuple.first
        , MeList.map incr
        ]


normalizeStrings =
    let
        inData =
            [ 99, 98, 97, 100, 101, 44, 42, 41 ]

        inVal =
            inData
                |> MeList.initInts

        code =
            inVal
                |> normalize
                |> MeElmCode.toElmCode

        outVal =
            inVal
                |> normalize
                |> MeRunTime.computeVal

        outConvert =
            MeList.toList MeInt.toInt

        output =
            case outConvert outVal of
                Ok v ->
                    Debug.toString v

                Err err ->
                    err
    in
    [ code
    , output
    ]


testData =
    [ normalizeStrings
    , permuteFloatStrings
    ]
