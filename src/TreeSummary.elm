module TreeSummary exposing
    ( TreeSummary(..)
    , arithmeticBreakdown
    , description
    )

type TreeSummary
    = Broken
    | Nada
    | Tree1 Int
    | Tree2 Int Int
    | Tree3 Int Int Int


description : TreeSummary -> String
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


arithmeticBreakdown : TreeSummary -> String
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
