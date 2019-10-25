module RangeList exposing
    ( decr
    , flip
    , incr
    , setN
    , toDict
    , toList
    , toString
    )

import MyDict as Dict
    exposing
        ( Dict
        )
import Type
    exposing
        ( InsertionMode(..)
        , RangeSpec
        )


toList : RangeSpec -> List Int
toList spec =
    case spec.insertionMode of
        InsertForward ->
            List.range 1 spec.n

        InsertReverse ->
            List.range 1 spec.n
                |> List.reverse


toDict : RangeSpec -> Dict Int String
toDict spec =
    spec
        |> toList
        |> List.map (\n -> ( n, "" ))
        |> Dict.fromList


toString : RangeSpec -> String
toString spec =
    case spec.insertionMode of
        InsertForward ->
            "from 1 to " ++ String.fromInt spec.n ++ " (forward)"

        InsertReverse ->
            "from " ++ String.fromInt spec.n ++ " to 1 (reverse)"


flip : RangeSpec -> RangeSpec
flip spec =
    let
        newInsertionMode =
            case spec.insertionMode of
                InsertForward ->
                    InsertReverse

                InsertReverse ->
                    InsertForward
    in
    { spec
        | insertionMode = newInsertionMode
    }


setN : RangeSpec -> Int -> RangeSpec
setN spec n =
    { spec
        | n = n
    }


incr : RangeSpec -> RangeSpec
incr spec =
    { spec
        | n = spec.n + 1
    }


decr : RangeSpec -> RangeSpec
decr spec =
    { spec
        | n = spec.n - 1
    }
