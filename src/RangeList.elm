module RangeList exposing
    ( decr
    , flip
    , fromSlug
    , incr
    , setN
    , toDict
    , toList
    , toSlug
    , toString
    )

import DictDotDot as Dict
    exposing
        ( Dict
        )
import Parser
    exposing
        ( (|.)
        , (|=)
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


fromSlug : String -> String -> Maybe RangeSpec
fromSlug prefix s =
    let
        makeSpec n mode =
            { n = n
            , insertionMode = mode
            }

        direction =
            Parser.oneOf
                [ Parser.map (\_ -> InsertForward) (Parser.keyword "forward")
                , Parser.map (\_ -> InsertReverse) (Parser.keyword "reverse")
                ]

        parser =
            Parser.succeed makeSpec
                |. Parser.keyword prefix
                |. Parser.symbol "/"
                |= Parser.int
                |. Parser.symbol "/"
                |= direction
    in
    case Parser.run parser s of
        Ok spec ->
            Just spec

        _ ->
            Nothing


toSlug : RangeSpec -> String
toSlug spec =
    let
        num =
            String.fromInt spec.n
    in
    case spec.insertionMode of
        InsertForward ->
            num ++ "/forward"

        InsertReverse ->
            num ++ "/reverse"


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
