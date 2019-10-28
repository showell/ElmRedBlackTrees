module MeList exposing (..)

import MeInt
import MeRunTime exposing (..)
import MeType exposing (..)


toList : (V -> Result String a) -> V -> Result String (List a)
toList getItem listValue =
    let
        get : Expr -> Result String a
        get elem =
            case getFinalValue elem of
                Ok v ->
                    getItem v

                Err s ->
                    Err s

        getItems : List Expr -> Result String (List a)
        getItems items =
            case items of
                [] ->
                    Ok []

                headV :: restV ->
                    case ( get headV, getItems restV ) of
                        ( Ok head, Ok rest ) ->
                            Ok (head :: rest)

                        ( Err s, _ ) ->
                            Err ("item is malformed: " ++ s)

                        ( _, Err s ) ->
                            Err s
    in
    case listValue of
        VList items ->
            getItems items

        _ ->
            Err "this is not a list value"


indexedMap : Expr -> Expr
indexedMap mapperExpr =
    let
        happy_path : List Expr -> (Expr -> Expr -> V) -> V
        happy_path lst mapper =
            let
                wrapped_mapper idx item =
                    let
                        idxExpr =
                            ComputedValue (VInt idx)
                    in
                    mapper idxExpr item
            in
            lst
                |> List.indexedMap wrapped_mapper
                |> List.map ComputedValue
                |> VList

        f c v =
            case ( computeV c v, getFuncVV c mapperExpr ) of
                ( VList lst, Just mapper ) ->
                    happy_path lst (mapper c)

                _ ->
                    VError "indexedMap has wrong types"
    in
    ComposeF "List.indexedMap" mapperExpr f


sortBy : (V -> Result String comparable1) -> comparable1 -> Expr -> Expr
sortBy unbox default ordExpr =
    let
        happy_path : List Expr -> (Expr -> V) -> V
        happy_path lst ord =
            let
                rawOrd item =
                    case unbox (ord item) of
                        Ok data ->
                            data

                        _ ->
                            default
            in
            lst
                |> List.sortBy rawOrd
                |> VList

        f : FV
        f c expr =
            case ( computeV c expr, getFuncV c ordExpr ) of
                ( VList lst, Just ord ) ->
                    happy_path lst (ord c)

                _ ->
                    VError "sortBy has wrong types"
    in
    ComposeF "List.sortBy" ordExpr f


map : Expr -> Expr
map mapperExpr =
    let
        happy_path : List Expr -> (Expr -> V) -> V
        happy_path lst mapper =
            lst
                |> List.map mapper
                |> List.map ComputedValue
                |> VList

        f c expr =
            case ( computeV c expr, getFuncV c mapperExpr ) of
                ( VList lst, Just mapper ) ->
                    happy_path lst (mapper c)

                _ ->
                    VError "map has wrong types"
    in
    ComposeF "List.map" mapperExpr f


initInts : List Int -> Expr
initInts lst =
    let
        v =
            lst
                |> List.map MeInt.init
                |> VList

        repr =
            lst
                |> List.map String.fromInt
                |> String.join ", "
                |> (\s -> "[" ++ s ++ "]")
    in
    Value repr v
