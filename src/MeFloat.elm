module MeFloat exposing (..)

import MeRunTime
    exposing
        ( computeVal
        )
import MeType exposing (..)


init : Float -> Expr
init num =
    Value (String.fromFloat num) (VFloat num)


toFloat : V -> Result String Float
toFloat v =
    case v of
        VFloat n ->
            Ok n

        _ ->
            Err "not an int"
