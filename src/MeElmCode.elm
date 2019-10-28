module MeElmCode exposing (toElmCode)

{--

Given an Expr written for the ME runtime, this
generates the equivalent Elm code.

This code is not meant to be executed--it's meant
more for documentation.  To actually evaluate the
Expr, use the ME runtime.

Of course, nothing is stopping you from running the
Elm code that gets generated here, but that would
require some tooling.
--}

import MeType exposing (..)


toElmCode : Expr -> String
toElmCode topExpr =
    let
        withParens s =
            "(" ++ s ++ ")"

        withoutParens s =
            s

        toCode parenWraper expr =
            case expr of
                Var name _ ->
                    -- TODO: make let statements
                    name

                Value name _ ->
                    name

                PipeLine a lst ->
                    a
                        :: lst
                        |> List.map (toCode withoutParens)
                        |> String.join "\n    |> "

                FunctionV name _ ->
                    name

                FunctionVV name _ ->
                    name

                Curry fvvExpr curryExpr ->
                    toCode withParens fvvExpr
                        ++ " "
                        ++ toCode withParens curryExpr
                        |> parenWraper

                ComposeF name exprF _ ->
                    name
                        ++ " "
                        ++ toCode withParens exprF
                        |> parenWraper

                _ ->
                    " ? "
    in
    toCode withoutParens topExpr
