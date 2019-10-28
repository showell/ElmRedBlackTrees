module MeRunTime exposing (..)

import MeType exposing (..)


makeContext : Context
makeContext =
    { vars = []
    }


computeVal : Expr -> V
computeVal expr =
    let
        context =
            makeContext
    in
    computeV context expr


computeV : FV
computeV context expr =
    case expr of
        Var _ v ->
            computeV context v

        ComputedValue v ->
            v

        Value _ v ->
            v

        PipeLine topExpr lst ->
            evalPipeLine context topExpr lst

        _ ->
            VError "cannot evaluate funcs without arguments"


getFuncV : Context -> Expr -> Maybe FV
getFuncV context expr =
    case expr of
        Var _ v ->
            getFuncV context v

        FunctionV _ f ->
            Just f

        ComposeF _ _ f ->
            Just f

        Curry curryExpr op1Expr ->
            case curryExpr of
                FunctionVV _ fvv ->
                    let
                        fv c op2Expr =
                            fvv c op1Expr op2Expr
                    in
                    Just fv

                _ ->
                    Nothing

        _ ->
            Nothing


getFuncVV : Context -> Expr -> Maybe FVV
getFuncVV context expr =
    case expr of
        FunctionVV _ fvv ->
            Just fvv

        _ ->
            Nothing


evalPipeLine : Context -> Expr -> List Expr -> V
evalPipeLine context v lst =
    case lst of
        [] ->
            computeV context v

        head :: rest ->
            case getFuncV context head of
                Just f ->
                    let
                        newV =
                            f context v
                    in
                    evalPipeLine context (ComputedValue newV) rest

                Nothing ->
                    VError "expecting function"


getFinalValue : Expr -> Result String V
getFinalValue expr =
    case expr of
        ComputedValue v ->
            Ok v

        Value _ v ->
            Ok v

        _ ->
            Err "final values were never computed with computeVal"
