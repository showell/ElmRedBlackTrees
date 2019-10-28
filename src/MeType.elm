module MeType exposing (..)


type V
    = VInt Int
    | VFloat Float
    | VTuple ( Expr, Expr )
    | VList (List Expr)
    | VError String


type alias FV =
    Context -> Expr -> V


type alias FVV =
    Context -> Expr -> Expr -> V


type Expr
    = Value String V
    | Var String Expr
    | ComputedValue V
    | FunctionV String FV
    | ComposeF String Expr FV
    | FunctionVV String FVV
    | PipeLine Expr (List Expr)
    | Curry Expr Expr
    | ExprError String


type alias Context =
    { vars : List Expr
    }
