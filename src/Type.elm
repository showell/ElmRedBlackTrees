module Type exposing (Model, Msg(..))


type alias Model =
    { activeTreeSize : Int
    }


type Msg
    = ShowTree Int
