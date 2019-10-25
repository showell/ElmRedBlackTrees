module Type exposing
    ( InsertionMode(..)
    , Model
    , Msg(..)
    , RangeSpec
    )


type InsertionMode
    = InsertForward
    | InsertReverse


type alias RangeSpec =
    { n : Int
    , insertionMode : InsertionMode
    }


type alias Model =
    { rangeSpec : RangeSpec
    }


type Msg
    = ShowTree RangeSpec
