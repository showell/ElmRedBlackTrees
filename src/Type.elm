module Type exposing
    ( InsertionMode(..)
    , Model
    , Msg(..)
    , RangeSpec
    , StatsInfo
    , StatsTree
    )

import BinaryTree
    exposing
        ( BinaryTree
        )


type alias StatsInfo =
    { depth : Int
    , blackDepth : Int
    , size : Int
    , color : String
    , sig : String
    }


type alias StatsTree =
    BinaryTree StatsInfo


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
