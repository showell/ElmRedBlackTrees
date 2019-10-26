module Type exposing
    ( InsertionMode(..)
    , Model
    , Msg(..)
    , RangeSpec
    , ShapeTree
    , StatsInfo
    , StatsTree
    )

import BinaryTree
    exposing
        ( BinaryTree
        )


type alias ShapeTree =
    BinaryTree
        { color : String
        , n : Int
        }


type alias StatsInfo =
    { depth : Int
    , blackDepth : Int
    , size : Int
    , color : String
    , sig : String
    , n : Int
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
