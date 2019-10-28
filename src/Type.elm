module Type exposing
    ( InsertionMode(..)
    , Model
    , Msg(..)
    , Page(..)
    , RangeSpec
    , ShapeTree
    , SmallTreeLesson(..)
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


type SmallTreeLesson
    = AllFourTrees
    | SimplifiedFourTrees
    | ExtendList


type Page
    = Explorer RangeSpec
    | SmallTree SmallTreeLesson


type alias Model =
    { page : Page
    }


type Msg
    = ShowTree RangeSpec
    | SetPage Page
