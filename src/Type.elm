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
import Browser.Navigation as Navigation
import Url
    exposing
        ( Url
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
    | AllFiveTrees


type Page
    = Explorer RangeSpec
    | SmallTree SmallTreeLesson


type alias Model =
    { page : Page
    , key : Navigation.Key
    }


type Msg
    = ShowTree RangeSpec
    | SetPage Page
    | UrlRequested
    | UrlChanged Url
