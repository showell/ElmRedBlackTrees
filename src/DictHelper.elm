module DictHelper exposing
    ( dictToStats
    , listToStats
    , statsToSummary
    )

import BinaryTree
    exposing
        ( BinaryTree(..)
        , withDefault
        )
import MyDict
    exposing
        ( Dict
        , fromList
        , toInternalRepresentation
        )
import TreeSummary
    exposing
        ( TreeSummary(..)
        )
import Type
    exposing
        ( StatsInfo
        , StatsTree
        )


type alias InternalNode k v =
    { k : k
    , v : v
    , color : String
    , path : String
    }


type alias ShapeTree =
    BinaryTree
        { color : String
        }


emptyStatsInfo : StatsInfo
emptyStatsInfo =
    { depth = 0
    , blackDepth = 0
    , size = 0
    , color = ""
    , sig = "_"
    }


statsToSummary : StatsTree -> TreeSummary
statsToSummary stats =
    case stats of
        BinaryTree.Empty ->
            Nada

        BinaryTree.Node data left right ->
            let
                size =
                    data.size

                lData =
                    left
                        |> withDefault emptyStatsInfo

                rData =
                    right
                        |> withDefault emptyStatsInfo

                lColor =
                    lData.color

                rColor =
                    rData.color

                lSize =
                    lData.size

                rSize =
                    rData.size
            in
            case ( lColor, rColor ) of
                ( _, "" ) ->
                    Tree1 size

                ( "B", "B" ) ->
                    Tree2 lSize rSize

                ( "R", "B" ) ->
                    case left of
                        Empty ->
                            Broken

                        Node _ ll lr ->
                            let
                                llSize =
                                    ll
                                        |> withDefault emptyStatsInfo
                                        |> .size

                                lrSize =
                                    lr
                                        |> withDefault emptyStatsInfo
                                        |> .size
                            in
                            Tree3 llSize lrSize rSize

                _ ->
                    Broken


listToDict : List comparable -> Dict comparable String
listToDict lst =
    lst
        |> List.map (\k -> ( k, "" ))
        |> fromList


dictToStats : Dict comparable v -> StatsTree
dictToStats dct =
    dct
        |> toInternalRepresentation
        |> toShapeTree
        |> shapeToStats


listToStats : List comparable -> StatsTree
listToStats lst =
    lst
        |> listToDict
        |> dictToStats


shapeToStats : ShapeTree -> StatsTree
shapeToStats shapeTree =
    case shapeTree of
        Empty ->
            Empty

        Node data_ left_ right_ ->
            let
                left =
                    left_ |> shapeToStats

                right =
                    right_ |> shapeToStats

                l =
                    left
                        |> withDefault emptyStatsInfo

                r =
                    right
                        |> withDefault emptyStatsInfo

                depth =
                    1 + min l.depth r.depth

                blackDepth =
                    1 + l.blackDepth

                size =
                    1 + l.size + r.size

                color =
                    data_.color

                sig =
                    color
                        ++ " ("
                        ++ l.sig
                        ++ ", "
                        ++ r.sig
                        ++ ")"

                data =
                    { depth = depth
                    , blackDepth = blackDepth
                    , size = size
                    , color = color
                    , sig = sig
                    }
            in
            Node data left right


toShapeTree : List (InternalNode k v) -> ShapeTree
toShapeTree internals =
    let
        top_ =
            internals
                |> List.filter (\node -> String.left 1 node.path == "")
                |> List.head
    in
    case top_ of
        Nothing ->
            Empty

        Just top ->
            let
                slicePath internalNode =
                    { internalNode
                        | path = String.dropLeft 1 internalNode.path
                    }

                getSubTree dir =
                    internals
                        |> List.filter (\node -> String.left 1 node.path == dir)
                        |> List.map slicePath
                        |> toShapeTree

                data =
                    { color = top.color
                    }
            in
            Node data (getSubTree "l") (getSubTree "r")
