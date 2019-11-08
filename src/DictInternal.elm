module DictInternal exposing (toInternalRepresentation)

import MyDict exposing (..)


type alias InternalNode k v =
    { k : k
    , v : v
    , color : String
    , path : String
    }


{-| Reveal internal representation of Dict for advanced testing/debugging.
This returns a list of nodes in sorted order. Each node has k/v, plus
a path to the node that looks something like "llr", and the color of
the node ("R" or "B").
-}
toInternalRepresentation : Dict k v -> List (InternalNode k v)
toInternalRepresentation dict =
    let
        nodeList : String -> Dict k v -> List (InternalNode k v)
        nodeList path d =
            case d of
                RBNode_elm_builtin c k v left right ->
                    let
                        color =
                            case c of
                                Red ->
                                    "R"

                                Black ->
                                    "B"

                        node =
                            { k = k
                            , v = v
                            , color = color
                            , path = path
                            }
                    in
                    nodeList (path ++ "l") left
                        ++ [ node ]
                        ++ nodeList (path ++ "r") right

                _ ->
                    []
    in
    nodeList "" dict
