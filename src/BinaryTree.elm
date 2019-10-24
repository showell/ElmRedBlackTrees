module BinaryTree exposing
    ( BinaryTree(..)
    , withDefault
    )


type BinaryTree v
    = Node v (BinaryTree v) (BinaryTree v)
    | Empty


withDefault : v -> BinaryTree v -> v
withDefault default tree =
    case tree of
        Node data _ _ ->
            data

        Empty ->
            default
