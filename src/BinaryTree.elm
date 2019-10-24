module BinaryTree exposing
    ( BinaryTree(..)
    , height
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


height : BinaryTree v -> Int
height tree =
    case tree of
        Node _ left right ->
            1 + max (height left) (height right)

        Empty ->
            0
