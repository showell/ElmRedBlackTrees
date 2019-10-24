module BinaryTree exposing
    ( BinaryTree(..)
    , height
    , map
    , size
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


size : BinaryTree v -> Int
size tree =
    case tree of
        Node _ left right ->
            1 + size left + size right

        Empty ->
            0


height : BinaryTree v -> Int
height tree =
    case tree of
        Node _ left right ->
            1 + max (height left) (height right)

        Empty ->
            0


map : (v -> w) -> BinaryTree v -> BinaryTree w
map f tree =
    case tree of
        Node v left right ->
            Node (f v) (map f left) (map f right)

        Empty ->
            Empty
