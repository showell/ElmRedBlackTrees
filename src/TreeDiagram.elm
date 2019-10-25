module TreeDiagram exposing (diagramData, diagramView)

import BinaryTree
    exposing
        ( BinaryTree(..)
        )
import Html
    exposing
        ( Html
        )
import Svg
import Svg.Attributes


type alias CoordNode v =
    { coord : ( Int, Int )
    , data : v
    }


type alias Edge v =
    { parent : CoordNode v
    , child : CoordNode v
    }


type alias DiagramData v =
    { coordNodes : List (CoordNode v)
    , edges : List (Edge v)
    }


diagramView : BinaryTree v -> Html msg
diagramView tree =
    let
        data : DiagramData v
        data =
            diagramData tree

        drawNode : CoordNode v -> Html msg
        drawNode coordNode =
            let
                ( x, y ) =
                    coordNode.coord

                r =
                    5.0

                cx =
                    15.0 * toFloat x + r

                cy =
                    15.0 * toFloat y + r

                fill =
                    "blue"
            in
            Svg.circle
                [ Svg.Attributes.cx (String.fromFloat cx)
                , Svg.Attributes.cy (String.fromFloat cy)
                , Svg.Attributes.r (String.fromFloat r)
                , Svg.Attributes.fill fill
                ]
                []

        drawNodes : Html msg
        drawNodes =
            data.coordNodes
                |> List.map drawNode
                |> Svg.svg []
    in
    drawNodes


diagramData : BinaryTree v -> DiagramData v
diagramData tree =
    let
        edges parentCoordNode childCoordNodeTree childDiagram =
            case childCoordNodeTree of
                Empty ->
                    []

                Node childCoordNode _ _ ->
                    let
                        newEdge =
                            { parent = parentCoordNode
                            , child = childCoordNode
                            }
                    in
                    newEdge :: childDiagram.edges

        diagram coordsTree =
            case coordsTree of
                Empty ->
                    { coordNodes = []
                    , edges = []
                    }

                Node topCoordNode leftCoordTree rightCoordTree ->
                    let
                        leftDiagram =
                            diagram leftCoordTree

                        rightDiagram =
                            diagram rightCoordTree

                        leftEdges =
                            edges topCoordNode leftCoordTree leftDiagram

                        rightEdges =
                            edges topCoordNode rightCoordTree rightDiagram

                        newEdges =
                            leftEdges ++ rightEdges

                        newCoordNodes =
                            topCoordNode
                                :: (leftDiagram.coordNodes ++ rightDiagram.coordNodes)
                    in
                    { coordNodes = newCoordNodes
                    , edges = newEdges
                    }
    in
    diagram (toCoordsTree tree)


toCoordsTree : BinaryTree v -> BinaryTree (CoordNode v)
toCoordsTree tree =
    case tree of
        Node data left_ right_ ->
            let
                xOffset =
                    BinaryTree.size left_ + 1

                adjustLeft ( x, y ) =
                    ( x, y + 1 )

                adjustRight ( x, y ) =
                    ( x + xOffset, y + 1 )

                adjustLeftNode node =
                    { node
                        | coord = node.coord |> adjustLeft
                    }

                adjustRightNode node =
                    { node
                        | coord = node.coord |> adjustRight
                    }

                left =
                    left_
                        |> toCoordsTree
                        |> BinaryTree.map adjustLeftNode

                right =
                    right_
                        |> toCoordsTree
                        |> BinaryTree.map adjustRightNode

                coordNode =
                    { coord = ( xOffset, 0 )
                    , data = data
                    }
            in
            Node coordNode left right

        Empty ->
            Empty
