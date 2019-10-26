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


diagramView : (v -> String) -> (v -> String) -> BinaryTree v -> Html msg
diagramView getNodeColor getNodeText tree =
    let
        data : DiagramData v
        data =
            diagramData tree

        size =
            BinaryTree.size tree
                |> toFloat

        w =
            100.0 / (size + 2)

        r =
            w / 2.5

        fontSize =
            r * 0.7

        strokeWidth =
            r / 30.0

        scaleCoord ( x, y ) =
            let
                xx =
                    w * toFloat x + r

                yy =
                    w * toFloat y + r
            in
            ( xx, yy )

        drawEdge : Edge v -> Html msg
        drawEdge edge =
            let
                ( x1, y1 ) =
                    scaleCoord edge.parent.coord

                ( x2, y2 ) =
                    scaleCoord edge.child.coord

                stroke =
                    "gray"
            in
            Svg.line
                [ Svg.Attributes.x1 (String.fromFloat x1)
                , Svg.Attributes.y1 (String.fromFloat y1)
                , Svg.Attributes.x2 (String.fromFloat x2)
                , Svg.Attributes.y2 (String.fromFloat y2)
                , Svg.Attributes.stroke stroke
                , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                ]
                []

        drawEdges : List (Html msg)
        drawEdges =
            data.edges
                |> List.map drawEdge

        drawCoordNode : CoordNode v -> Html msg
        drawCoordNode coordNode =
            let
                ( cx, cy ) =
                    scaleCoord coordNode.coord

                fill =
                    getNodeColor coordNode.data

                circle =
                    Svg.circle
                        [ Svg.Attributes.cx (String.fromFloat cx)
                        , Svg.Attributes.cy (String.fromFloat cy)
                        , Svg.Attributes.r (String.fromFloat r)
                        , Svg.Attributes.fill fill
                        ]
                        []

                text =
                    getNodeText coordNode.data

                textAnchor =
                    "middle"

                textFill =
                    "white"

                x =
                    cx

                y =
                    cy + (fontSize / 4)

                label =
                    Svg.text_
                        [ Svg.Attributes.x (String.fromFloat x)
                        , Svg.Attributes.y (String.fromFloat y)
                        , Svg.Attributes.fontSize (String.fromFloat fontSize)
                        , Svg.Attributes.fill textFill
                        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                        , Svg.Attributes.textAnchor textAnchor
                        ]
                        [ Svg.text text
                        ]
            in
            Svg.g [] [ circle, label ]

        drawCoordNodes : List (Html msg)
        drawCoordNodes =
            data.coordNodes
                |> List.map drawCoordNode
    in
    drawEdges
        ++ drawCoordNodes
        |> Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.viewBox "0 0 100 75"
            ]


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
