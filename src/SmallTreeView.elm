module SmallTreeView exposing (view)

import DictHelper
import Html
    exposing
        ( Html
        , div
        )
import Html.Attributes exposing (style)
import List.Extra
import ListUtil
import StatsTreeDiagram
import Type exposing (Msg)


view : Html Msg
view =
    let
        allLists =
            4
                |> List.range 1
                |> List.Extra.permutations

        treeTups =
            allLists
                |> List.map (\lst -> ( lst, DictHelper.listToStats lst ))

        groupTups =
            treeTups
                |> ListUtil.combineEqual DictHelper.eqStatsTree

        makeRow ( lists, stats ) =
            let
                listDiv lst =
                    lst
                        |> Debug.toString
                        |> Html.text
                        |> List.singleton
                        |> div []

                listCell =
                    lists
                        |> List.map listDiv
                        |> Html.td [ style "padding" "7px" ]

                diagramCell =
                    stats
                        |> StatsTreeDiagram.treeDiagram
                        |> List.singleton
                        |> div [ style "max-width" "500px" ]
                        |> List.singleton
                        |> Html.td
                            [ style "border-top" "1px solid blue"
                            , style "border-bottom" "1px solid blue"
                            ]
            in
            [ listCell, diagramCell ]
                |> Html.tr [ style "vertical-align" "center" ]
    in
    groupTups
        |> List.map makeRow
        |> Html.table [ style "border-collapse" "collapse" ]
