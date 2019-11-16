module TreeTable exposing (view)

import DictHelper
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import ListUtil
import StatsTreeDiagram


view : List (List Int) -> Html msg
view allLists =
    let
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
                            , style "padding-top" "10px"
                            ]
            in
            [ listCell, diagramCell ]
                |> Html.tr [ style "vertical-align" "center" ]
    in
    groupTups
        |> List.map makeRow
        |> Html.table [ style "border-collapse" "collapse" ]
