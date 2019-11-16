module PageTabs exposing (makeTabs)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


makeTabs : List ( String, msg ) -> Html msg
makeTabs tabConfigs =
    let
        makeTab ( label, cmd ) =
            Html.button
                [ onClick cmd ]
                [ Html.text label ]
    in
    tabConfigs
        |> List.map makeTab
        |> div [ style "padding" "10px" ]
