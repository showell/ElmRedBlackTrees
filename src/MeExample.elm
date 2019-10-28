module MeExample exposing (view)

import Html exposing (Html)
import Html.Attributes
    exposing
        ( style
        )
import MeRunTime
import MeSnippet


view : List (Html msg)
view =
    let
        makeTh : String -> Html msg
        makeTh s =
            s
                |> Html.text
                |> List.singleton
                |> Html.th []

        headings : Html msg
        headings =
            [ "code"
            , "output"
            ]
                |> List.map makeTh
                |> Html.tr []

        makeTd : String -> Html msg
        makeTd item =
            item
                |> Html.text
                |> List.singleton
                |> Html.pre []
                |> List.singleton
                |> Html.td []

        makeTr : List String -> Html msg
        makeTr items =
            items
                |> List.map makeTd
                |> Html.tr [ style "vertical-align" "top" ]
    in
    MeSnippet.testData
        |> List.map makeTr
        |> (::) headings
        |> Html.table []
        |> List.singleton