module Widget exposing
    ( introText
    , nextButton
    )

import Html
    exposing
        ( Html
        , div
        )
import Html.Attributes exposing (style)
import Html.Events
    exposing
        ( onClick
        )
import Type
    exposing
        ( LessonPage(..)
        , Msg(..)
        , Page(..)
        )


nextButton : LessonPage -> Html Msg
nextButton lesson =
    Html.button
        [ onClick (SetPage (Lesson lesson))
        , style "margin-bottom" "5px"
        ]
        [ Html.text "Next" ]


introText : String -> Html msg
introText text =
    text
        |> String.split "\n\n"
        |> List.map Html.text
        |> List.map List.singleton
        |> List.map (Html.p [])
        |> div [ style "max-width" "500px" ]
