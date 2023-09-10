module Visible exposing
    ( Visible(..)
    , view
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events


type Visible
    = Visible
    | Hidden


view : { label : String, onChange : Visible -> msg, value : Visible } -> Html msg
view { onChange, value, label } =
    Html.label []
        [ Html.text label
        , Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked <|
                case value of
                    Visible ->
                        True

                    Hidden ->
                        False
            , Html.Events.onCheck
                (\isChecked ->
                    onChange <|
                        if isChecked then
                            Visible

                        else
                            Hidden
                )
            ]
            []
        ]
