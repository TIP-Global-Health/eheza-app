module Pages.Utils exposing (..)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Date
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, or, unwrap)
import Translate exposing (TranslationId, translate)


viewLabel : Language -> TranslationId -> Html any
viewLabel language translationId =
    viewCustomLabel language translationId ":" "label"


viewQuestionLabel : Language -> TranslationId -> Html any
viewQuestionLabel language translationId =
    viewCustomLabel language translationId "?" "label"


viewCustomLabel : Language -> TranslationId -> String -> String -> Html any
viewCustomLabel language translationId suffix class_ =
    div [ class class_ ] [ text <| (translate language translationId ++ suffix) ]



-- Inputs


viewSelectListInput :
    Language
    -> Maybe a
    -> List a
    -> (a -> String)
    -> (String -> msg)
    -> (a -> TranslationId)
    -> String
    -> Html msg
viewSelectListInput language currentValue options toStringFunc setMsg transId inputClass =
    viewCustomSelectListInput currentValue
        options
        toStringFunc
        setMsg
        (transId >> translate language)
        ("form-input " ++ inputClass)
        True


viewCustomSelectListInput :
    Maybe a
    -> List a
    -> (a -> String)
    -> (String -> msg)
    -> (a -> String)
    -> String
    -> Bool
    -> Html msg
viewCustomSelectListInput currentValue options toStringFunc setMsg transFunc inputClass withEmptyOption =
    let
        emptyOption =
            if withEmptyOption then
                emptySelectOption (currentValue == Nothing)

            else
                emptyNode
    in
    emptyOption
        :: List.map
            (\option_ ->
                option
                    [ value (toStringFunc option_)
                    , selected (currentValue == Just option_)
                    ]
                    [ text <| transFunc option_ ]
            )
            options
        |> select
            [ onInput setMsg
            , class inputClass
            ]


emptySelectOption : Bool -> Html any
emptySelectOption isSelected =
    option
        [ value ""
        , selected isSelected
        ]
        [ text "" ]


viewEncounterActionButton : Language -> TranslationId -> String -> Bool -> msg -> Html msg
viewEncounterActionButton language label buttonColor allowAction action =
    let
        attributes =
            if allowAction then
                [ class <| "ui fluid button " ++ buttonColor
                , onClick action
                ]

            else
                [ class <| "ui fluid button disabled " ++ buttonColor ]
    in
    div [ class "actions" ]
        [ button attributes
            [ text <| translate language label ]
        ]
