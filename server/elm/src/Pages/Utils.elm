module Pages.Utils exposing (..)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Date
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icons
import Maybe.Extra exposing (isJust, or, unwrap)
import Svg.Attributes
import Translate exposing (TranslationId, translate)


viewYearSelector : Language -> NominalDate -> Int -> (Int -> msg) -> Html msg
viewYearSelector language currentDate gap changeGapMsg =
    let
        currentYear =
            Date.year currentDate

        selectedYear =
            currentYear + gap

        backClass =
            if selectedYear == minYear then
                [ Svg.Attributes.class "hidden" ]

            else
                []

        forwardClass =
            if gap == 0 then
                [ Svg.Attributes.class "hidden" ]

            else
                []

        minYear =
            2018
    in
    div [ class "year-selector" ]
        [ Icons.iconBack <|
            (onClick <| changeGapMsg -1)
                :: backClass
        , span [ class "label" ] [ text <| String.fromInt selectedYear ]
        , Icons.iconForward <|
            (onClick <| changeGapMsg 1)
                :: forwardClass
        ]


resolveSelectedDateForMonthSelector : NominalDate -> Int -> NominalDate
resolveSelectedDateForMonthSelector currentDate monthGap =
    Date.add Date.Months (-1 * monthGap) currentDate


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


viewActionButton : Language -> TranslationId -> Bool -> msg -> Html msg
viewActionButton language label allowAction action =
    let
        attributes =
            if allowAction then
                [ class <| "ui fluid button"
                , onClick action
                ]

            else
                [ class <| "ui fluid button disabled" ]
    in
    div [ class "actions" ]
        [ button attributes
            [ text <| translate language label ]
        ]
