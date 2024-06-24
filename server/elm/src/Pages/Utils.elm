module Pages.Utils exposing (..)

import App.Types exposing (Language)
import Backend.Entities exposing (fromEntityId, toEntityId)
import Date
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icons
import Svg.Attributes
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (..)


viewYearSelector : NominalDate -> Int -> (Int -> msg) -> Html msg
viewYearSelector currentDate gap changeGapMsg =
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
            2023
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



-- Labels


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
    let
        transFunc =
            transId >> translate language

        optionsPairs =
            List.map
                (\option ->
                    ( transFunc option, option )
                )
                options
    in
    viewCustomSelectListInput currentValue
        optionsPairs
        toStringFunc
        setMsg
        inputClass
        True


viewCustomSelectListInput :
    Maybe a
    -> List ( String, a )
    -> (a -> String)
    -> (String -> msg)
    -> String
    -> Bool
    -> Html msg
viewCustomSelectListInput currentValue options toStringFunc setMsg inputClass withEmptyOption =
    let
        emptyOption =
            if withEmptyOption then
                emptySelectOption (currentValue == Nothing)

            else
                emptyNode
    in
    emptyOption
        :: List.map
            (\( label, value_ ) ->
                option
                    [ value (toStringFunc value_)
                    , selected (currentValue == Just value_)
                    ]
                    [ text label ]
            )
            options
        |> select
            [ onInput setMsg
            , class inputClass
            ]


viewGeoLocationSelectListInput :
    Language
    -> Maybe GeoLocationId
    -> List ( String, String )
    -> (String -> msg)
    -> TranslationId
    -> Bool
    -> Html msg
viewGeoLocationSelectListInput language currentValue options setMsg labelTransId disabled =
    let
        selectOptions =
            emptyOption
                :: List.map
                    (\option_ ->
                        let
                            isSelected =
                                Tuple.first option_
                                    |> String.toInt
                                    |> Maybe.map
                                        (\id ->
                                            currentValue == (Just <| toEntityId id)
                                        )
                                    |> Maybe.withDefault False
                        in
                        option
                            [ value <| Tuple.first option_
                            , selected isSelected
                            ]
                            [ text <| Tuple.second option_ ]
                    )
                    options

        emptyOption =
            emptySelectOption (currentValue == Nothing)
    in
    select
        [ onInput setMsg
        , class "select-input"
        ]
        selectOptions
        |> wrapSelectListInput language labelTransId disabled


wrapSelectListInput : Language -> TranslationId -> Bool -> Html msg -> Html msg
wrapSelectListInput language labelTransId disabled selectList =
    div
        [ classList
            [ ( "select-input-wrapper", True )
            , ( "disabled", disabled )
            ]
        ]
        [ viewLabel language labelTransId
        , selectList
        ]


emptySelectOption : Bool -> Html any
emptySelectOption isSelected =
    option
        [ value ""
        , selected isSelected
        ]
        [ text "" ]



-- Buttons


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


viewGenerateReportButton : Language -> String -> msg -> Html msg
viewGenerateReportButton language path selectionMadeMsg =
    viewMenuActionButton language path Translate.GenerateReport selectionMadeMsg


viewLoadDataButton : Language -> String -> msg -> Html msg
viewLoadDataButton language path selectionMadeMsg =
    viewMenuActionButton language path Translate.LoadData selectionMadeMsg


viewMenuActionButton : Language -> String -> TranslationId -> msg -> Html msg
viewMenuActionButton language path label selectionMadeMsg =
    a [ href path ]
        [ button [ onClick selectionMadeMsg ]
            [ text <| translate language label ]
        ]
