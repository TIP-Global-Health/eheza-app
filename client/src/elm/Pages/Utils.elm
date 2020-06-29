module Pages.Utils exposing (backFromSessionPage, filterDependentNoResultsMessage, ifEverySetEmpty, isTaskCompleted, matchFilter, matchMotherAndHerChildren, normalizeFilter, taskCompleted, taskListCompleted, tasksBarId, viewBoolInput, viewCheckBoxMultipleSelectCustomInput, viewCheckBoxMultipleSelectInput, viewCheckBoxSelectCustomInput, viewCheckBoxSelectInput, viewCheckBoxSelectInputItem, viewCheckBoxValueInput, viewCheckBoxValueInputItem, viewCheckBoxValueInputNone, viewCustomLabel, viewEndEncounterDialog, viewEverySetInput, viewLabel, viewMeasurementInput, viewNameFilter, viewPhotoThumb, viewPhotoThumbFromPhotoUrl, viewPreviousMeasurement, viewQuestionLabel)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (OfflineSession)
import Backend.Session.Utils exposing (getChildren)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Translate exposing (Language, TranslationId, translate)


filterDependentNoResultsMessage : Language -> String -> TranslationId -> String
filterDependentNoResultsMessage language filter message =
    if String.isEmpty filter then
        translate language message

    else
        translate language Translate.NoMatchesFound


matchFilter : String -> String -> Bool
matchFilter filter filteredValue =
    if String.isEmpty filter then
        True

    else
        filteredValue
            |> String.toLower
            |> String.contains filter


matchMotherAndHerChildren : String -> OfflineSession -> PersonId -> Person -> Bool
matchMotherAndHerChildren filter offlineSession motherId mother =
    let
        motherContainsFilter =
            matchFilter filter mother.name

        -- A function, rather than value, to preserve the
        -- short-circuiting benefits of the `||` below.
        childrenContainsFilter _ =
            getChildren motherId offlineSession
                |> List.any
                    (\( _, child ) ->
                        matchFilter filter child.name
                    )
    in
    motherContainsFilter || childrenContainsFilter ()


normalizeFilter : String -> String
normalizeFilter filterInput =
    filterInput
        |> String.toLower
        |> String.trim


viewNameFilter : Language -> String -> (String -> msg) -> Html msg
viewNameFilter language filterInput setFilterMsg =
    div
        [ class "ui action input small" ]
        [ input
            [ placeholder <| translate language Translate.FilterByName
            , type_ "text"
            , onInput setFilterMsg
            , value filterInput
            ]
            []
        , button
            [ classList
                [ ( "ui button primary", True )
                , ( "disabled", String.isEmpty <| normalizeFilter filterInput )
                ]
            , onClick <| setFilterMsg ""
            ]
            [ text <| translate language Translate.Clear ]
        ]


backFromSessionPage : Nurse -> OfflineSession -> Page
backFromSessionPage nurse offlineSession =
    if isCommunityHealthWorker nurse then
        UserPage ClinicalPage

    else
        UserPage <| ClinicsPage (Just offlineSession.session.clinicId)


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


viewBoolInput :
    Language
    -> Maybe Bool
    -> (Bool -> msg)
    -> String
    -> Maybe ( TranslationId, TranslationId )
    -> Html msg
viewBoolInput language currentValue setMsg inputClass optionsTranslationIds =
    let
        ( yesTransId, noTransId ) =
            optionsTranslationIds |> Maybe.withDefault ( Translate.Yes, Translate.No )

        inputWidth =
            if isJust optionsTranslationIds then
                "eight"

            else
                "four"

        viewInput value =
            let
                isChecked =
                    currentValue == Just value

                transId =
                    if value then
                        yesTransId

                    else
                        noTransId
            in
            [ input
                [ type_ "radio"
                , checked isChecked
                , classList [ ( "checked", isChecked ) ]
                , onCheck (always (setMsg value))
                ]
                []
            , label [ onClick <| setMsg value ]
                [ text <| translate language transId ]
            ]
    in
    div [ class <| "form-input yes-no " ++ inputClass ]
        [ div [ class "ui grid" ]
            [ div [ class <| inputWidth ++ " wide column" ] <|
                viewInput True
            , div [ class <| inputWidth ++ " wide column" ] <|
                viewInput False
            ]
        ]


viewEverySetInput :
    Language
    -> EverySet a
    -> a
    -> (a -> msg)
    -> String
    -> Maybe ( TranslationId, TranslationId )
    -> Html msg
viewEverySetInput language currentValue sign setMsg inputClass optionsTranslationIds =
    let
        ( yesTransId, noTransId ) =
            optionsTranslationIds |> Maybe.withDefault ( Translate.Yes, Translate.No )

        inputWidth =
            if isJust optionsTranslationIds then
                "eight"

            else
                "four"

        viewInput value =
            let
                isChecked =
                    not (EverySet.isEmpty currentValue) && EverySet.member sign currentValue == value

                transId =
                    if value then
                        yesTransId

                    else
                        noTransId

                inputAttributes =
                    [ type_ "radio"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    ]

                labelAttributes =
                    if isChecked then
                        []

                    else
                        [ onClick <| setMsg sign ]
            in
            [ input inputAttributes []
            , label labelAttributes
                [ text <| translate language transId ]
            ]
    in
    div [ class <| "form-input yes-no " ++ inputClass ]
        [ div [ class "ui grid" ]
            [ div [ class <| inputWidth ++ " wide column" ] <|
                viewInput True
            , div [ class <| inputWidth ++ " wide column" ] <|
                viewInput False
            ]
        ]


viewCheckBoxSelectInput : Language -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxSelectInput language leftOptions rightOptions currentValue setMsg translateFunc =
    let
        checkedOptions =
            currentValue |> Maybe.map List.singleton |> Maybe.withDefault []
    in
    viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions Nothing setMsg translateFunc


viewCheckBoxSelectCustomInput : Language -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> Html msg) -> Html msg
viewCheckBoxSelectCustomInput language leftOptions rightOptions currentValue setMsg viewOptionFunc =
    let
        checkedOptions =
            currentValue |> Maybe.map List.singleton |> Maybe.withDefault []
    in
    viewCheckBoxMultipleSelectCustomInput language leftOptions rightOptions checkedOptions Nothing setMsg viewOptionFunc


viewCheckBoxMultipleSelectInput : Language -> List a -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions noneOption setMsg translateFunc =
    let
        viewOptionFunc option =
            label []
                [ translateFunc option |> translate language |> text ]
    in
    viewCheckBoxMultipleSelectCustomInput language leftOptions rightOptions checkedOptions noneOption setMsg viewOptionFunc


viewCheckBoxMultipleSelectCustomInput : Language -> List a -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> Html msg) -> Html msg
viewCheckBoxMultipleSelectCustomInput language leftOptions rightOptions checkedOptions noneOption setMsg viewOptionFunc =
    let
        noneSection =
            noneOption
                |> unwrap
                    []
                    (\option ->
                        [ div [ class "ui divider" ] []
                        , viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc option
                        ]
                    )

        ( leftOptionsClass, rightOptionsSection ) =
            if List.isEmpty rightOptions then
                ( "sixteen", emptyNode )

            else
                ( "eight"
                , rightOptions
                    |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc)
                    |> div [ class "eight wide column" ]
                )
    in
    div [ class "checkbox-select-input" ] <|
        div [ class "ui grid" ]
            [ leftOptions
                |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc)
                |> div [ class <| leftOptionsClass ++ " wide column" ]
            , rightOptionsSection
            ]
            :: noneSection


viewCheckBoxSelectInputItem : Language -> List a -> (a -> msg) -> (a -> Html msg) -> a -> Html msg
viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc option =
    let
        isChecked =
            List.member option checkedOptions
    in
    div
        [ class "ui checkbox activity"
        , onClick <| setMsg option
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , classList [ ( "checked", isChecked ) ]
            ]
            []
        , viewOptionFunc option
        ]


viewMeasurementInput : Language -> Maybe Float -> (String -> msg) -> String -> TranslationId -> Html msg
viewMeasurementInput language maybeCurrentValue setMsg inputClass unitTranslationId =
    let
        currentValue =
            maybeCurrentValue
                |> Maybe.map Debug.toString
                |> Maybe.withDefault ""

        inputAttrs =
            [ type_ "number"
            , Html.Attributes.min "0"
            , onInput setMsg
            , value currentValue
            ]
    in
    div [ class <| "form-input measurement " ++ inputClass ]
        [ input inputAttrs []
        , div [ class "unit" ]
            [ text <| translate language unitTranslationId ]
        ]


viewPreviousMeasurement : Language -> Maybe Float -> TranslationId -> Html any
viewPreviousMeasurement language maybePreviousValue unitTranslationId =
    let
        message =
            maybePreviousValue
                |> unwrap
                    (translate language Translate.PreviousMeasurementNotFound)
                    (\previousValue ->
                        (previousValue
                            |> Translate.PreviousFloatMeasurement
                            |> translate language
                        )
                            ++ " "
                            ++ translate language unitTranslationId
                    )
    in
    div [ class "previous-value" ] [ text message ]


viewCheckBoxValueInput : Language -> ( List a, a ) -> Dict a Int -> (a -> msg) -> (a -> String -> msg) -> (a -> TranslationId) -> List (Html msg)
viewCheckBoxValueInput language ( signs, none ) data toggleMsg setMsg translateFunc =
    let
        items =
            List.map (viewCheckBoxValueInputItem language data toggleMsg setMsg translateFunc) signs

        noneItem =
            [ viewCheckBoxValueInputNone language data toggleMsg translateFunc none ]
    in
    items ++ noneItem


viewCheckBoxValueInputItem : Language -> Dict a Int -> (a -> msg) -> (a -> String -> msg) -> (a -> TranslationId) -> a -> Html msg
viewCheckBoxValueInputItem language data toggleMsg setMsg translateFunc sign =
    let
        currentValue =
            Dict.get sign data

        isChecked =
            isJust currentValue

        periodSection =
            if isChecked then
                let
                    periodInput =
                        List.range 1 14
                            |> List.map
                                (\number ->
                                    option
                                        [ value (Debug.toString number)
                                        , selected (currentValue == Just number)
                                        ]
                                        [ text (Debug.toString number) ]
                                )
                            |> select [ onInput (setMsg sign), class "form-input period" ]
                in
                [ div [ class "three wide column" ] [ periodInput ]
                , div [ class "four wide column" ]
                    [ div [ class "days-present" ] [ text <| translate language Translate.DaysPresent ] ]
                ]

            else
                []
    in
    div [ class "ui grid" ] <|
        div [ class "eight wide column" ]
            [ div
                [ class "ui checkbox activity"
                , onClick <| toggleMsg sign
                ]
                [ input
                    [ type_ "checkbox"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    ]
                    []
                , label []
                    [ text <| translate language (translateFunc sign) ]
                ]
            ]
            :: periodSection


viewCheckBoxValueInputNone : Language -> Dict a Int -> (a -> msg) -> (a -> TranslationId) -> a -> Html msg
viewCheckBoxValueInputNone language data setMsg translateFunc noneSign =
    let
        currentValue =
            Dict.get noneSign data

        isChecked =
            isJust currentValue

        action =
            if isChecked then
                []

            else
                [ onClick <| setMsg noneSign ]
    in
    div [ class "ui grid" ]
        [ div
            [ class "seven wide column" ]
            [ div (class "ui checkbox activity" :: action)
                [ input
                    [ type_ "checkbox"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    ]
                    []
                , label []
                    [ text <| translate language (translateFunc noneSign) ]
                ]
            ]
        ]


viewEndEncounterDialog : Language -> TranslationId -> TranslationId -> msg -> msg -> Html msg
viewEndEncounterDialog language heading message confirmAction cancelAction =
    div [ class "ui tiny active modal" ]
        [ div [ class "header" ]
            [ text <| translate language heading ]
        , div
            [ class "content" ]
            [ p [] [ text <| translate language message ]
            ]
        , div
            [ class "actions" ]
            [ div [ class "two ui buttons" ]
                [ button
                    [ class "ui fluid button"
                    , onClick cancelAction
                    ]
                    [ text <| translate language Translate.Cancel ]
                , button
                    [ class "ui primary fluid button"
                    , onClick confirmAction
                    ]
                    [ text <| translate language Translate.Continue ]
                ]
            ]
        ]


taskCompleted : Maybe a -> Int
taskCompleted maybe =
    if isJust maybe then
        1

    else
        0


taskListCompleted : List (Maybe a) -> Int
taskListCompleted list =
    if List.all isJust list then
        1

    else
        0


ifEverySetEmpty : a -> EverySet a -> EverySet a
ifEverySetEmpty value set =
    if EverySet.isEmpty set then
        EverySet.singleton value

    else
        set


{-| Show a photo thumbnail.
-}
viewPhotoThumb : String -> Html any
viewPhotoThumb url =
    div []
        [ img
            [ src url
            , class "ui small image rotate-90"
            ]
            []
        ]


viewPhotoThumbFromPhotoUrl : PhotoUrl -> Html any
viewPhotoThumbFromPhotoUrl (PhotoUrl url) =
    viewPhotoThumb url


isTaskCompleted : Dict t ( Int, Int ) -> t -> Bool
isTaskCompleted dict task =
    Dict.get task dict
        |> Maybe.map (\( completed, total ) -> completed == total)
        |> Maybe.withDefault False


tasksBarId : String
tasksBarId =
    "tasks-bar"
