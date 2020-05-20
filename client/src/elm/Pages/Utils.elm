module Pages.Utils exposing (backFromSessionPage, filterDependentNoResultsMessage, matchFilter, matchMotherAndHerChildren, normalizeFilter, taskCompleted, taskListCompleted, viewBoolInput, viewCheckBoxMultipleSelectInput, viewCheckBoxSelectInput, viewCheckBoxSelectInputItem, viewCustomLabel, viewLabel, viewMeasurementInput, viewNameFilter, viewPhotoThumb, viewPhotoThumbFromPhotoUrl, viewPreviousMeasurement, viewQuestionLabel)

import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (OfflineSession)
import Backend.Session.Utils exposing (getChildren)
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

        viewInput value currentValue_ setMsg_ =
            let
                isChecked =
                    currentValue_ == Just value
            in
            input
                [ type_ "radio"
                , checked isChecked
                , classList [ ( "checked", isChecked ) ]
                , onCheck (always (setMsg_ value))
                ]
                []
    in
    div [ class <| "form-input yes-no " ++ inputClass ]
        [ div [ class "ui grid" ]
            [ div [ class <| inputWidth ++ " wide column" ]
                [ viewInput True currentValue setMsg
                , label [ onClick <| setMsg True ]
                    [ text <| translate language yesTransId ]
                ]
            , div [ class <| inputWidth ++ " wide column" ]
                [ viewInput False currentValue setMsg
                , label [ onClick <| setMsg False ]
                    [ text <| translate language noTransId ]
                ]
            ]
        ]


viewCheckBoxSelectInput : Language -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxSelectInput language leftOptions rightOptions currentValue setMsg translateFunc =
    let
        checkedOptions =
            currentValue |> Maybe.map List.singleton |> Maybe.withDefault []
    in
    viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions Nothing setMsg translateFunc


viewCheckBoxMultipleSelectInput : Language -> List a -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions noneOption setMsg translateFunc =
    let
        noneSection =
            noneOption
                |> unwrap
                    []
                    (\option ->
                        [ div [ class "ui divider" ] []
                        , viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc option
                        ]
                    )
    in
    div [ class "checkbox-select-input" ] <|
        div [ class "ui grid" ]
            [ leftOptions
                |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc)
                |> div [ class "eight wide column" ]
            , rightOptions
                |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc)
                |> div [ class "eight wide column" ]
            ]
            :: noneSection


viewCheckBoxSelectInputItem : Language -> List a -> (a -> msg) -> (a -> TranslationId) -> a -> Html msg
viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc option =
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
        , label []
            [ text <| translate language (translateFunc option) ]
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


{-| Show a photo thumbnail.
-}
viewPhotoThumb : String -> Html any
viewPhotoThumb url =
    div []
        [ img
            [ src url
            , class "ui small image orientation"
            ]
            []
        ]


viewPhotoThumbFromPhotoUrl : PhotoUrl -> Html any
viewPhotoThumbFromPhotoUrl (PhotoUrl url) =
    viewPhotoThumb url
