module Components.ReportToWhatsAppDialog.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Components.ReportToWhatsAppDialog.Model exposing (DialogState(..), Model, Msg(..), PhoneData, ReportComponentAntenatal(..), ReportComponentNCD(..), ReportComponentWellChild(..), ReportComponentsConfig, ReportComponentsList(..), ReportType(..), emptyPhoneData)
import Components.ReportToWhatsAppDialog.Utils exposing (allCountryCodes, countryCodeToString, minimalNumberLength, siteToCountryCode, trimLeadingZeros)
import EverySet
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Utils exposing (viewCheckBoxMultipleSelectInput, viewCustomAction, viewTextInput)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate, translateText)
import Utils.Html exposing (viewCustomModal)


view : Language -> Site -> ( PersonId, Person ) -> ReportType -> Maybe (ReportComponentsConfig msg) -> Model -> Html (Msg msg)
view language site ( personId, person ) reportType componentsConfig model =
    viewCustomModal [ "bright" ] <|
        Maybe.map (viewDialog language site ( personId, person ) reportType componentsConfig) model.state


viewDialog : Language -> Site -> ( PersonId, Person ) -> ReportType -> Maybe (ReportComponentsConfig msg) -> DialogState -> Html (Msg msg)
viewDialog language site ( personId, person ) reportType componentsConfig state =
    let
        content =
            case state of
                Consent ->
                    viewConsent language person

                PhoneVerification phoneNumber ->
                    viewPhoneVerification language allowComponentsSelection phoneNumber

                PhoneInput data ->
                    viewPhoneInput language site data

                PhoneUpdateAtProfile phoneNumber ->
                    viewPhoneUpdateAtProfile language allowComponentsSelection personId person phoneNumber

                PhoneUpdateConfirmation phoneNumber ->
                    viewPhoneUpdateConfirmation language allowComponentsSelection phoneNumber

                ComponentsSelection phoneNumber componentsList ->
                    Maybe.map (viewComponentsSelection language phoneNumber componentsList reportType)
                        componentsConfig
                        |> Maybe.withDefault []

                ConfirmationBeforeExecuting phoneNumber ->
                    Maybe.map
                        (\config ->
                            config.setReportComponentsMsg Nothing
                        )
                        componentsConfig
                        |> viewConfirmationBeforeExecuting language site reportType personId phoneNumber

                ExecutionResult result ->
                    Maybe.map
                        (\config ->
                            config.setReportComponentsMsg Nothing
                        )
                        componentsConfig
                        |> viewExecutionResult language result

        allowComponentsSelection =
            isJust componentsConfig
    in
    div [ class "ui tiny active modal send-via-whatsapp" ]
        content


viewConsent : Language -> Person -> List (Html (Msg msg))
viewConsent language person =
    let
        nextState =
            Maybe.map
                (\number ->
                    let
                        trimmed =
                            String.trim number
                    in
                    if isJust (String.toInt trimmed) && String.length trimmed >= minimalNumberLength then
                        PhoneVerification trimmed

                    else
                        PhoneInput emptyPhoneData
                )
                person.telephoneNumber
                |> Maybe.withDefault (PhoneInput emptyPhoneData)
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.ReportToWhatsAppConsentQuestion ]
        , p [] [ text <| translate language Translate.ReportToWhatsAppNoticeOfNonRespobsibility ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState Nothing
                ]
                [ text <| translate language Translate.No ]
            , button
                [ class "ui primary fluid button"
                , onClick <| SetState <| Just nextState
                ]
                [ text <| translate language Translate.Yes ]
            ]
        ]
    ]


viewPhoneVerification : Language -> Bool -> String -> List (Html (Msg msg))
viewPhoneVerification language allowComponentsSelection phoneNumber =
    let
        nextStateForYes =
            if allowComponentsSelection then
                ComponentsSelection phoneNumber Nothing

            else
                ConfirmationBeforeExecuting phoneNumber
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.ReportToWhatsAppPhoneVerificationHeader ]
        , p [] [ text phoneNumber ]
        , p [] [ text <| translate language Translate.ReportToWhatsAppPhoneVerificationQuestion ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState <| Just (PhoneInput emptyPhoneData)
                ]
                [ text <| translate language Translate.No ]
            , button
                [ class "ui primary fluid button"
                , onClick <| SetState <| Just nextStateForYes
                ]
                [ text <| translate language Translate.Yes ]
            ]
        ]
    ]


viewPhoneInput : Language -> Site -> PhoneData -> List (Html (Msg msg))
viewPhoneInput language site data =
    let
        countryCodeOptions =
            List.map
                (\item ->
                    option
                        [ value <| countryCodeToString item
                        , selected <| data.countryCode == item
                        ]
                        [ text <| "+" ++ countryCodeToString item ]
                )
                allCountryCodes

        continueButtonAttributes =
            if String.length data.phone >= minimalNumberLength then
                let
                    curerntPhoneNumber =
                        if siteToCountryCode site == data.countryCode then
                            "0" ++ trimLeadingZeros data.phone

                        else
                            "+" ++ countryCodeToString data.countryCode ++ trimLeadingZeros data.phone
                in
                [ class "ui primary fluid button"
                , onClick <| SetState <| Just (PhoneUpdateAtProfile curerntPhoneNumber)
                ]

            else
                [ class "ui primary fluid button disabled" ]
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.ReportToWhatsAppPhoneInputHeader ]
        , div [ class "phone-inputs" ]
            [ select
                [ onInput SetCountryCode
                , class "select"
                ]
                countryCodeOptions
            , viewTextInput language
                data.phone
                SetPhoneNumber
                Nothing
                (Just "phone-number")
            ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState Nothing
                ]
                [ text <| translate language Translate.Cancel ]
            , button
                continueButtonAttributes
                [ text <| translate language Translate.Continue ]
            ]
        ]
    ]


viewPhoneUpdateAtProfile : Language -> Bool -> PersonId -> Person -> String -> List (Html (Msg msg))
viewPhoneUpdateAtProfile language allowComponentsSelection personId person phoneNumber =
    let
        nextStateForNo =
            if allowComponentsSelection then
                ComponentsSelection phoneNumber Nothing

            else
                ConfirmationBeforeExecuting phoneNumber
    in
    [ div [ class "content" ]
        [ translateText language Translate.ReportToWhatsAppPhoneUpdateAtProfileQuestionPrefix
        , span [] [ text person.name ]
        , translateText language Translate.ReportToWhatsAppPhoneUpdateAtProfileQuestionSuffix
        , span [] [ text phoneNumber ]
        , text "?"
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState <| Just nextStateForNo
                ]
                [ text <| translate language Translate.No ]
            , button
                [ class "ui primary fluid button"
                , onClick <| UpdatePhoneAtProfile personId person phoneNumber
                ]
                [ text <| translate language Translate.Yes ]
            ]
        ]
    ]


viewPhoneUpdateConfirmation : Language -> Bool -> String -> List (Html (Msg msg))
viewPhoneUpdateConfirmation language allowComponentsSelection phoneNumber =
    let
        nextState =
            if allowComponentsSelection then
                ComponentsSelection phoneNumber Nothing

            else
                ConfirmationBeforeExecuting phoneNumber
    in
    [ div [ class "content" ]
        [ translateText language Translate.ReportToWhatsAppPhoneUpdateConfirmationMessasge ]
    , viewCustomAction language (SetState <| Just nextState) False Translate.Continue
    ]


viewComponentsSelection :
    Language
    -> String
    -> Maybe ReportComponentsList
    -> ReportType
    -> ReportComponentsConfig msg
    -> List (Html (Msg msg))
viewComponentsSelection language phoneNumber componentsList reportType config =
    let
        componentsSelectionInput =
            case reportType of
                -- Not in use.
                ReportAcuteIllness ->
                    emptyNode

                ReportAntenatal ->
                    let
                        currentValue =
                            Maybe.map
                                (\list ->
                                    case list of
                                        Antenatal components ->
                                            EverySet.toList components

                                        -- We should never get here.
                                        _ ->
                                            []
                                )
                                componentsList
                                |> Maybe.withDefault []

                        setMsg =
                            \component ->
                                let
                                    currentComponents =
                                        EverySet.fromList currentValue

                                    updatedComponents =
                                        if EverySet.member component currentComponents then
                                            EverySet.remove component currentComponents

                                        else
                                            EverySet.insert component currentComponents
                                in
                                Antenatal updatedComponents
                                    |> Just
                                    |> ComponentsSelection phoneNumber
                                    |> Just
                                    |> SetState
                    in
                    viewCheckBoxMultipleSelectInput language
                        [ ComponentAntenatalObstetricHistory
                        , ComponentAntenatalMedicalHistory
                        , ComponentAntenatalMedicalDiagnosis
                        , ComponentAntenatalObstetricalDiagnosis
                        , ComponentAntenatalMedicationHistory
                        , ComponentAntenatalImmunizationHistory
                        , ComponentAntenatalCHWActivity
                        , ComponentAntenatalPatientProgress
                        , ComponentAntenatalLabsResults
                        , ComponentAntenatalProgressPhotos
                        ]
                        []
                        currentValue
                        Nothing
                        setMsg
                        Translate.ReportComponentAntenatal

                ReportNCD ->
                    let
                        currentValue =
                            Maybe.map
                                (\list ->
                                    case list of
                                        NCD components ->
                                            EverySet.toList components

                                        -- We should never get here.
                                        _ ->
                                            []
                                )
                                componentsList
                                |> Maybe.withDefault []

                        setMsg =
                            \component ->
                                let
                                    currentComponents =
                                        EverySet.fromList currentValue

                                    updatedComponents =
                                        if EverySet.member component currentComponents then
                                            EverySet.remove component currentComponents

                                        else
                                            EverySet.insert component currentComponents
                                in
                                NCD updatedComponents
                                    |> Just
                                    |> ComponentsSelection phoneNumber
                                    |> Just
                                    |> SetState
                    in
                    viewCheckBoxMultipleSelectInput language
                        [ ComponentNCDRiskFactors
                        , ComponentNCDActiveDiagnosis
                        , ComponentNCDMedicalDiagnosis
                        , ComponentNCDPatientProgress
                        , ComponentNCDLabsResults
                        ]
                        []
                        currentValue
                        Nothing
                        setMsg
                        Translate.ReportComponentNCD

                -- Not in use.
                ReportTuberculosis ->
                    emptyNode

                ReportWellChild ->
                    let
                        currentValue =
                            Maybe.map
                                (\list ->
                                    case list of
                                        WellChild components ->
                                            EverySet.toList components

                                        -- We should never get here.
                                        _ ->
                                            []
                                )
                                componentsList
                                |> Maybe.withDefault []

                        setMsg =
                            \component ->
                                let
                                    currentComponents =
                                        EverySet.fromList currentValue

                                    updatedComponents =
                                        if EverySet.member component currentComponents then
                                            EverySet.remove component currentComponents

                                        else
                                            EverySet.insert component currentComponents
                                in
                                WellChild updatedComponents
                                    |> Just
                                    |> ComponentsSelection phoneNumber
                                    |> Just
                                    |> SetState
                    in
                    viewCheckBoxMultipleSelectInput language
                        [ ComponentWellChildActiveDiagnoses
                        , ComponentWellChildImmunizationHistory
                        , ComponentWellChildECD
                        , ComponentWellChildGrowth
                        , ComponentWellChildNextAppointment
                        ]
                        []
                        currentValue
                        Nothing
                        setMsg
                        Translate.ReportComponentWellChild

        continueButtonAction =
            if componentsSelected then
                [ onClick <| SetReportComponents (config.setReportComponentsMsg componentsList) phoneNumber ]

            else
                []

        componentsSelected =
            Maybe.map
                (\list ->
                    case reportType of
                        -- Not in use.
                        ReportAcuteIllness ->
                            False

                        ReportAntenatal ->
                            case list of
                                Antenatal components ->
                                    not <| EverySet.isEmpty components

                                -- We should never get here.
                                _ ->
                                    False

                        ReportNCD ->
                            case list of
                                NCD components ->
                                    not <| EverySet.isEmpty components

                                -- We should never get here.
                                _ ->
                                    False

                        -- Not in use.
                        ReportTuberculosis ->
                            False

                        ReportWellChild ->
                            case list of
                                WellChild components ->
                                    not <| EverySet.isEmpty components

                                -- We should never get here.
                                _ ->
                                    False
                )
                componentsList
                |> Maybe.withDefault False
    in
    [ div [ class "content" ]
        [ p [] [ translateText language <| Translate.ReportToWhatsAppComponentsSelectionHeader reportType ]
        , componentsSelectionInput
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState Nothing
                ]
                [ text <| translate language Translate.Cancel ]
            , button
                (classList [ ( "ui primary fluid button", True ), ( "disabled", not componentsSelected ) ]
                    :: continueButtonAction
                )
                [ text <| translate language Translate.Continue ]
            ]
        ]
    ]


viewConfirmationBeforeExecuting : Language -> Site -> ReportType -> PersonId -> String -> Maybe msg -> List (Html (Msg msg))
viewConfirmationBeforeExecuting language site reportType personId phoneNumber clearComponentsMsg =
    let
        phoneNumberForWhatsApp =
            if String.startsWith "+" phoneNumber then
                -- International number with country code. Leave as is.
                phoneNumber

            else
                let
                    localCountryCode =
                        siteToCountryCode site
                in
                if String.startsWith "0" phoneNumber then
                    -- Local number with no country code.
                    -- Trim leading 0, add '+' and country code.
                    "+" ++ countryCodeToString localCountryCode ++ trimLeadingZeros phoneNumber

                else
                    -- Local numberm without leading 0.
                    -- Add '+' and country code.
                    "+" ++ countryCodeToString localCountryCode ++ phoneNumber
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.ReportToWhatsAppConfirmationBeforeExecutingHeader ]
        , p [] [ text phoneNumber ]
        , p [] [ text <| translate language Translate.ReportToWhatsAppConfirmationBeforeExecutingInstructions ]
        , p [] [ text <| translate language Translate.ReportToWhatsAppConfirmationBeforeExecutingQuestion ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| CancelExecute clearComponentsMsg
                ]
                [ text <| translate language Translate.No ]
            , button
                [ class "ui primary fluid button"
                , onClick <| Execute language reportType personId phoneNumberForWhatsApp
                ]
                [ text <| translate language Translate.Send ]
            ]
        ]
    ]


viewExecutionResult : Language -> Maybe String -> Maybe msg -> List (Html (Msg msg))
viewExecutionResult language maybeResult clearComponentsMsg =
    let
        ( message, actions ) =
            Maybe.map
                (\result ->
                    ( case result of
                        "success" ->
                            Translate.ReportToWhatsAppExecutionResultSuccess

                        "failure" ->
                            Translate.ReportToWhatsAppExecutionResultFailure

                        -- We should never get here, since proper responses are set at app.js.
                        _ ->
                            Translate.ReportToWhatsAppExecutionResultSomethingWentWrong
                    , viewCustomAction language (SetState Nothing) False Translate.Close
                    )
                )
                maybeResult
                |> Maybe.withDefault ( Translate.EmptyString, emptyNode )
    in
    [ div
        [ classList
            [ ( "content", True )
            , ( "hidden", isNothing maybeResult )
            ]
        , Html.Attributes.id "execution-response"
        , on "screenshotcomplete" (Json.Decode.map (SetExecutionResult clearComponentsMsg) (Json.Decode.at [ "detail", "result" ] Json.Decode.string))
        ]
        [ text <| translate language message ]
    , actions
    ]
