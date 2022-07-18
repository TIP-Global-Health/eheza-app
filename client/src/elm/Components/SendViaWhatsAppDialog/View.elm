module Components.SendViaWhatsAppDialog.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Components.SendViaWhatsAppDialog.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Utils exposing (viewCheckBoxMultipleSelectInput, viewTextInput)
import Translate exposing (Language, translate, translateText)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> ( PersonId, Person ) -> Maybe (ReportComponentsConfig msg) -> Model -> Html (Msg msg)
view language currentDate ( personId, person ) componentsConfig model =
    viewModal <|
        Maybe.map (viewDialog language currentDate ( personId, person ) componentsConfig) model.state


viewDialog : Language -> NominalDate -> ( PersonId, Person ) -> Maybe (ReportComponentsConfig msg) -> DialogState -> Html (Msg msg)
viewDialog language currentDate ( personId, person ) componentsConfig state =
    let
        content =
            case state of
                Consent ->
                    viewConsent language currentDate person

                PhoneVerification phoneNumber ->
                    viewPhoneVerification language currentDate allowComponentsSelection phoneNumber

                PhoneInput inputValue ->
                    viewPhoneInput language currentDate inputValue

                PhoneUpdateAtProfile phoneNumber ->
                    viewPhoneUpdateAtProfile language currentDate allowComponentsSelection personId person phoneNumber

                PhoneUpdateConfirmation phoneNumber ->
                    viewPhoneUpdateConfirmation language currentDate allowComponentsSelection phoneNumber

                ComponentsSelection phoneNumber componentsList ->
                    Maybe.map (viewComponentsSelection language currentDate phoneNumber componentsList)
                        componentsConfig
                        |> Maybe.withDefault []

                ConfirmationBeforeSending phoneNumber ->
                    viewConfirmationBeforeSending language currentDate phoneNumber

        allowComponentsSelection =
            isJust componentsConfig
    in
    div [ class "ui tiny active modal send-via-whatsapp" ]
        content


viewConsent : Language -> NominalDate -> Person -> List (Html (Msg msg))
viewConsent language currentDate person =
    let
        nextState =
            Maybe.map
                (\number ->
                    if not <| String.isEmpty number then
                        PhoneVerification number

                    else
                        PhoneInput ""
                )
                person.telephoneNumber
                |> Maybe.withDefault (PhoneInput "")
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppConsentQuestion ]
        , p [] [ text <| translate language Translate.SendViaWhatsAppNoticeOfNonRespobsibility ]
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


viewPhoneVerification : Language -> NominalDate -> Bool -> String -> List (Html (Msg msg))
viewPhoneVerification language currentDate allowComponentsSelection phoneNumber =
    let
        nextStateForYes =
            if allowComponentsSelection then
                ComponentsSelection phoneNumber Nothing

            else
                ConfirmationBeforeSending phoneNumber
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppPhoneVerificationHeader ]
        , p [] [ text phoneNumber ]
        , p [] [ text <| translate language Translate.SendViaWhatsAppPhoneVerificationQuestion ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState <| Just (PhoneInput "")
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


viewPhoneInput : Language -> NominalDate -> String -> List (Html (Msg msg))
viewPhoneInput language currentDate inputValue =
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppPhoneInputHeader ]
        , viewTextInput language
            inputValue
            (PhoneInput >> Just >> SetState)
            Nothing
            (Just "phone-number")
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState Nothing
                ]
                [ text <| translate language Translate.Cancel ]
            , button
                [ class "ui primary fluid button"
                , onClick <| SetState <| Just (PhoneUpdateAtProfile inputValue)
                ]
                [ text <| translate language Translate.Continue ]
            ]
        ]
    ]


viewPhoneUpdateAtProfile : Language -> NominalDate -> Bool -> PersonId -> Person -> String -> List (Html (Msg msg))
viewPhoneUpdateAtProfile language currentDate allowComponentsSelection personId person phoneNumber =
    let
        nextStateForNo =
            if allowComponentsSelection then
                ComponentsSelection phoneNumber Nothing

            else
                ConfirmationBeforeSending phoneNumber
    in
    [ div [ class "content" ]
        [ translateText language Translate.SendViaWhatsAppPhoneUpdateAtProfileQuestionPrefix
        , span [] [ text person.name ]
        , translateText language Translate.SendViaWhatsAppPhoneUpdateAtProfileQuestionSuffix
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


viewPhoneUpdateConfirmation : Language -> NominalDate -> Bool -> String -> List (Html (Msg msg))
viewPhoneUpdateConfirmation language currentDate allowComponentsSelection phoneNumber =
    let
        nextState =
            if allowComponentsSelection then
                ComponentsSelection phoneNumber Nothing

            else
                ConfirmationBeforeSending phoneNumber
    in
    [ div [ class "content" ]
        [ translateText language Translate.SendViaWhatsAppPhoneUpdateConfirmationMessasge ]
    , div [ class "actions" ]
        [ button
            [ class "ui primary fluid button"
            , onClick <| SetState <| Just nextState
            ]
            [ text <| translate language Translate.Continue ]
        ]
    ]


viewComponentsSelection : Language -> NominalDate -> String -> Maybe ReportComponentsList -> ReportComponentsConfig msg -> List (Html (Msg msg))
viewComponentsSelection language currentDate phoneNumber componentsList config =
    let
        componentsSelectionInput =
            case config.reportType of
                ReportWellChild ->
                    let
                        currentValue =
                            Maybe.map
                                (\list ->
                                    case list of
                                        WellChild components ->
                                            EverySet.toList components

                                        -- We should never get here.
                                        Antenatal _ ->
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

                ReportAntenatal ->
                    let
                        currentValue =
                            Maybe.map
                                (\list ->
                                    case list of
                                        Antenatal components ->
                                            EverySet.toList components

                                        -- We should never get here.
                                        WellChild _ ->
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
                        [ ComponentAntenatalRiskFactors
                        , ComponentAntenatalMedicalDiagnoses
                        , ComponentAntenatalObstetricalDiagnoses
                        , ComponentAntenatalPatientProgress
                        , ComponentAntenatalLabsResults
                        , ComponentAntenatalProgressPhotos
                        ]
                        []
                        currentValue
                        Nothing
                        setMsg
                        Translate.ReportComponentAntenatal

        continueButtonAction =
            if componentsSelected then
                [ onClick <| SetReportComponents (config.setReportComponentsFunc componentsList) phoneNumber ]

            else
                []

        componentsSelected =
            Maybe.map
                (\list ->
                    case config.reportType of
                        ReportWellChild ->
                            case list of
                                WellChild components ->
                                    not <| EverySet.isEmpty components

                                -- We should never get here.
                                Antenatal _ ->
                                    False

                        ReportAntenatal ->
                            case list of
                                Antenatal components ->
                                    not <| EverySet.isEmpty components

                                -- We should never get here.
                                WellChild _ ->
                                    False
                )
                componentsList
                |> Maybe.withDefault False
    in
    [ div [ class "content" ]
        [ p [] [ translateText language <| Translate.SendViaWhatsAppComponentsSelectionHeader config.reportType ]
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
                ([ classList [ ( "ui primary fluid button", True ), ( "disabled", not componentsSelected ) ] ]
                    ++ continueButtonAction
                )
                [ text <| translate language Translate.Continue ]
            ]
        ]
    ]


viewConfirmationBeforeSending : Language -> NominalDate -> String -> List (Html (Msg msg))
viewConfirmationBeforeSending language currentDate phoneNumber =
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppConfirmationBeforeSendingHeader ]
        , p [] [ text phoneNumber ]
        , p [] [ text <| translate language Translate.SendViaWhatsAppConfirmationBeforeSendingQuestion ]
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
                , onClick <| Execute phoneNumber
                ]
                [ text <| translate language Translate.Send ]
            ]
        ]
    ]
