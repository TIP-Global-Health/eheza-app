module Pages.NCD.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model
    exposing
        ( MedicalCondition(..)
        , NCDFamilyHistorySign(..)
        , NCDHealthEducationSign(..)
        , NCDMeasurements
        , NCDSocialHistorySign(..)
        , NonReferralSign(..)
        , ReasonForNonReferral(..)
        , RecommendedTreatmentSign(..)
        , ReferToFacilitySign(..)
        , ReferralFacility(..)
        , TestExecutionNote(..)
        , TestVariant(..)
        )
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Utils exposing (getAllActivities)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..), NCDProgressReportInitiator(..))
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.Person.Model exposing (Person)
import Components.SendViaWhatsAppDialog.Model
import Components.SendViaWhatsAppDialog.Utils
import Components.SendViaWhatsAppDialog.View
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.AcuteIllness.Participant.Utils exposing (isAcuteIllnessActive)
import Pages.NCD.Activity.Utils exposing (expectLaboratoryTask)
import Pages.NCD.Model exposing (AssembledData)
import Pages.NCD.ProgressReport.Model exposing (..)
import Pages.NCD.ProgressReport.Svg exposing (viewBloodGlucoseByTime, viewBloodPressureByTime, viewHbA1cByTime, viewMarkers)
import Pages.NCD.Utils
    exposing
        ( allRecommendedTreatmentSignsForHypertension
        , diabetesDiagnoses
        , generateAssembledData
        , patientIsPregnant
        , recommendedTreatmentSignsForDiabetes
        , updateChronicDiagnoses
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.Model exposing (..)
import Pages.Report.Utils exposing (..)
import Pages.Report.View exposing (..)
import Pages.Utils
    exposing
        ( viewEncounterActionButton
        , viewEndEncounterButton
        , viewEndEncounterDialog
        , viewEndEncounterMenuForProgressReport
        , viewPersonDetailsExtended
        )
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id initiator db model =
    let
        assembled =
            generateAssembledData id db

        header =
            viewHeader language initiator model

        content =
            viewWebData language (viewContent language currentDate initiator db model) identity assembled

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-report ncd" ] <|
        [ header
        , content
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> NCDProgressReportInitiator -> Model -> Html Msg
viewHeader language initiator model =
    let
        label =
            Maybe.map
                (\mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            Translate.LabResults

                        LabResultsHistory _ ->
                            Translate.LabHistory
                )
                model.labResultsMode
                |> Maybe.withDefault Translate.NCDProgressReport

        backIcon =
            let
                iconForView action =
                    span
                        [ class "link-back" ]
                        [ span
                            [ class "icon-back"
                            , onClick action
                            ]
                            []
                        ]

                goBackAction defaultAction =
                    Maybe.map goBackActionByLabResultsState model.labResultsMode
                        |> Maybe.withDefault (goBackActionByDiagnosisMode defaultAction)

                goBackActionByLabResultsState mode =
                    let
                        backToCurrentMsg targetMode =
                            SetLabResultsMode (Just (LabResultsCurrent targetMode))
                    in
                    case mode of
                        LabResultsCurrent currentMode ->
                            case currentMode of
                                LabResultsCurrentMain ->
                                    SetLabResultsMode Nothing

                                LabResultsCurrentDipstickShort ->
                                    backToCurrentMsg LabResultsCurrentMain

                                LabResultsCurrentDipstickLong ->
                                    backToCurrentMsg LabResultsCurrentMain

                                LabResultsCurrentLipidPanel ->
                                    backToCurrentMsg LabResultsCurrentMain

                        LabResultsHistory historyMode ->
                            SetLabResultsMode model.labResultsHistoryOrigin

                goBackActionByDiagnosisMode defaultAction =
                    case model.diagnosisMode of
                        ModeActiveDiagnosis ->
                            defaultAction

                        ModeCompletedDiagnosis ->
                            SetDiagnosisMode ModeActiveDiagnosis
            in
            case initiator of
                Backend.NCDEncounter.Types.InitiatorEncounterPage id ->
                    iconForView <| goBackAction (SetActivePage <| UserPage <| NCDEncounterPage id)

                Backend.NCDEncounter.Types.InitiatorRecurrentEncounterPage id ->
                    iconForView <| goBackAction (SetActivePage <| UserPage <| NCDRecurrentEncounterPage id)

        goBackPage =
            case initiator of
                Backend.NCDEncounter.Types.InitiatorEncounterPage id ->
                    NCDEncounterPage id

                Backend.NCDEncounter.Types.InitiatorRecurrentEncounterPage id ->
                    NCDRecurrentEncounterPage id
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label ]
        , backIcon
        ]


viewContent : Language -> NominalDate -> NCDProgressReportInitiator -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate initiator db model assembled =
    let
        derivedContent =
            let
                labResultsConfig =
                    { hivPCR = False
                    , partnerHIV = False
                    , syphilis = False
                    , hepatitisB = False
                    , malaria = False
                    , hemoglobin = False
                    , bloodGpRs = False
                    , creatinine = True
                    , liverFunction = True
                    , pregnancy = expectLaboratoryTask currentDate assembled TaskPregnancyTest
                    , hba1c = True
                    , lipidPanel = True
                    }
            in
            case model.labResultsMode of
                Just mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            [ generateLabsResultsPaneData currentDate assembled
                                |> viewLabResultsPane language currentDate currentMode SetLabResultsMode labResultsConfig
                            ]

                        LabResultsHistory historyMode ->
                            [ viewLabResultsHistoryPane language currentDate historyMode ]

                Nothing ->
                    let
                        acuteIllnesses =
                            Dict.get assembled.participant.person db.individualParticipantsByPerson
                                |> Maybe.andThen RemoteData.toMaybe
                                |> Maybe.map Dict.toList
                                |> Maybe.withDefault []
                                |> List.filter
                                    (\( _, participant ) ->
                                        participant.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                                    )
                    in
                    case model.diagnosisMode of
                        ModeActiveDiagnosis ->
                            let
                                -- Drawing SVG charts causes major slowness, specially when
                                -- typing new phone number. Therefore, we do not show it when
                                -- 'Send via WhatsApp' dialog is open, until its final
                                -- confirmation steps.
                                showPatientProgressPaneByWhatsAppDialog =
                                    Maybe.map
                                        (\state ->
                                            case state of
                                                Components.SendViaWhatsAppDialog.Model.ConfirmationBeforeExecuting _ ->
                                                    True

                                                Components.SendViaWhatsAppDialog.Model.ExecutionResult _ ->
                                                    True

                                                _ ->
                                                    False
                                        )
                                        model.sendViaWhatsAppDialog.state
                                        |> Maybe.withDefault True

                                patientProgressPane =
                                    if showPatientProgressPaneByWhatsAppDialog then
                                        viewPatientProgressPane language currentDate assembled
                                            |> showIf (showComponent Components.SendViaWhatsAppDialog.Model.ComponentNCDPatientProgress)

                                    else
                                        emptyNode

                                labsPane =
                                    Maybe.map
                                        (\components ->
                                            generateLabsResultsPaneData currentDate assembled
                                                |> viewLabResultsPane language currentDate LabResultsCurrentMain SetLabResultsMode labResultsConfig
                                                |> showIf (showComponent Components.SendViaWhatsAppDialog.Model.ComponentNCDLabsResults)
                                        )
                                        model.components
                                        |> Maybe.withDefault (viewLabsPane language currentDate SetLabResultsMode)

                                actions =
                                    case initiator of
                                        Backend.NCDEncounter.Types.InitiatorEncounterPage _ ->
                                            let
                                                ( completedActivities, pendingActivities ) =
                                                    List.filter (Pages.NCD.Activity.Utils.expectActivity currentDate assembled) getAllActivities
                                                        |> List.partition (Pages.NCD.Activity.Utils.activityCompleted currentDate assembled)

                                                allowEndEncounter =
                                                    List.isEmpty pendingActivities
                                            in
                                            viewEndEncounterMenuForProgressReport language
                                                allowEndEncounter
                                                SetEndEncounterDialogState
                                                (MsgSendViaWhatsAppDialog <|
                                                    Components.SendViaWhatsAppDialog.Model.SetState <|
                                                        Just Components.SendViaWhatsAppDialog.Model.Consent
                                                )

                                        Backend.NCDEncounter.Types.InitiatorRecurrentEncounterPage _ ->
                                            viewEndEncounterMenuForProgressReport language
                                                True
                                                (always (SetActivePage <| UserPage GlobalCaseManagementPage))
                                                (MsgSendViaWhatsAppDialog <|
                                                    Components.SendViaWhatsAppDialog.Model.SetState <|
                                                        Just Components.SendViaWhatsAppDialog.Model.Consent
                                                )

                                showComponent =
                                    Components.SendViaWhatsAppDialog.Utils.showComponent model.components
                            in
                            [ viewRiskFactorsPane language currentDate assembled
                                |> showIf (showComponent Components.SendViaWhatsAppDialog.Model.ComponentNCDRiskFactors)
                            , viewAcuteIllnessPane language currentDate initiator acuteIllnesses model.diagnosisMode db
                                |> showIf (showComponent Components.SendViaWhatsAppDialog.Model.ComponentNCDActiveDiagnosis)
                            , viewMedicalDiagnosisPane language currentDate assembled
                                |> showIf (showComponent Components.SendViaWhatsAppDialog.Model.ComponentNCDMedicalDiagnosis)
                            , patientProgressPane
                            , labsPane
                            , -- Actions are hidden when viewing for sharing via WhatsApp.
                              showIf (isNothing model.components) actions
                            ]

                        ModeCompletedDiagnosis ->
                            [ viewAcuteIllnessPane language currentDate initiator acuteIllnesses model.diagnosisMode db ]

        componentsConfig =
            Just { setReportComponentsMsg = SetReportComponents }
    in
    div
        [ class "ui unstackable items"
        , Html.Attributes.id "report-content"
        ]
    <|
        viewPersonInfoPane language currentDate assembled.person
            :: (derivedContent
                    ++ [ Html.map MsgSendViaWhatsAppDialog
                            (Components.SendViaWhatsAppDialog.View.view
                                language
                                currentDate
                                ( assembled.participant.person, assembled.person )
                                Components.SendViaWhatsAppDialog.Model.ReportNCD
                                componentsConfig
                                model.sendViaWhatsAppDialog
                            )
                       ]
               )


viewPersonInfoPane : Language -> NominalDate -> Person -> Html any
viewPersonInfoPane language currentDate person =
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div [ class "patient-info" ] <|
            viewPersonDetailsExtended language currentDate person
        ]


viewPaneHeading : Language -> TranslationId -> Html any
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]


viewRiskFactorsPane : Language -> NominalDate -> AssembledData -> Html Msg
viewRiskFactorsPane language currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        content =
            List.map (Translate.NCDRiskFactor >> translate language >> text >> List.singleton >> li [])
                riskFactors
                |> ul []
                |> List.singleton

        riskFactors =
            List.map
                (\measurements ->
                    let
                        familyRisks =
                            getMeasurementValueFunc measurements.familyHistory
                                |> Maybe.map (.signs >> generateFamilyHistoryRiskFactors)
                                |> Maybe.withDefault []

                        socialRisks =
                            getMeasurementValueFunc measurements.socialHistory
                                |> Maybe.map (.signs >> generateSocialHistoryRiskFactors)
                                |> Maybe.withDefault []
                    in
                    familyRisks ++ socialRisks
                )
                allMeasurements
                |> List.concat
                |> Pages.Utils.unique
    in
    div [ class "risk-factors" ]
        [ div [ class <| "pane-heading red" ]
            [ img [ src "assets/images/exclamation-white-outline.png" ] []
            , span [] [ text <| translate language Translate.RiskFactors ]
            ]
        , div [ class "pane-content" ] content
        ]


generateFamilyHistoryRiskFactors : EverySet NCDFamilyHistorySign -> List NCDRiskFactor
generateFamilyHistoryRiskFactors signs =
    List.filter
        (\riskFactor ->
            case riskFactor of
                RiskFactorHypertensionHistory ->
                    EverySet.member SignHypertensionHistory signs

                RiskFactorHearProblemHistory ->
                    EverySet.member SignHeartProblemHistory signs

                RiskFactorDiabetesHistory ->
                    EverySet.member SignDiabetesHistory signs

                _ ->
                    False
        )
        familyHistoryRiskFactors


generateSocialHistoryRiskFactors : EverySet NCDSocialHistorySign -> List NCDRiskFactor
generateSocialHistoryRiskFactors signs =
    List.filter
        (\riskFactor ->
            case riskFactor of
                RiskFactorSmokeCigarettes ->
                    EverySet.member SignSmokeCigarettes signs

                RiskFactorConsumeSalt ->
                    EverySet.member SignConsumeSalt signs

                _ ->
                    False
        )
        socialHistoryRiskFactors


familyHistoryRiskFactors : List NCDRiskFactor
familyHistoryRiskFactors =
    [ RiskFactorHypertensionHistory
    , RiskFactorHearProblemHistory
    , RiskFactorDiabetesHistory
    ]


socialHistoryRiskFactors : List NCDRiskFactor
socialHistoryRiskFactors =
    [ RiskFactorSmokeCigarettes
    , RiskFactorConsumeSalt
    ]


viewMedicalDiagnosisPane : Language -> NominalDate -> AssembledData -> Html Msg
viewMedicalDiagnosisPane language currentDate assembled =
    let
        allEncountersData =
            { id = assembled.id
            , startDate = assembled.encounter.startDate
            , diagnoses = assembled.encounter.diagnoses
            , measurements = assembled.measurements
            }
                :: assembled.previousEncountersData

        content =
            List.map (Translate.MedicalCondition >> translate language >> text >> List.singleton >> li []) coMorbidities
                ++ dignoses
                |> ul []
                |> List.singleton

        coMorbidities =
            List.map .measurements allEncountersData
                |> List.map
                    (.coMorbidities
                        >> getMeasurementValueFunc
                        >> Maybe.map
                            (EverySet.toList
                                >> List.filter
                                    (\mdecicalCondition ->
                                        List.member mdecicalCondition
                                            [ MedicalConditionHIV
                                            , MedicalConditionDiabetes
                                            , MedicalConditionKidneyDisease
                                            , MedicalConditionPregnancy
                                            , MedicalConditionHypertension
                                            , MedicalConditionGestationalDiabetes
                                            , MedicalConditionPregnancyRelatedHypertension
                                            ]
                                    )
                            )
                        >> Maybe.withDefault []
                    )
                |> List.concat
                |> Pages.Utils.unique

        dignoses =
            List.map
                (\data ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicDiagnoses data.startDate data.diagnoses assembled

                        withRenalComplications =
                            List.member DiagnosisRenalComplications diagnosesIncludingChronic

                        withDiabetes =
                            List.any (\diagnosis -> List.member diagnosis diagnosesIncludingChronic) diabetesDiagnoses
                    in
                    List.map (viewTreatmentForDiagnosis language data.startDate data.measurements withRenalComplications withDiabetes) diagnosesIncludingChronic
                )
                allEncountersData
                |> List.concat
    in
    div [ class "medical-diagnosis" ]
        [ viewItemHeading language Translate.MedicalDiagnosis "blue"
        , div [ class "pane-content" ] content
        ]


viewTreatmentForDiagnosis :
    Language
    -> NominalDate
    -> NCDMeasurements
    -> Bool
    -> Bool
    -> NCDDiagnosis
    -> Html any
viewTreatmentForDiagnosis language date measurements withRenalComplications withDiabetes diagnosis =
    let
        diagnosisForProgressReport =
            translate language <| Translate.NCDDiagnosisForProgressReport withRenalComplications isPregnant diagnosis

        isPregnant =
            patientIsPregnant measurements

        hypertensionMessage =
            let
                treatmentPhrase =
                    getMeasurementValueFunc measurements.medicationDistribution
                        |> Maybe.map
                            (\value ->
                                let
                                    recordedTreatmentSignsForHypertension =
                                        EverySet.toList value.recommendedTreatmentSigns
                                            |> List.filter (\sign -> List.member sign allRecommendedTreatmentSignsForHypertension)
                                in
                                case recordedTreatmentSignsForHypertension of
                                    [ NoTreatmentForHypertension ] ->
                                        String.toLower <| translate language Translate.NoTreatmentAdministered

                                    _ ->
                                        let
                                            treatment =
                                                List.map (Translate.RecommendedTreatmentSignLabel >> translate language) recordedTreatmentSignsForHypertension
                                                    |> List.intersperse (translate language Translate.And)
                                                    |> String.join ""
                                        in
                                        (String.toLower <| translate language Translate.TreatedWith)
                                            ++ " "
                                            ++ treatment
                            )
                        |> Maybe.withDefault
                            (getMeasurementValueFunc measurements.healthEducation
                                |> Maybe.map
                                    (\signs ->
                                        case EverySet.toList signs of
                                            [ NoNCDHealthEducationSigns ] ->
                                                String.toLower <| translate language Translate.HealthEducationNotProvided

                                            _ ->
                                                String.toLower <| translate language Translate.HealthEducationProvided
                                    )
                                |> Maybe.withDefault (String.toLower <| translate language Translate.NoTreatmentRecorded)
                            )

                referralPhrase =
                    if
                        -- These are conditions when referral is needed, when
                        -- combined with Hypertension diagnosis.
                        isPregnant || withRenalComplications || withDiabetes || diagnosis == DiagnosisHypertensionStage3
                    then
                        let
                            phrase =
                                getMeasurementValueFunc measurements.referral
                                    |> Maybe.map
                                        (\value ->
                                            let
                                                ( facility, referralSign, nonReferralSign ) =
                                                    if isPregnant then
                                                        ( FacilityANCServices, ReferToANCServices, NonReferralReasonANCServices )

                                                    else
                                                        ( FacilityHospital, ReferToHospital, NonReferralReasonHospital )
                                            in
                                            if EverySet.member referralSign value.referralSigns then
                                                String.toLower <| translate language <| Translate.ReferredToFacility facility

                                            else
                                                let
                                                    nonReferralReason =
                                                        getCurrentReasonForNonReferral nonReferralSign value.nonReferralReasons
                                                            |> Maybe.map
                                                                (\reason ->
                                                                    if reason == NoReasonForNonReferral then
                                                                        ""

                                                                    else
                                                                        " - " ++ (String.toLower <| translate language <| Translate.ReasonForNonReferral reason)
                                                                )
                                                            |> Maybe.withDefault ""
                                                in
                                                (String.toLower <| translate language <| Translate.ReferredToFacilityNot facility) ++ nonReferralReason
                                        )
                                    |> Maybe.withDefault (translate language Translate.NoReferralRecorded)
                        in
                        (String.toLower <| translate language Translate.And)
                            ++ " "
                            ++ phrase
                            ++ " "

                    else
                        ""
            in
            diagnosisForProgressReport
                ++ " - "
                ++ treatmentPhrase
                ++ " "
                ++ referralPhrase
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI

        diabetesMessage =
            let
                treatmentPhrase =
                    getMeasurementValueFunc measurements.medicationDistribution
                        |> Maybe.andThen
                            (.recommendedTreatmentSigns
                                >> EverySet.toList
                                >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForDiabetes)
                                >> List.head
                            )
                        |> Maybe.map
                            (\treatmentSign ->
                                if treatmentSign == NoTreatmentForDiabetes then
                                    String.toLower <| translate language Translate.NoTreatmentAdministered

                                else
                                    (String.toLower <| translate language Translate.TreatedWith)
                                        ++ " "
                                        ++ (translate language <| Translate.RecommendedTreatmentSignLabel treatmentSign)
                            )
                        |> Maybe.withDefault (String.toLower <| translate language Translate.NoTreatmentRecorded)
            in
            diagnosisForProgressReport
                ++ " - "
                ++ treatmentPhrase
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI

        wrapWithLI =
            text >> List.singleton >> li []
    in
    case diagnosis of
        DiagnosisHypertensionStage1 ->
            hypertensionMessage

        DiagnosisHypertensionStage2 ->
            hypertensionMessage

        DiagnosisHypertensionStage3 ->
            hypertensionMessage

        DiagnosisDiabetesInitial ->
            diabetesMessage

        DiagnosisDiabetesRecurrent ->
            diabetesMessage

        DiagnosisRenalComplications ->
            emptyNode

        NoNCDDiagnosis ->
            emptyNode


viewPatientProgressPane : Language -> NominalDate -> AssembledData -> Html Msg
viewPatientProgressPane language currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        sysMeasurements =
            List.map Tuple.first bloodPressure

        diaMeasurements =
            List.map Tuple.second bloodPressure

        bloodPressure =
            List.map
                (.vitals
                    >> getMeasurementValueFunc
                    >> Maybe.andThen
                        (\value ->
                            Maybe.map2 (\sys dia -> ( sys, dia ))
                                value.sys
                                value.dia
                        )
                )
                allMeasurements
                |> Maybe.Extra.values
                |> List.take 12
                |> List.reverse

        sugarCountMeasurements =
            List.map
                (.randomBloodSugarTest
                    >> getMeasurementValueFunc
                    >> Maybe.andThen randomBloodSugarResultFromValue
                    >> Maybe.andThen Tuple.second
                )
                allMeasurements
                |> Maybe.Extra.values
                |> List.take 12
                |> List.reverse

        hba1cMeasurements =
            List.map
                (.hba1cTest >> getMeasurementValueFunc)
                allMeasurements
                |> Maybe.Extra.values
                |> List.filterMap
                    (\value ->
                        Maybe.map2 Tuple.pair
                            value.executionDate
                            value.hba1cResult
                    )
                -- We do this to have a unique value for each date.
                |> Dict.fromList
                |> Dict.values
                |> List.take 12
                |> List.reverse
    in
    div [ class "patient-progress" ]
        [ viewItemHeading language Translate.PatientProgress "blue"
        , div [ class "pane-content" ]
            [ viewMarkers
            , div [ class "chart-section" ]
                [ div [ class "heading" ] [ text <| translate language Translate.BloodPressure ]
                , viewBloodPressureByTime language sysMeasurements diaMeasurements
                ]
            , div [ class "chart-section" ]
                [ div [ class "heading" ] [ text <| translate language Translate.BloodGlucose ]
                , viewBloodGlucoseByTime language sugarCountMeasurements
                ]
            , div [ class "chart-section" ]
                [ div [ class "heading" ] [ text <| translate language Translate.HbA1c ]
                , viewHbA1cByTime language hba1cMeasurements
                ]
            ]
        ]


viewAcuteIllnessPane :
    Language
    -> NominalDate
    -> NCDProgressReportInitiator
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> DiagnosisMode
    -> ModelIndexedDb
    -> Html Msg
viewAcuteIllnessPane language currentDate initiator acuteIllnesses diagnosisMode db =
    let
        ( activeIllnesses, completedIllnesses ) =
            List.partition (Tuple.second >> isAcuteIllnessActive currentDate) acuteIllnesses

        entriesHeading =
            div [ class "heading diagnosis" ]
                [ div [ class "assesment" ] [ text <| translate language Translate.Assessment ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                , div [ class "date" ] [ text <| translate language Translate.DiagnosisDate ]
                , div [ class "see-more" ] [ text <| translate language Translate.SeeMore ]
                ]

        ( label, priorDiagniosisButton ) =
            case diagnosisMode of
                ModeActiveDiagnosis ->
                    ( Translate.ActiveDiagnosis
                    , div [ class "pane-action" ]
                        [ button
                            [ class "ui primary button"
                            , onClick <| SetDiagnosisMode ModeCompletedDiagnosis
                            ]
                            [ text <| translate language Translate.ReviewPriorDiagnosis ]
                        ]
                    )

                ModeCompletedDiagnosis ->
                    ( Translate.PriorDiagnosis
                    , emptyNode
                    )

        daignosisEntries =
            List.map
                (\( data, _ ) ->
                    let
                        acuteIllnessProgressReportInitiator =
                            InitiatorNCDProgressReport initiator
                    in
                    viewAcuteIllnessDiagnosisEntry language acuteIllnessProgressReportInitiator db SetActivePage data
                )
                selectedDiagnosisEntries
                |> Maybe.Extra.values

        selectedDiagnosisEntries =
            case diagnosisMode of
                ModeActiveDiagnosis ->
                    List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activeIllnesses

                ModeCompletedDiagnosis ->
                    List.map (\( participantId, data ) -> ( ( participantId, StatusResolved ), data )) completedIllnesses

        entries =
            List.sortWith sortTuplesByDateDesc daignosisEntries
                |> List.map Tuple.second
    in
    div [ class "pane diagnosis" ]
        [ viewPaneHeading language label
        , div [ class "pane-content" ] <|
            entriesHeading
                :: viewEntries language entries
        , priorDiagniosisButton
        ]


generateLabsResultsPaneData :
    NominalDate
    -> AssembledData
    -> LabsResultsValues NCDEncounterId
generateLabsResultsPaneData currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        extractValues getMeasurementFunc =
            List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
                allMeasurements
    in
    { hiv = extractValues .hivTest
    , urineDipstick = extractValues .urineDipstickTest
    , randomBloodSugar = extractValues .randomBloodSugarTest
    , hivPCR = []
    , partnerHIV = []
    , syphilis = []
    , hepatitisB = []
    , malaria = []
    , hemoglobin = []
    , bloodGpRs = []
    , creatinine = extractValues .creatinineTest
    , liverFunction = extractValues .liverFunctionTest
    , pregnancy = extractValues .pregnancyTest
    , hba1c = extractValues .hba1cTest
    , lipidPanel = extractValues .lipidPanelTest
    }
