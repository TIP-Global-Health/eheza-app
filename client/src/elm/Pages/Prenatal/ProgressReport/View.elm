module Pages.Prenatal.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , DangerSign(..)
        , EyesCPESign(..)
        , FetalPresentation(..)
        , HandsCPESign(..)
        , IllnessSymptom(..)
        , LabsResultsReviewState(..)
        , LastMenstrualPeriodValue
        , MedicalHistoryInfectiousDisease(..)
        , MedicalHistoryMentalHealthIssue(..)
        , MedicalHistoryPhysicalCondition(..)
        , MedicalHistorySign(..)
        , MedicationDistributionSign(..)
        , NonReferralSign(..)
        , ObstetricHistoryStep2Sign(..)
        , OutsideCareMedication(..)
        , PrenatalHIVSign(..)
        , PrenatalHealthEducationSign(..)
        , PrenatalMeasurements
        , PrenatalSymptomQuestion(..)
        , ReasonForNonReferral(..)
        , RecommendedTreatmentSign(..)
        , ReferToFacilitySign(..)
        , ReferralFacility(..)
        , Rhesus(..)
        , SendToHCSign(..)
        , SpecialityCareSign(..)
        , TestExecutionNote(..)
        , TestResult(..)
        )
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, getHeightValue, getMeasurementValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isLabTechnician)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , PrenatalRecurrentActivity(..)
        , allMedicalDiagnoses
        , allObstetricalDiagnoses
        , allTrimesters
        )
import Backend.PrenatalActivity.Utils exposing (getEncounterTrimesterData)
import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Backend.Utils exposing (reportToWhatsAppEnabled)
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Utils
import Components.ReportToWhatsAppDialog.View
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (VaccinationStatus(..))
import Measurement.Utils
    exposing
        ( outsideCareMedicationOptionsAnemia
        , outsideCareMedicationOptionsHIV
        , outsideCareMedicationOptionsHypertension
        , outsideCareMedicationOptionsMalaria
        , outsideCareMedicationOptionsSyphilis
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Utils
    exposing
        ( generateFutureVaccinationsDataByProgress
        , resolveMeasuredHeight
        , resolvePrePregnancyClassification
        , resolvePrePregnancyWeight
        , respiratoryRateElevated
        , weightGainStandardsPerPrePregnancyClassification
        )
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Encounter.View exposing (viewActionButton)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData, VaccinationProgressDict)
import Pages.Prenatal.ProgressReport.Model exposing (..)
import Pages.Prenatal.ProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers, viewWeightGainForEGA)
import Pages.Prenatal.ProgressReport.Utils exposing (..)
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.RecurrentEncounter.Utils
import Pages.Prenatal.Utils
    exposing
        ( outsideCareDiagnoses
        , outsideCareDiagnosesWithPossibleMedication
        , recommendedTreatmentSignsForHypertension
        , recommendedTreatmentSignsForMalaria
        , recommendedTreatmentSignsForMastitis
        , recommendedTreatmentSignsForSyphilis
        , resolveARVReferralDiagnosis
        , resolveNCDReferralDiagnoses
        )
import Pages.Report.Model exposing (..)
import Pages.Report.View exposing (..)
import Pages.Utils
    exposing
        ( viewConfirmationDialog
        , viewCustomAction
        , viewEndEncounterMenuForProgressReport
        , viewPhotoThumbFromImageUrl
        )
import Round
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.NominalDate exposing (sortByDateDesc)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Nurse
    -> PrenatalEncounterId
    -> Bool
    -> PrenatalProgressReportInitiator
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features nurse id isChw initiator db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewContentAndHeader language currentDate zscores site features nurse isChw initiator model) identity assembled


viewContentAndHeader :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Nurse
    -> Bool
    -> PrenatalProgressReportInitiator
    -> Model
    -> AssembledData
    -> Html Msg
viewContentAndHeader language currentDate zscores site features nurse isChw initiator model assembled =
    let
        isLabTech =
            isLabTechnician nurse

        isResultsReviewer =
            -- A nurse.
            (not isChw && not isLabTech)
                && -- Access was perfomed from case  managament.
                   (case initiator of
                        InitiatorCaseManagement _ ->
                            True

                        _ ->
                            False
                   )
                && -- Labs results review was requested.
                   (getMeasurementValueFunc assembled.measurements.labsResults
                        |> Maybe.map (.reviewState >> (==) (Just LabsResultsReviewRequested))
                        |> Maybe.withDefault False
                   )

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewConfirmationDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter assembled.id)
                        (SetEndEncounterDialogState False)

            else
                Nothing

        componentsConfig =
            Just { setReportComponentsMsg = SetReportComponents }
    in
    div [ class "page-report clinical" ] <|
        [ viewHeader language assembled.id isLabTech isResultsReviewer initiator model
        , viewContent language currentDate zscores site features isChw isLabTech isResultsReviewer initiator model assembled
        , viewModal endEncounterDialog
        , Html.map MsgReportToWhatsAppDialog
            (Components.ReportToWhatsAppDialog.View.view
                language
                currentDate
                site
                ( assembled.participant.person, assembled.person )
                Components.ReportToWhatsAppDialog.Model.ReportAntenatal
                componentsConfig
                model.reportToWhatsAppDialog
            )
        ]


viewHeader : Language -> PrenatalEncounterId -> Bool -> Bool -> PrenatalProgressReportInitiator -> Model -> Html Msg
viewHeader language id isLabTech isResultsReviewer initiator model =
    let
        label =
            if isLabTech || isResultsReviewer then
                Translate.LabResults

            else
                Maybe.map
                    (\mode ->
                        case mode of
                            LabResultsCurrent _ ->
                                Translate.LabResults

                            LabResultsHistory _ ->
                                Translate.LabHistory
                    )
                    model.labResultsMode
                    |> Maybe.withDefault Translate.AntenatalProgressReport

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

                goBackActionByLabResultsState defaultAction =
                    Maybe.map
                        (\mode ->
                            let
                                backToCurrentMsg targetMode =
                                    SetLabResultsMode (Just (LabResultsCurrent targetMode))
                            in
                            case mode of
                                LabResultsCurrent currentMode ->
                                    case currentMode of
                                        LabResultsCurrentMain ->
                                            if isLabTech then
                                                defaultAction

                                            else
                                                SetLabResultsMode Nothing

                                        LabResultsCurrentDipstickShort ->
                                            backToCurrentMsg LabResultsCurrentMain

                                        LabResultsCurrentDipstickLong ->
                                            backToCurrentMsg LabResultsCurrentMain

                                        LabResultsCurrentLipidPanel ->
                                            backToCurrentMsg LabResultsCurrentMain

                                LabResultsHistory _ ->
                                    SetLabResultsMode model.labResultsHistoryOrigin
                        )
                        model.labResultsMode
                        |> Maybe.withDefault defaultAction
            in
            case initiator of
                InitiatorEncounterPage prenatalEncounterId ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId)

                InitiatorRecurrentEncounterPage prenatalEncounterId ->
                    let
                        action =
                            if isLabTech then
                                SetActivePage <|
                                    UserPage <|
                                        PrenatalRecurrentActivityPage prenatalEncounterId LabResults

                            else
                                SetActivePage <|
                                    UserPage <|
                                        PrenatalRecurrentEncounterPage prenatalEncounterId
                    in
                    iconForView <| goBackActionByLabResultsState action

                InitiatorNewEncounter _ ->
                    emptyNode

                Backend.PrenatalEncounter.Model.InitiatorPatientRecord patientId ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PatientRecordPage InitiatorParticipantDirectory patientId)

                InitiatorCaseManagement _ ->
                    iconForView <|
                        SetActivePage <|
                            UserPage
                                GlobalCaseManagementPage
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label ]
        , backIcon
        ]


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> Bool
    -> Bool
    -> PrenatalProgressReportInitiator
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate zscores site features isChw isLabTech isResultsReviewer initiator model assembled =
    let
        globalLmpValue =
            let
                nursePreviousMeasurements =
                    List.map .measurements assembled.nursePreviousEncountersData

                chwPreviousMeasurements =
                    List.map (\( _, _, measurements ) -> measurements)
                        assembled.chwPreviousMeasurementsWithDates
            in
            resolveGlobalLmpValue nursePreviousMeasurements chwPreviousMeasurements assembled.measurements

        content =
            let
                labResultsConfig =
                    { hivPCR = True
                    , partnerHIV = True
                    , syphilis = True
                    , hepatitisB = True
                    , malaria = True
                    , hemoglobin = True
                    , bloodGpRs = True
                    , creatinine = False
                    , liverFunction = False
                    , pregnancy = False
                    , hba1c = False
                    , lipidPanel = False
                    }

                viewForConfirmation =
                    -- Lab technician  and nurse results reviewer are
                    -- presented data for confirmation purposes.
                    -- Confirmed data is only of current encounter.
                    isLabTech || isResultsReviewer

                labResultsMode =
                    if viewForConfirmation then
                        -- Lab technician  and nurse results reviewer go
                        -- straight to main page of lab results.
                        Maybe.Extra.or model.labResultsMode (Just <| LabResultsCurrent LabResultsCurrentMain)

                    else
                        model.labResultsMode
            in
            case labResultsMode of
                Just mode ->
                    let
                        resultsPane =
                            case mode of
                                LabResultsCurrent currentMode ->
                                    -- Possibly view is for confirmation, therefore,
                                    -- passing 'viewForConfirmation.'
                                    generateLabsResultsPaneData currentDate viewForConfirmation assembled
                                        |> viewLabResultsPane language
                                            currentDate
                                            viewForConfirmation
                                            currentMode
                                            SetLabResultsMode
                                            labResultsConfig

                                LabResultsHistory historyMode ->
                                    viewLabResultsHistoryPane language currentDate historyMode

                        bottomActions =
                            case initiator of
                                InitiatorRecurrentEncounterPage id ->
                                    if isLabTech then
                                        div [ class "two ui buttons" ]
                                            [ button
                                                [ class "ui primary fluid button"
                                                , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
                                                ]
                                                [ text <| translate language Translate.SubmitResults ]
                                            , button
                                                [ class "ui primary fluid button"
                                                , onClick <|
                                                    SetActivePage <|
                                                        UserPage <|
                                                            PrenatalRecurrentActivityPage id LabResults
                                                ]
                                                [ text <| translate language Translate.EditResults ]
                                            ]

                                    else
                                        emptyNode

                                InitiatorCaseManagement encounterId ->
                                    if isResultsReviewer then
                                        Maybe.map2
                                            (\( resultsId, _ ) value ->
                                                button
                                                    [ class "ui primary fluid button"
                                                    , onClick <|
                                                        ReviewAndAcceptLabsResults
                                                            assembled.participant.person
                                                            encounterId
                                                            resultsId
                                                            value
                                                    ]
                                                    [ text <| translate language Translate.ReviewAndAccept ]
                                            )
                                            assembled.measurements.labsResults
                                            (getMeasurementValueFunc assembled.measurements.labsResults)
                                            |> Maybe.withDefault emptyNode

                                    else
                                        emptyNode

                                _ ->
                                    emptyNode
                    in
                    [ resultsPane
                    , bottomActions
                    ]

                Nothing ->
                    let
                        firstNurseEncounterMeasurements =
                            getFirstNurseEncounterMeasurements isChw assembled

                        labsPane =
                            Maybe.map
                                (\_ ->
                                    -- This is for sure not view for confirmation, therefore
                                    -- 'False' is passed to make sure we generated data for current
                                    -- and previous encounters.
                                    generateLabsResultsPaneData currentDate False assembled
                                        |> viewLabResultsPane language
                                            currentDate
                                            viewForConfirmation
                                            LabResultsCurrentMain
                                            SetLabResultsMode
                                            labResultsConfig
                                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalLabsResults)
                                )
                                model.components
                                |> Maybe.withDefault (viewLabsPane language currentDate SetLabResultsMode)

                        actions =
                            case initiator of
                                InitiatorEncounterPage id ->
                                    let
                                        ( completedActivities, pendingActivities ) =
                                            getAllActivities assembled
                                                |> List.filter (Pages.Prenatal.Activity.Utils.expectActivity currentDate site assembled)
                                                |> List.partition (Pages.Prenatal.Activity.Utils.activityCompleted currentDate site assembled)

                                        ( actionsClass, actionButtonColor, reportToWhatsAppButton ) =
                                            if reportToWhatsAppEnabled features then
                                                ( "actions two"
                                                , "velvet"
                                                , button
                                                    [ class "ui fluid primary button"
                                                    , onClick <|
                                                        MsgReportToWhatsAppDialog <|
                                                            Components.ReportToWhatsAppDialog.Model.SetState <|
                                                                Just Components.ReportToWhatsAppDialog.Model.Consent
                                                    ]
                                                    [ text <| translate language Translate.ReportToWhatsApp ]
                                                )

                                            else
                                                ( "actions"
                                                , "primary"
                                                , emptyNode
                                                )
                                    in
                                    div [ class actionsClass ]
                                        [ viewActionButton language
                                            actionButtonColor
                                            pendingActivities
                                            completedActivities
                                            -- When pausing, we close the encounter.
                                            -- Entering lab results is available from
                                            -- Case management page.
                                            (CloseEncounter id)
                                            SetEndEncounterDialogState
                                            assembled
                                        , reportToWhatsAppButton
                                        ]

                                InitiatorRecurrentEncounterPage _ ->
                                    let
                                        ( _, pendingActivities ) =
                                            Pages.Prenatal.RecurrentEncounter.Utils.getAllActivities isLabTech
                                                |> List.filter (Pages.Prenatal.RecurrentActivity.Utils.expectActivity currentDate isLabTech assembled)
                                                |> List.partition (Pages.Prenatal.RecurrentActivity.Utils.activityCompleted currentDate isLabTech assembled)

                                        allowEndEncounter =
                                            List.isEmpty pendingActivities
                                    in
                                    viewEndEncounterMenuForProgressReport language
                                        features
                                        allowEndEncounter
                                        (always <| SetActivePage PinCodePage)
                                        (MsgReportToWhatsAppDialog <|
                                            Components.ReportToWhatsAppDialog.Model.SetState <|
                                                Just Components.ReportToWhatsAppDialog.Model.Consent
                                        )

                                InitiatorNewEncounter encounterId ->
                                    viewCustomAction language
                                        (SetActivePage <| UserPage <| PrenatalEncounterPage encounterId)
                                        False
                                        Translate.Reviewed

                                Backend.PrenatalEncounter.Model.InitiatorPatientRecord _ ->
                                    emptyNode

                                -- Access from case management is granted only for labs results
                                -- reviewers, so, this branch is not in use.
                                InitiatorCaseManagement _ ->
                                    emptyNode

                        showComponent =
                            Components.ReportToWhatsAppDialog.Utils.showComponent model.components
                    in
                    [ viewObstetricHistoryPane language currentDate firstNurseEncounterMeasurements
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalObstetricHistory)
                    , viewMedicalHistoryPane language currentDate firstNurseEncounterMeasurements
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalMedicalHistory)
                    , viewMedicalDiagnosisPane language currentDate isChw firstNurseEncounterMeasurements assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalMedicalDiagnosis)
                    , viewObstetricalDiagnosisPane language currentDate isChw globalLmpValue firstNurseEncounterMeasurements assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalObstetricalDiagnosis)
                    , viewMedicationHistoryPane language currentDate isChw assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalMedicationHistory)
                    , viewVaccinationHistoryPane language currentDate assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalImmunizationHistory)
                    , viewChwActivityPane language currentDate isChw assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalCHWActivity)
                    , viewPatientProgressPane language currentDate zscores isChw globalLmpValue assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalPatientProgress)
                    , labsPane
                    , viewProgressPhotosPane language currentDate isChw assembled
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentAntenatalProgressPhotos)
                    , -- Actions are hidden when viewing for sharing via WhatsApp.
                      showIf (isNothing model.components) actions
                    ]
    in
    div
        [ class "ui unstackable items"
        , Html.Attributes.id "report-content"
        ]
    <|
        viewHeaderPane language currentDate globalLmpValue assembled
            :: content


viewHeaderPane : Language -> NominalDate -> Maybe LastMenstrualPeriodValue -> AssembledData -> Html Msg
viewHeaderPane language currentDate globalLmpValue assembled =
    let
        mother =
            assembled.person

        ( edd, ega ) =
            assembled.globalLmpDate
                |> generateEDDandEGA language currentDate ( "--/--/----", "----" )

        obstetricHistoryValue =
            assembled.globalObstetricHistory

        ( gravida, para ) =
            unwrap
                ( "----", "----" )
                (\value ->
                    ( generateGravida value
                    , generatePara value
                    )
                )
                obstetricHistoryValue

        viewUnsureOfLmp =
            Maybe.map
                (\value ->
                    if value.confident == False then
                        p [ class "lmp-warning" ] [ text <| translate language Translate.UnsureOfLmp ]

                    else
                        emptyNode
                )
                globalLmpValue
                |> Maybe.withDefault emptyNode

        viewLineItem class_ label value =
            p [ class class_ ]
                [ span [ class "label" ] [ text <| translate language label ++ ":" ]
                , span [ class "value" ] [ text value ]
                ]
    in
    div [ class "header-pane" ]
        [ viewItemHeading language Translate.PatientInformation "blue"
        , div [ class "pane-content" ]
            [ div [ class "mother-details" ]
                [ div [ class "ui image" ]
                    [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
                , div [ class "content middle" ]
                    [ p [ class "mother-name" ] [ text mother.name ]
                    , showMaybe <|
                        Maybe.map
                            (\age ->
                                viewLineItem "age-wrapper" Translate.AgeWord (translate language <| Translate.YearsOld age)
                            )
                            (ageInYears currentDate mother)
                    ]
                , div [ class "content right" ]
                    [ viewLineItem "edd" Translate.Edd edd
                    , viewLineItem "ega" Translate.Ega ega
                    , viewUnsureOfLmp
                    ]
                ]
            , div [ class "gravida-para" ]
                [ div [ class "gravida" ]
                    [ div [ class "label" ] [ text <| translate language Translate.Gravida ]
                    , div [ class "value" ] [ text gravida ]
                    ]
                , div [ class "para" ]
                    [ div [ class "label" ] [ text <| translate language Translate.Para ]
                    , div [ class "para-breakdown" ]
                        [ div [ class "term" ]
                            [ div [] [ text <| String.slice 0 1 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.Term ]
                            ]
                        , div [ class "pre-term" ]
                            [ div [] [ text <| String.slice 1 2 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.PreTerm ]
                            ]
                        , div [ class "abortions" ]
                            [ div [] [ text <| String.slice 2 3 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.Abortions ]
                            ]
                        , div [ class "live-children" ]
                            [ div [] [ text <| String.slice 3 4 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.LiveChildren ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewObstetricHistoryPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewObstetricHistoryPane language currentDate measurements =
    let
        obsetricHistory =
            getMeasurementValueFunc measurements.obstetricHistory
                |> Maybe.map
                    (\value ->
                        let
                            abortions =
                                if value.abortions > 0 then
                                    Just <| translate language <| Translate.NumberOfAbortions value.abortions

                                else
                                    Nothing

                            pretermStillbirth =
                                if value.stillbirthsPreTerm > 0 then
                                    Just <| translate language <| Translate.NumberOfPretermStillbirths value.stillbirthsPreTerm

                                else
                                    Nothing

                            termStillbirth =
                                if value.stillbirthsAtTerm > 0 then
                                    Just <| translate language <| Translate.NumberOfTermStillbirths value.stillbirthsAtTerm

                                else
                                    Nothing

                            pretermDeliviries =
                                if value.preTermPregnancy > 0 then
                                    Just <| translate language <| Translate.NumberOfPretermDeliviries value.preTermPregnancy

                                else
                                    Nothing
                        in
                        Maybe.Extra.values [ abortions, pretermStillbirth, termStillbirth, pretermDeliviries ]
                    )
                |> Maybe.withDefault []

        obstetricHistoryStep2 =
            getMeasurementValueFunc measurements.obstetricHistoryStep2
                |> Maybe.map
                    (\value ->
                        let
                            cSectionInfo =
                                if EverySet.member Backend.Measurement.Model.CSectionInPast value.previousDelivery then
                                    Maybe.andThen (EverySet.toList >> List.head) value.cSectionReason
                                        |> Maybe.map
                                            (\cSectionReason ->
                                                let
                                                    previousDeliveryMethod =
                                                        if EverySet.member Backend.Measurement.Model.CSectionInPreviousDelivery value.previousDelivery then
                                                            translate language Translate.CSection

                                                        else
                                                            translate language Translate.VaginalDeliveryLabel
                                                in
                                                [ translate language Translate.CSectionFor
                                                    ++ " "
                                                    ++ String.toLower (translate language <| Translate.CSectionReasons cSectionReason)
                                                    ++ " "
                                                    ++ translate language Translate.WithMostRecentDeliveryBy
                                                    ++ " "
                                                    ++ String.toLower previousDeliveryMethod
                                                    ++ "."
                                                ]
                                            )
                                        |> Maybe.withDefault []

                                else
                                    []

                            conditionsDuringPrevoiusPregnancy =
                                case EverySet.toList value.signs of
                                    [] ->
                                        []

                                    [ NoObstetricHistoryStep2Sign ] ->
                                        []

                                    _ ->
                                        let
                                            conditions =
                                                EverySet.toList value.signs
                                                    |> List.map (Translate.ObstetricHistoryStep2Sign >> translate language)
                                                    |> String.join ", "
                                        in
                                        [ translate language Translate.ConditionsDuringPrevoiusPregnancy ++ ": " ++ conditions ]
                        in
                        cSectionInfo ++ conditionsDuringPrevoiusPregnancy
                    )
                |> Maybe.withDefault []

        medicalHistory =
            getMeasurementValueFunc measurements.medicalHistory
                |> Maybe.map
                    (\value ->
                        [ translate language Translate.FamilyHistoryOfPreeclampsia
                            ++ ": "
                            ++ (translate language <| Translate.OccursInFamilySign value.preeclampsiaInFamily)
                        ]
                    )
                |> Maybe.withDefault []

        content =
            obsetricHistory
                ++ obstetricHistoryStep2
                ++ medicalHistory
                |> List.map (\alert -> li [] [ text alert ])
                |> ul []
                |> List.singleton
    in
    div [ class "risk-factors" ]
        [ div [ class <| "pane-heading red" ]
            [ img [ src "assets/images/exclamation-white-outline.png" ] []
            , span [] [ text <| translate language Translate.ObstetricHistory ]
            ]
        , div [ class "pane-content" ] content
        ]


viewMedicalHistoryPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewMedicalHistoryPane language currentDate measurements =
    let
        medicalHistory =
            getMeasurementValueFunc measurements.medicalHistory
                |> Maybe.map
                    (\value ->
                        let
                            viewEntry resolveSignsFunc noSignsValue lableTransId signTransId =
                                let
                                    signs =
                                        resolveSignsFunc value
                                            |> EverySet.toList

                                    noSignsSelected =
                                        List.isEmpty signs
                                            || (List.length signs == 1)
                                            && (List.head signs
                                                    |> Maybe.map ((==) noSignsValue)
                                                    |> Maybe.withDefault False
                                               )
                                in
                                if noSignsSelected then
                                    []

                                else
                                    let
                                        conditions =
                                            List.map (signTransId >> translate language) signs
                                                |> String.join ", "
                                    in
                                    [ translate language lableTransId ++ ": " ++ conditions ]
                        in
                        [ viewEntry .signs NoMedicalHistorySigns Translate.MedicalConditions Translate.MedicalHistorySign
                        , viewEntry .physicalConditions
                            NoMedicalHistoryPhysicalCondition
                            Translate.PhysicalConditions
                            Translate.MedicalHistoryPhysicalCondition
                        , viewEntry .infectiousDiseases
                            NoMedicalHistoryInfectiousDisease
                            Translate.InfectiousDiseases
                            Translate.MedicalHistoryInfectiousDisease
                        , viewEntry .mentalHealthIssues
                            NoMedicalHistoryMentalHealthIssue
                            Translate.MentalHealthIssues
                            Translate.MedicalHistoryMentalHealthIssue
                        ]
                            |> List.concat
                    )
                |> Maybe.withDefault []

        content =
            List.map (\alert -> li [] [ text alert ]) medicalHistory
                |> ul []
                |> List.singleton
    in
    div [ class "risk-factors" ]
        [ div [ class <| "pane-heading red" ]
            [ img [ src "assets/images/exclamation-white-outline.png" ] []
            , span [] [ text <| translate language Translate.MedicalHistory ]
            ]
        , div [ class "pane-content" ] content
        ]


viewMedicalDiagnosisPane : Language -> NominalDate -> Bool -> PrenatalMeasurements -> AssembledData -> Html Msg
viewMedicalDiagnosisPane language currentDate isChw firstNurseEncounterMeasurements assembled =
    let
        allNurseEncountersData =
            generateAllNurseEncountersData isChw assembled
                |> List.sortWith (sortByDateDesc .startDate)

        diganoses =
            List.concatMap
                (\data ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicHypertensionDiagnoses data.startDate data.diagnoses assembled medicalDiagnoses

                        diagnosesEntries =
                            List.concatMap (viewTreatmentForDiagnosis language data.startDate data.measurements data.diagnoses) diagnosesIncludingChronic

                        outsideCareDiagnosesEntries =
                            getMeasurementValueFunc data.measurements.outsideCare
                                |> Maybe.andThen
                                    (\value ->
                                        Maybe.map
                                            (EverySet.toList
                                                >> List.filter (\diagnosis -> List.member diagnosis medicalDiagnoses)
                                                >> List.concatMap (viewTreatmentForOutsideCareDiagnosis language data.startDate value.medications)
                                            )
                                            value.diagnoses
                                    )
                                |> Maybe.withDefault []

                        knownAsPositiveEntries =
                            viewKnownPositives language data.startDate data.measurements

                        programReferralEntries =
                            getMeasurementValueFunc data.measurements.specialityCare
                                |> Maybe.map
                                    (\value ->
                                        let
                                            arvEntry =
                                                resolveARVReferralDiagnosis assembled.nursePreviousEncountersData
                                                    |> Maybe.map
                                                        (\diagnosis ->
                                                            if not <| EverySet.member EnrolledToARVProgram value then
                                                                viewProgramReferralEntry language data.startDate diagnosis FacilityARVProgram

                                                            else
                                                                []
                                                        )
                                                    |> Maybe.withDefault []

                                            ncdEntries =
                                                resolveNCDReferralDiagnoses assembled.nursePreviousEncountersData
                                                    |> List.concatMap
                                                        (\diagnosis ->
                                                            if not <| EverySet.member EnrolledToARVProgram value then
                                                                viewProgramReferralEntry language data.startDate diagnosis FacilityNCDProgram

                                                            else
                                                                []
                                                        )
                                        in
                                        arvEntry ++ ncdEntries
                                    )
                                |> Maybe.withDefault []

                        pastDiagnosesEntries =
                            EverySet.toList data.pastDiagnoses
                                |> List.filter (\diagnosis -> List.member diagnosis medicalDiagnoses)
                                |> List.concatMap (viewTreatmentForPastDiagnosis language data.startDate)
                    in
                    knownAsPositiveEntries
                        ++ diagnosesEntries
                        ++ outsideCareDiagnosesEntries
                        ++ pastDiagnosesEntries
                        ++ programReferralEntries
                )
                allNurseEncountersData
                |> List.append discordantCoupleStatus
                |> ul []

        discordantCoupleStatus =
            List.filterMap
                (\encounterData ->
                    let
                        byHIVTest =
                            getMeasurementValueFunc encounterData.measurements.hivTest
                                |> Maybe.andThen .hivSigns
                                |> Maybe.andThen
                                    (\hivSigns ->
                                        let
                                            partnerPositive =
                                                EverySet.member PartnerHIVPositive hivSigns

                                            takingARV =
                                                EverySet.member PartnerTakingARV hivSigns

                                            surpressedViralLoad =
                                                EverySet.member PartnerSurpressedViralLoad hivSigns
                                        in
                                        if partnerPositive then
                                            Just <| Translate.DiscordantCoupleStatus takingARV surpressedViralLoad

                                        else
                                            Nothing
                                    )

                        byPartnerHIVTest =
                            let
                                patientHIVNegative =
                                    getMeasurementValueFunc encounterData.measurements.hivTest
                                        |> Maybe.map
                                            (\value ->
                                                List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]
                                                    && (value.testResult == Just TestNegative)
                                            )
                                        |> Maybe.withDefault False
                            in
                            if patientHIVNegative then
                                getMeasurementValueFunc encounterData.measurements.partnerHIVTest
                                    |> Maybe.andThen
                                        (\value ->
                                            let
                                                partnerHIVPositive =
                                                    (value.executionNote == TestNoteKnownAsPositive)
                                                        || (List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]
                                                                && (value.testResult == Just TestPositive)
                                                           )
                                            in
                                            if partnerHIVPositive then
                                                Maybe.map
                                                    (\hivSigns ->
                                                        let
                                                            takingARV =
                                                                EverySet.member PartnerTakingARV hivSigns

                                                            surpressedViralLoad =
                                                                EverySet.member PartnerSurpressedViralLoad hivSigns
                                                        in
                                                        Translate.DiscordantCoupleStatus takingARV surpressedViralLoad
                                                    )
                                                    value.hivSigns

                                            else
                                                Nothing
                                        )

                            else
                                Nothing
                    in
                    Maybe.Extra.or byPartnerHIVTest byHIVTest
                )
                allNurseEncountersData
                |> List.head
                |> Maybe.map (translate language >> wrapWithLI)
                |> Maybe.withDefault []

        alerts =
            -- Alerts are displayed only for CHW.
            if isChw then
                List.filterMap
                    (generateMedicalDiagnosisAlertData language currentDate firstNurseEncounterMeasurements)
                    allMedicalDiagnoses
                    |> List.map (\alert -> li [] [ text alert ])
                    |> ul []
                    |> List.singleton

            else
                []
    in
    div [ class "medical-diagnosis" ]
        [ viewItemHeading language Translate.MedicalDiagnosis "blue"
        , div [ class "pane-content" ] <|
            diganoses
                :: alerts
        ]


viewProgramReferralEntry : Language -> NominalDate -> PrenatalDiagnosis -> ReferralFacility -> List (Html Msg)
viewProgramReferralEntry language date diagnosis facility =
    diagnosisForProgressReportToString language diagnosis
        ++ " - "
        ++ (translate language <| Translate.ReferredToFacilityPostpartum facility)
        ++ " "
        ++ (String.toLower <| translate language Translate.On)
        ++ " "
        ++ formatDDMMYYYY date
        |> wrapWithLI


viewObstetricalDiagnosisPane : Language -> NominalDate -> Bool -> Maybe LastMenstrualPeriodValue -> PrenatalMeasurements -> AssembledData -> Html Msg
viewObstetricalDiagnosisPane language currentDate isChw globalLmpValue firstNurseEncounterMeasurements assembled =
    let
        allNurseEncountersData =
            generateAllNurseEncountersData isChw assembled
                |> List.sortWith (sortByDateDesc .startDate)

        initialHealthEducationOccurances =
            List.foldr
                (\data accum ->
                    getMeasurementValueFunc data.measurements.healthEducation
                        |> Maybe.map
                            (\value ->
                                let
                                    signRecord sign =
                                        if
                                            EverySet.member sign value.signs
                                                && (isNothing <| Dict.get sign accum)
                                        then
                                            Just ( sign, data.startDate )

                                        else
                                            Nothing
                                in
                                [ signRecord EducationNauseaVomiting
                                , signRecord EducationLegCramps
                                , signRecord EducationLowBackPain
                                , signRecord EducationConstipation
                                , signRecord EducationVaricoseVeins
                                , signRecord EducationLegPainRedness
                                , signRecord EducationPelvicPain
                                ]
                                    |> Maybe.Extra.values
                                    |> Dict.fromList
                                    |> Dict.union accum
                            )
                        |> Maybe.withDefault accum
                )
                Dict.empty
                allNurseEncountersData

        -- RH Factor Uknown should be a default Obsteric Diagnosis on any patient who has
        -- not had an RH lab result (for any reason).
        rhesusEntry =
            let
                rhesusRecorded =
                    List.reverse allNurseEncountersData
                        |> List.map .measurements
                        |> List.filterMap (.bloodGpRsTest >> getMeasurementValueFunc >> Maybe.andThen .rhesus)
                        |> List.isEmpty
                        |> not
            in
            if not rhesusRecorded then
                translate language Translate.RHFactorUnknown
                    |> wrapWithLI

            else
                []

        dignoses =
            List.concatMap
                (\data ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicHypertensionDiagnoses data.startDate data.diagnoses assembled obstetricalDiagnoses

                        diagnosesEntries =
                            List.concatMap (viewTreatmentForDiagnosis language data.startDate data.measurements data.diagnoses) diagnosesIncludingChronic

                        outsideCareDiagnosesEntries =
                            getMeasurementValueFunc data.measurements.outsideCare
                                |> Maybe.andThen
                                    (\value ->
                                        Maybe.map
                                            (EverySet.toList
                                                >> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                                                >> List.concatMap (viewTreatmentForOutsideCareDiagnosis language data.startDate value.medications)
                                            )
                                            value.diagnoses
                                    )
                                |> Maybe.withDefault []

                        pastDiagnosesEntries =
                            EverySet.toList data.pastDiagnoses
                                |> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                                |> List.concatMap (viewTreatmentForPastDiagnosis language data.startDate)

                        healthEducationDiagnosesEntries =
                            getMeasurementValueFunc data.measurements.healthEducation
                                |> Maybe.map
                                    (\value ->
                                        let
                                            formatedDate =
                                                formatDDMMYYYY data.startDate

                                            messageForSign sign =
                                                if EverySet.member sign value.signs then
                                                    Dict.get sign initialHealthEducationOccurances
                                                        |> Maybe.map
                                                            (\initialDate ->
                                                                let
                                                                    currentIsInitial =
                                                                        Date.compare initialDate data.startDate == EQ
                                                                in
                                                                Translate.PrenatalHealthEducationSignsDiagnosis currentIsInitial formatedDate sign
                                                                    |> translate language
                                                                    |> wrapWithLI
                                                            )

                                                else
                                                    Nothing
                                        in
                                        [ messageForSign EducationNauseaVomiting
                                        , messageForSign EducationLegCramps
                                        , messageForSign EducationLowBackPain
                                        , messageForSign EducationConstipation
                                        , messageForSign EducationVaricoseVeins
                                        , messageForSign EducationLegPainRedness
                                        , messageForSign EducationPelvicPain
                                        ]
                                            |> Maybe.Extra.values
                                            |> List.concat
                                    )
                                |> Maybe.withDefault []
                    in
                    diagnosesEntries
                        ++ outsideCareDiagnosesEntries
                        ++ pastDiagnosesEntries
                        ++ healthEducationDiagnosesEntries
                )
                allNurseEncountersData

        lmpDateNonConfidentEntry =
            Maybe.map
                (\value ->
                    if value.confident == False then
                        Maybe.map
                            (Translate.LmpDateNotConfidentReasonforReport
                                >> translate language
                                >> wrapWithLI
                            )
                            value.notConfidentReason
                            |> Maybe.withDefault []

                    else
                        []
                )
                globalLmpValue
                |> Maybe.withDefault []

        common =
            ul [] <|
                rhesusEntry
                    ++ dignoses
                    ++ lmpDateNonConfidentEntry

        alerts =
            -- Alerts are displayed only for CHW.
            if isChw then
                List.filterMap
                    (generateObstetricalDiagnosisAlertData language currentDate isChw firstNurseEncounterMeasurements assembled)
                    allObstetricalDiagnoses
                    |> List.map (\alert -> li [] [ text alert ])
                    |> ul []
                    |> List.singleton

            else
                []
    in
    div [ class "obstetric-diagnosis" ]
        [ viewItemHeading language Translate.ObstetricalDiagnosis "blue"
        , div [ class "pane-content" ] <|
            common
                :: alerts
        ]


viewMedicationHistoryPane : Language -> NominalDate -> Bool -> AssembledData -> Html any
viewMedicationHistoryPane language currentDate isChw assembled =
    let
        allNurseEncountersData =
            generateAllNurseEncountersData isChw assembled
                |> List.sortWith (sortByDateDesc .startDate)

        resolveMedicationAdministrationDate measurementFunc =
            List.map (.measurements >> measurementFunc) allNurseEncountersData
                |> List.filterMap
                    (Maybe.andThen
                        (\( _, measurement ) ->
                            if measurement.value == AdministeredToday then
                                Just measurement.dateMeasured

                            else
                                Nothing
                        )
                    )
                |> List.head

        entriesHeading =
            div [ class "heading medication" ]
                [ div [ class "name" ] [ text <| translate language Translate.Medication ]
                , div [ class "date" ] [ text <| translate language Translate.DateReceived ]
                ]

        viewEntry medicationType administrationDate =
            let
                dateForView =
                    Maybe.map formatDDMMYYYY administrationDate
                        |> Maybe.withDefault "--"
            in
            div [ class "entry medication" ]
                [ div [ class "cell name" ] [ text <| translate language <| Translate.MedicationDistributionSign medicationType ]
                , div [ class "cell date" ] [ p [] [ text dateForView ] ]
                ]
    in
    div [ class "medication-history" ] <|
        [ viewItemHeading language Translate.MedicationHistory "blue"
        , div [ class "pane-content" ]
            [ entriesHeading
            , resolveMedicationAdministrationDate .aspirin
                |> viewEntry Aspirin
            , resolveMedicationAdministrationDate .calcium
                |> viewEntry Calcium
            , resolveMedicationAdministrationDate .fefol
                |> viewEntry Fefol
            , resolveMedicationAdministrationDate .folate
                |> viewEntry FolicAcid
            , resolveMedicationAdministrationDate .iron
                |> viewEntry Iron
            , resolveMedicationAdministrationDate .mms
                |> viewEntry MMS
            , resolveMedicationAdministrationDate .mebendazole
                |> viewEntry Mebendezole
            ]
        ]


viewVaccinationHistoryPane : Language -> NominalDate -> AssembledData -> Html any
viewVaccinationHistoryPane language currentDate assembled =
    div [ class "vaccination-history" ] <|
        [ viewItemHeading language Translate.ImmunizationHistory "blue"
        , div [ class "pane-content" ] <|
            viewVaccinationOverview language currentDate assembled
        ]


viewVaccinationOverview :
    Language
    -> NominalDate
    -> AssembledData
    -> List (Html any)
viewVaccinationOverview language currentDate assembled =
    let
        entriesHeading =
            div [ class "heading vaccination" ]
                [ div [ class "name" ] [ text <| translate language Translate.Immunisation ]
                , div [ class "date" ] [ text <| translate language Translate.DateReceived ]
                , div [ class "next-due" ] [ text <| translate language Translate.NextDue ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                ]

        futureVaccinationsData =
            generateFutureVaccinationsDataByProgress currentDate assembled
                |> Dict.fromList

        entries =
            Dict.toList assembled.vaccinationProgress
                |> List.map viewVaccinationEntry

        viewVaccinationEntry ( vaccineType, doses ) =
            let
                nextDue =
                    Dict.get vaccineType futureVaccinationsData
                        |> Maybe.Extra.join
                        |> Maybe.map Tuple.second

                nextDueText =
                    Maybe.map formatDDMMYYYY nextDue
                        |> Maybe.withDefault ""

                ( status, statusClass ) =
                    Maybe.map
                        (\dueDate ->
                            if Date.compare dueDate currentDate == LT then
                                ( StatusBehind, "behind" )

                            else
                                ( StatusUpToDate, "up-to-date" )
                        )
                        nextDue
                        |> Maybe.withDefault ( StatusCompleted, "completed" )
            in
            div [ class "entry vaccination" ]
                [ div [ class "cell name" ] [ text <| translate language <| Translate.PrenatalVaccineLabel vaccineType ]
                , Dict.values doses
                    |> List.sortWith Date.compare
                    |> List.map (formatDDMMYYYY >> text >> List.singleton >> p [])
                    |> div [ class "cell date" ]
                , div [ classList [ ( "cell next-due ", True ), ( "red", status == StatusBehind ) ] ]
                    [ text nextDueText ]
                , div [ class <| "cell status " ++ statusClass ]
                    [ text <| translate language <| Translate.VaccinationStatus status ]
                ]
    in
    entriesHeading :: entries


viewChwActivityPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewChwActivityPane language currentDate isChw assembled =
    let
        allMeasurementsWithDates =
            assembled.chwPreviousMeasurementsWithDates
                ++ (if isChw then
                        [ ( currentDate, assembled.encounter.encounterType, assembled.measurements ) ]

                    else
                        []
                   )
                |> List.sortWith (sortByDateDesc (\( date, _, _ ) -> date))

        activitiesWithDate =
            List.map
                (\( date, _, measurements ) ->
                    ( date, List.filter (matchCHWActivityAtEncounter measurements) allCHWActions )
                )
                allMeasurementsWithDates

        content =
            if not <| List.isEmpty actions then
                div [ class "heading" ]
                    [ div [ class "date" ] [ text <| translate language Translate.Date ]
                    , div [ class "chw-actions" ] [ text <| translate language Translate.Actions ]
                    ]
                    :: actions

            else
                []

        actions =
            List.map
                (\( date, activities ) ->
                    div [ class "table-row" ]
                        [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
                        , List.map
                            (\activity ->
                                li [ class <| chwActionToColor activity ]
                                    [ text <| translate language <| Translate.CHWAction activity ]
                            )
                            activities
                            |> ul [ class "chw-actions" ]
                        ]
                )
                activitiesWithDate
    in
    div [ class "chw-activities" ]
        [ viewItemHeading language Translate.ChwActivity "blue"
        , div [ class "pane-content" ]
            content
        ]


matchCHWActivityAtEncounter : PrenatalMeasurements -> CHWAction -> Bool
matchCHWActivityAtEncounter measurements activity =
    case activity of
        ActionPregnancyDating ->
            isJust measurements.lastMenstrualPeriod

        ActionLabs ->
            isJust measurements.pregnancyTest

        ActionDangerSignsPresent ->
            getMeasurementValueFunc measurements.dangerSigns
                |> Maybe.map
                    (\value ->
                        case EverySet.toList value.signs of
                            [] ->
                                False

                            [ NoDangerSign ] ->
                                False

                            _ ->
                                True
                    )
                |> Maybe.withDefault False

        ActionReferredToHealthCenter ->
            getMeasurementValueFunc measurements.sendToHC
                |> Maybe.andThen (.sendToHCSigns >> Maybe.map (EverySet.member ReferToHealthCenter))
                |> Maybe.withDefault False

        ActionAppointmentConfirmation ->
            isJust measurements.appointmentConfirmation

        ActionHealthEducation ->
            isJust measurements.healthEducation

        ActionBirthPlan ->
            isJust measurements.birthPlan


viewPatientProgressPane : Language -> NominalDate -> ZScore.Model.Model -> Bool -> Maybe LastMenstrualPeriodValue -> AssembledData -> Html Msg
viewPatientProgressPane language currentDate zscores isChw globalLmpValue assembled =
    let
        allNurseEncountersData =
            generateAllNurseEncountersData isChw assembled
                |> List.map (\data -> ( data.startDate, data.measurements ))

        encountersTrimestersData =
            allNurseEncountersData
                |> List.map
                    (\( date, _ ) ->
                        ( date
                        , getEncounterTrimesterData date assembled.globalLmpDate
                        )
                    )

        getTrimesterEncounters trimester =
            encountersTrimestersData
                |> List.filter (\t -> Tuple.second t == Just trimester)
                |> List.map Tuple.first

        encountersFirstTrimester =
            getTrimesterEncounters FirstTrimester

        encountersFirstTrimesterCount =
            List.length encountersFirstTrimester

        encountersSecondTrimester =
            getTrimesterEncounters SecondTrimester

        encountersSecondTrimesterCount =
            List.length encountersSecondTrimester

        encountersThirdTrimester =
            getTrimesterEncounters ThirdTrimester

        encountersThirdTrimesterCount =
            List.length encountersThirdTrimester

        fetalMovementsDate =
            allNurseEncountersData
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalMovement >> (==) True)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        fetalHeartRateDate =
            allNurseEncountersData
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalHeartRate >> (<) 0)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        egaWeeksDaysLabel language_ encounterDate lmpDate =
            let
                diffInDays =
                    diffDays lmpDate encounterDate
            in
            generateEGAWeeksDaysLabel language_ diffInDays

        ( eddLabel, fetalHeartRateLabel, fetalMovementsLabel ) =
            Maybe.map
                (\lmpDate ->
                    let
                        eddDate =
                            lmpToEDDDate lmpDate
                    in
                    ( div [ class "due-date-label" ]
                        [ div [] [ text <| translate language Translate.DueDate ++ ":" ]
                        , div []
                            [ text <|
                                (Date.day eddDate |> String.fromInt)
                                    ++ " "
                                    ++ translate language (Translate.ResolveMonth False (Date.month eddDate))
                            ]
                        ]
                    , fetalHeartRateDate
                        |> Maybe.map
                            (\date ->
                                div [ class "heart-rate-label" ]
                                    [ span [] [ text <| translate language Translate.FetalHeartRate ++ ": " ]
                                    , span [] [ egaWeeksDaysLabel language date lmpDate |> text ]
                                    ]
                            )
                        |> Maybe.withDefault emptyNode
                    , fetalMovementsDate
                        |> Maybe.map
                            (\date ->
                                div [ class "movements-label" ]
                                    [ span [] [ text <| translate language Translate.FetalMovement ++ ": " ]
                                    , span [] [ egaWeeksDaysLabel language date lmpDate |> text ]
                                    ]
                            )
                        |> Maybe.withDefault emptyNode
                    )
                )
                assembled.globalLmpDate
                |> Maybe.withDefault ( emptyNode, emptyNode, emptyNode )

        viewTrimesterTimeline trimester =
            let
                encounterIconWidth =
                    18

                currentEncounterTrimester =
                    if encountersThirdTrimesterCount > 0 then
                        ThirdTrimester

                    else if encountersSecondTrimesterCount > 0 then
                        SecondTrimester

                    else
                        FirstTrimester

                periodWidth =
                    case trimester of
                        FirstTrimester ->
                            (180 - encounterIconWidth * encountersFirstTrimesterCount) // (encountersFirstTrimesterCount + 1)

                        SecondTrimester ->
                            (180 - encounterIconWidth * encountersSecondTrimesterCount) // (encountersSecondTrimesterCount + 1)

                        ThirdTrimester ->
                            (210 - encounterIconWidth * encountersThirdTrimesterCount) // (encountersThirdTrimesterCount + 1)

                ( trimesterPeriodsColors, trimesterEncountersDates ) =
                    case trimester of
                        FirstTrimester ->
                            ( if currentEncounterTrimester == FirstTrimester then
                                List.repeat encountersFirstTrimesterCount "blue" ++ [ "gray" ]

                              else
                                List.repeat (encountersFirstTrimesterCount + 1) "blue"
                            , encountersFirstTrimester
                            )

                        SecondTrimester ->
                            ( if currentEncounterTrimester == SecondTrimester then
                                List.repeat encountersSecondTrimesterCount "blue" ++ [ "gray" ]

                              else if currentEncounterTrimester == FirstTrimester then
                                List.repeat (encountersSecondTrimesterCount + 1) "gray"

                              else
                                List.repeat (encountersSecondTrimesterCount + 1) "blue"
                            , encountersSecondTrimester
                            )

                        ThirdTrimester ->
                            ( if currentEncounterTrimester == ThirdTrimester then
                                List.repeat encountersThirdTrimesterCount "blue" ++ [ "gray" ]

                              else
                                List.repeat (encountersThirdTrimesterCount + 1) "gray"
                            , encountersThirdTrimester
                            )

                fetalMovementsIcon =
                    span [ class "fetal-movements" ]
                        [ img
                            [ src "assets/images/icon-fetal-movement.png"
                            , style "height" "30px"
                            ]
                            []
                        ]

                fetalHeartRateIcon rightMargin =
                    span
                        [ class "fetal-heart-rate"
                        , style "margin-right" rightMargin
                        ]
                        [ img
                            [ src "assets/images/icon-fetal-heartrate.png"
                            , style "height" "30px"
                            ]
                            []
                        ]

                dueDateInfo =
                    if trimester == ThirdTrimester then
                        let
                            -- At 36 weeks and after and if Cephalic condition is present, the head of the icon
                            -- should face upwards, with no Cephalic condition, it should face downward.
                            -- Before 36 weeks the heads should face upward.
                            babyHeadDown =
                                Maybe.map
                                    (\lmpDate ->
                                        let
                                            currentWeek =
                                                diffDays lmpDate currentDate // 7
                                        in
                                        if currentWeek < 36 then
                                            False

                                        else
                                            let
                                                currentFetalPresentation =
                                                    List.reverse allNurseEncountersData
                                                        |> List.filterMap
                                                            (\( _, measurements ) ->
                                                                getMeasurementValueFunc measurements.obstetricalExam
                                                                    |> Maybe.map .fetalPresentation
                                                            )
                                                        |> List.head
                                            in
                                            Maybe.map ((/=) Cephalic) currentFetalPresentation
                                                |> Maybe.withDefault True
                                    )
                                    assembled.globalLmpDate
                                    |> Maybe.withDefault False
                        in
                        div [ class "due-date-info" ]
                            [ span [ class "due-date-icon" ]
                                [ img
                                    [ classList [ ( "rotate", babyHeadDown ) ]
                                    , src "assets/images/icon-baby-due-date.png"
                                    ]
                                    []
                                ]
                            , eddLabel
                            ]

                    else
                        emptyNode

                trimesterPeriods =
                    trimesterPeriodsColors
                        |> List.map
                            (\color ->
                                p
                                    [ class <| "period " ++ color
                                    , style "width" (String.fromInt periodWidth ++ "px")
                                    ]
                                    []
                            )

                timelineIcons date =
                    if fetalMovementsDate == Just date && fetalHeartRateDate == Just date then
                        div [ style "margin-left" "-25px", style "width" "65px" ]
                            [ fetalHeartRateIcon "5px"
                            , fetalMovementsIcon
                            ]

                    else if fetalHeartRateDate == Just date then
                        div [ style "margin-left" "-6px", style "width" "35px" ] [ fetalHeartRateIcon "0" ]

                    else if fetalMovementsDate == Just date then
                        div [ style "margin-left" "-2px", style "width" "30px" ] [ fetalMovementsIcon ]

                    else
                        emptyNode

                trimesterEncounters =
                    trimesterEncountersDates
                        |> List.map
                            (\date ->
                                span [ style "width" (String.fromInt encounterIconWidth ++ "px") ]
                                    [ img
                                        [ src "assets/images/icon-blue-circle.png"
                                        , style "width" (String.fromInt encounterIconWidth ++ "px")
                                        ]
                                        []
                                    , timelineIcons date
                                    ]
                            )
            in
            List.Extra.interweave trimesterPeriods trimesterEncounters
                |> List.append [ dueDateInfo ]
                |> div [ class "trimester-timeline" ]

        viewTrimesterVisits trimester =
            let
                ( expectedVisits, actualVisists, visitsLabel ) =
                    case trimester of
                        FirstTrimester ->
                            ( 1, encountersFirstTrimesterCount, Translate.OneVisit )

                        SecondTrimester ->
                            ( 2, encountersSecondTrimesterCount, Translate.TwoVisits )

                        ThirdTrimester ->
                            ( 5, encountersThirdTrimesterCount, Translate.FiveVisits )

                actualVisists_ =
                    if actualVisists > expectedVisits then
                        expectedVisits

                    else
                        actualVisists

                missingVisits =
                    expectedVisits - actualVisists_

                visitsView =
                    List.repeat actualVisists_ "icon-checked-green-circle.png"
                        ++ List.repeat missingVisits "icon-gray-circle-small.png"
                        |> List.map (\icon -> img [ src <| "assets/images/" ++ icon ] [])
            in
            div [ class "trimester-visits" ]
                [ div [ class "label-trimester" ] [ text <| translate language <| Translate.PregnancyTrimester trimester ]
                , div [ class "details" ]
                    [ div [ class "label-visit" ] [ text <| translate language visitsLabel ]
                    , div [ class "visits" ] visitsView
                    ]
                ]

        viewChartHeading transId =
            div [ class "chart-heading" ]
                [ img [ src <| "assets/images/icon-gray-circle-small.png" ] []
                , span [] [ text <| translate language transId ]
                ]

        egaBmiValues =
            List.filterMap
                (\( date, measurements ) ->
                    Maybe.map
                        (\lmpDate ->
                            let
                                bmi =
                                    Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> getHeightValue

                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> weightValueFunc
                                            in
                                            calculateBmi (Just height) (Just weight)
                                                |> Maybe.withDefault 0
                                        )
                                        measurements.nutrition
                                        |> Maybe.withDefault 0
                            in
                            ( diffDays lmpDate date, bmi )
                        )
                        assembled.globalLmpDate
                )
                allNurseEncountersData

        egaFundalHeightValues =
            Maybe.map
                (\lmpDate ->
                    List.filterMap
                        (\( date, measurements ) ->
                            measurements.obstetricalExam
                                |> Maybe.andThen
                                    (Tuple.second
                                        >> .value
                                        >> .fundalHeight
                                        >> Maybe.map getHeightValue
                                    )
                                |> Maybe.map
                                    (\fundalHeight ->
                                        ( diffDays lmpDate date, fundalHeight )
                                    )
                        )
                        allNurseEncountersData
                )
                assembled.globalLmpDate
                |> Maybe.withDefault []

        viewUnsureOfLmp =
            Maybe.map
                (\value ->
                    if value.confident == False then
                        p [ class "lmp-warning" ] [ text <| translate language Translate.UnsureOfLmp ]

                    else
                        emptyNode
                )
                globalLmpValue
                |> Maybe.withDefault emptyNode

        weightGainForEGAChart =
            let
                prePregnancyWeight =
                    resolvePrePregnancyWeight assembled |> Maybe.map weightValueFunc

                height =
                    resolveMeasuredHeight assembled |> Maybe.map getHeightValue
            in
            calculateBmi height prePregnancyWeight
                |> resolvePrePregnancyClassification zscores assembled
                |> Maybe.map2
                    (\baselineWeight prePregnancyClassification ->
                        let
                            egaWeightGainValues =
                                Maybe.map
                                    (\lmpDate ->
                                        List.filterMap
                                            (\( date, measurements ) ->
                                                measurements.nutrition
                                                    |> Maybe.map
                                                        (Tuple.second
                                                            >> .value
                                                            >> .weight
                                                            >> weightValueFunc
                                                            >> (\weight ->
                                                                    ( diffDays lmpDate date, weight - baselineWeight )
                                                               )
                                                        )
                                            )
                                            allNurseEncountersData
                                    )
                                    assembled.globalLmpDate
                                    |> Maybe.withDefault []
                        in
                        div [ class "weight-gain-info" ]
                            [ viewChartHeading Translate.WeightGain
                            , weightGainTable language currentDate assembled.globalLmpDate baselineWeight allNurseEncountersData
                            , viewWeightGainForEGA language
                                (weightGainStandardsPerPrePregnancyClassification prePregnancyClassification)
                                egaWeightGainValues
                            , illustrativePurposes language
                            ]
                    )
                    prePregnancyWeight
                |> Maybe.withDefault emptyNode
    in
    div [ class "patient-progress" ]
        [ viewItemHeading language Translate.PatientProgress "blue"
        , div [ class "pane-content" ]
            [ div [ class "caption timeline" ] [ text <| translate language Translate.ProgressTimeline ++ ":" ]
            , viewUnsureOfLmp
            , div [ class "timeline-section" ]
                [ div [ class "indicators" ]
                    [ fetalHeartRateLabel
                    , fetalMovementsLabel
                    ]
                , allTrimesters
                    |> List.map viewTrimesterTimeline
                    |> div [ class "timeline" ]
                ]
            , allTrimesters
                |> List.map viewTrimesterVisits
                |> div [ class "visits-section" ]
            , div [ class "caption trends" ] [ text <| translate language Translate.ProgressTrends ++ ":" ]
            , div [ class "trends-section" ]
                [ viewMarkers
                , div [ class "bmi-info" ]
                    [ viewChartHeading Translate.BMI
                    , heightWeightBMITable language currentDate assembled.globalLmpDate allNurseEncountersData
                    , viewBMIForEGA language egaBmiValues
                    , illustrativePurposes language
                    ]
                , weightGainForEGAChart
                , div [ class "fundal-height-info" ]
                    [ viewChartHeading Translate.FundalHeight
                    , fundalHeightTable language currentDate assembled.globalLmpDate allNurseEncountersData
                    , viewFundalHeightForEGA language egaFundalHeightValues
                    , illustrativePurposes language
                    ]
                ]
            ]
        ]


tableEgaHeading : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
tableEgaHeading language currentDate maybeLmpDate measurementsWithDates =
    measurementsWithDates
        |> List.map
            (\( date, _ ) ->
                maybeLmpDate
                    |> Maybe.map
                        (\lmpDate ->
                            diffDays lmpDate date
                                |> generateEGAWeeksDaysLabel language
                                |> String.toLower
                                |> text
                                |> List.singleton
                        )
                    |> Maybe.withDefault [ text "--" ]
                    |> th
                        [ classList
                            [ ( "center", True )
                            , ( "bottom", True )
                            , ( "aligned", True )
                            , ( "ega-header", True )
                            ]
                        ]
            )
        |> (::)
            (th
                [ class "uppercase" ]
                [ text <| translate language Translate.Ega ]
            )
        |> tr []


heightWeightBMITable : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
heightWeightBMITable language currentDate maybeLmpDate allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    allMeasurementsWithDates
        |> greedyGroupsOf 6
        |> List.concatMap
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| String.fromFloat height ++ translate language Translate.UnitCentimeter ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Height)
                            |> tr []

                    weights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)
                                            in
                                            [ text <| String.fromFloat weight ++ translate language Translate.KilogramShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Weight)
                            |> tr []

                    bmis =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)

                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)

                                                bmi =
                                                    calculateBmi (Just height) (Just weight)
                                                        |> Maybe.withDefault 0
                                                        |> Round.round 1
                                            in
                                            [ text bmi ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.BMI)
                            |> tr []
                in
                [ egas
                , heights
                , weights
                , bmis
                ]
            )
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


fundalHeightTable : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
fundalHeightTable language currentDate maybeLmpDate allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    allMeasurementsWithDates
        |> greedyGroupsOf 6
        |> List.concatMap
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .obstetricalExam
                                    >> Maybe.andThen
                                        (Tuple.second
                                            >> .value
                                            >> .fundalHeight
                                            >> Maybe.map
                                                (\heightInCm ->
                                                    [ text <|
                                                        String.fromFloat
                                                            (getHeightValue heightInCm)
                                                            ++ translate language Translate.UnitCentimeter
                                                    ]
                                                )
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.FundalHeight)
                            |> tr []
                in
                [ egas
                , heights
                ]
            )
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


weightGainTable : Language -> NominalDate -> Maybe NominalDate -> Float -> List ( NominalDate, PrenatalMeasurements ) -> Html any
weightGainTable language currentDate maybeLmpDate baselineWeight allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    greedyGroupsOf 6 allMeasurementsWithDates
        |> List.concatMap
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    weights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> .weight
                                            >> weightValueFunc
                                            >> (\weight ->
                                                    [ text <|
                                                        Round.round 1
                                                            (weight - baselineWeight)
                                                            ++ translate language Translate.KilogramShorthand
                                                    ]
                                               )
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.WeightGain)
                            |> tr []
                in
                [ egas
                , weights
                ]
            )
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


illustrativePurposes : Language -> Html any
illustrativePurposes language =
    div [ class "illustrative-purposes" ] [ text <| translate language Translate.ForIllustrativePurposesOnly ]


generateLabsResultsPaneData :
    NominalDate
    -> Bool
    -> AssembledData
    -> LabsResultsValues PrenatalEncounterId
generateLabsResultsPaneData currentDate viewForConfirmation assembled =
    let
        allMeasurements =
            assembled.measurements
                :: (if viewForConfirmation then
                        -- Confirmation is for current encounter only, so there's
                        -- no need to include data from previous encounters.
                        []

                    else
                        List.map .measurements assembled.nursePreviousEncountersData
                   )

        extractValues getMeasurementFunc =
            List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
                allMeasurements
    in
    { hiv = extractValues .hivTest
    , urineDipstick = extractValues .urineDipstickTest
    , randomBloodSugar = extractValues .randomBloodSugarTest
    , hivPCR = extractValues .hivPCRTest
    , partnerHIV = extractValues .partnerHIVTest
    , syphilis = extractValues .syphilisTest
    , hepatitisB = extractValues .hepatitisBTest
    , malaria = extractValues .malariaTest
    , hemoglobin = extractValues .hemoglobinTest
    , bloodGpRs = extractValues .bloodGpRsTest
    , creatinine = []
    , liverFunction = []
    , pregnancy = []
    , hba1c = []
    , lipidPanel = []
    }


viewProgressPhotosPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewProgressPhotosPane language currentDate isChw assembled =
    let
        allNurseEncountersData =
            generateAllNurseEncountersData isChw assembled
                |> List.map (\data -> ( data.startDate, data.measurements ))

        content =
            allNurseEncountersData
                |> List.filterMap
                    (\( date, measurements ) ->
                        measurements.prenatalPhoto
                            |> Maybe.map
                                (Tuple.second
                                    >> .value
                                    >> (\photoUrl ->
                                            let
                                                egaLabel =
                                                    assembled.globalLmpDate
                                                        |> Maybe.map (\lmpDate -> diffDays lmpDate date |> generateEGAWeeksDaysLabel language)
                                                        |> Maybe.withDefault ""
                                            in
                                            div [ class "progress-photo" ]
                                                [ viewPhotoThumbFromImageUrl photoUrl
                                                , div [ class "ega" ] [ text egaLabel ]
                                                ]
                                       )
                                )
                    )
    in
    div [ class "progress-photos" ]
        [ viewItemHeading language Translate.ProgressPhotos "blue"
        , div [ class "pane-content" ] content
        ]


viewKnownPositives :
    Language
    -> NominalDate
    -> PrenatalMeasurements
    -> List (Html any)
viewKnownPositives language date measurements =
    let
        resolveKnownPositive getMeasurementFunc knownPositiveTransId =
            getMeasurementFunc measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        if value.executionNote == TestNoteKnownAsPositive then
                            li []
                                [ text <|
                                    translate language knownPositiveTransId
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                ]

                        else
                            emptyNode
                    )
                |> Maybe.withDefault emptyNode
    in
    [ resolveKnownPositive .hivTest Translate.KnownPositiveHIV
    , resolveKnownPositive .hepatitisBTest Translate.KnownPositiveHepatitisB
    ]


viewTreatmentForDiagnosis :
    Language
    -> NominalDate
    -> PrenatalMeasurements
    -> EverySet PrenatalDiagnosis
    -> PrenatalDiagnosis
    -> List (Html any)
viewTreatmentForDiagnosis language date measurements allDiagnoses diagnosis =
    let
        diagnosisForProgressReport =
            diagnosisForProgressReportToString language diagnosis

        hypertensionTreatmentMessage =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
                        >> List.head
                        >> Maybe.map
                            (\treatmentSign ->
                                if treatmentSign == NoTreatmentForHypertension then
                                    noTreatmentAdministeredMessage

                                else
                                    let
                                        continued =
                                            if EverySet.member diagnosis allDiagnoses then
                                                ""

                                            else
                                                " (" ++ (String.toLower <| translate language Translate.Continued) ++ ") "

                                        treatmentLabel =
                                            case treatmentSign of
                                                TreatmentHypertensionAddCarvedilol ->
                                                    [ TreatmentMethyldopa4, TreatmentHypertensionAddCarvedilol ]
                                                        |> List.map (Translate.RecommendedTreatmentSignLabel >> translate language)
                                                        |> String.join ", "

                                                TreatmentHypertensionAddAmlodipine ->
                                                    [ TreatmentMethyldopa4, TreatmentHypertensionAddCarvedilol, TreatmentHypertensionAddAmlodipine ]
                                                        |> List.map (Translate.RecommendedTreatmentSignLabel >> translate language)
                                                        |> String.join ", "

                                                _ ->
                                                    translate language <| Translate.RecommendedTreatmentSignLabel treatmentSign
                                    in
                                    diagnosisForProgressReport
                                        ++ continued
                                        ++ " - "
                                        ++ (String.toLower <| translate language Translate.TreatedWith)
                                        ++ " "
                                        ++ treatmentLabel
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                            )
                        >> Maybe.withDefault noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        syphilisTreatmentMessage complications =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForSyphilis)
                        >> (\treatment ->
                                if List.isEmpty treatment then
                                    noTreatmentRecordedMessage

                                else if List.member NoTreatmentForSyphilis treatment then
                                    noTreatmentAdministeredMessage

                                else
                                    let
                                        treatedWithMessage =
                                            List.head treatment
                                                |> Maybe.map
                                                    (\medication ->
                                                        " - "
                                                            ++ (String.toLower <| translate language Translate.TreatedWith)
                                                            ++ " "
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabel medication)
                                                    )
                                                |> Maybe.withDefault ""
                                    in
                                    diagnosisForProgressReport
                                        ++ complications
                                        ++ treatedWithMessage
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                           )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        malariaTreatmentMessage =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForMalaria)
                        >> (\treatment ->
                                if List.isEmpty treatment then
                                    noTreatmentRecordedMessage

                                else if List.member NoTreatmentForMalaria treatment then
                                    noTreatmentAdministeredMessage

                                else if List.member TreatmentReferToHospital treatment then
                                    referredToHospitalMessage

                                else if List.member TreatmentWrittenProtocols treatment then
                                    translate language Translate.MalariaWithGIComplications
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        ++ " - "
                                        ++ translate language Translate.WrittenProtocolsFollowed
                                        |> wrapWithLI

                                else
                                    let
                                        treatedWithMessage =
                                            List.head treatment
                                                |> Maybe.map
                                                    (\medication ->
                                                        " - "
                                                            ++ (String.toLower <| translate language Translate.TreatedWith)
                                                            ++ " "
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabelForProgressReport medication)
                                                    )
                                                |> Maybe.withDefault ""
                                    in
                                    diagnosisForProgressReport
                                        ++ treatedWithMessage
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                           )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        mentalHealthMessage =
            translate language Translate.EPDSPreformedOn
                ++ " "
                ++ formatDDMMYYYY date
                ++ " - "
                ++ diagnosisForProgressReport
                |> wrapWithLI

        mastitisMessage =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForMastitis)
                        >> (\treatment ->
                                if List.isEmpty treatment then
                                    noTreatmentRecordedMessage

                                else if List.member NoTreatmentForMastitis treatment then
                                    noTreatmentAdministeredMessage

                                else
                                    let
                                        treatedWithMessage =
                                            List.head treatment
                                                |> Maybe.map
                                                    (\medication ->
                                                        " - "
                                                            ++ (String.toLower <| translate language Translate.TreatedWith)
                                                            ++ " "
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabel medication)
                                                    )
                                                |> Maybe.withDefault ""
                                    in
                                    diagnosisForProgressReport
                                        ++ treatedWithMessage
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                           )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        referredToHospitalMessage =
            referredToHospitalMessageWithComplications ""

        referredToHospitalMessageWithComplications complications =
            referredToFacilityMessageWithComplications FacilityHospital complications

        referredToFacilityMessage facility =
            referredToFacilityMessageWithComplications facility ""

        referredToFacilityMessageWithComplications facility complications =
            if isNothing measurements.sendToHC then
                noTreatmentRecordedMessageWithComplications complications

            else
                let
                    refferedToFacility =
                        getMeasurementValueFunc measurements.sendToHC
                            |> Maybe.andThen .referToFacilitySigns
                            |> Maybe.map
                                (\referToFacilitySigns ->
                                    case facility of
                                        FacilityHospital ->
                                            EverySet.member ReferToHospital referToFacilitySigns

                                        FacilityMentalHealthSpecialist ->
                                            EverySet.member ReferToMentalHealthSpecialist referToFacilitySigns

                                        FacilityARVProgram ->
                                            EverySet.member ReferToARVProgram referToFacilitySigns

                                        FacilityNCDProgram ->
                                            EverySet.member ReferToNCDProgram referToFacilitySigns

                                        FacilityANCServices ->
                                            -- Explicit NCD facility.
                                            False

                                        FacilityUltrasound ->
                                            EverySet.member ReferToUltrasound referToFacilitySigns

                                        FacilityHealthCenter ->
                                            -- We should never get here.
                                            False
                                )
                            |> Maybe.withDefault False
                in
                if refferedToFacility then
                    diagnosisForProgressReport
                        ++ complications
                        ++ " - "
                        ++ (String.toLower <| translate language <| Translate.ReferredToFacility facility)
                        ++ " "
                        ++ (String.toLower <| translate language Translate.On)
                        ++ " "
                        ++ formatDDMMYYYY date
                        |> wrapWithLI

                else
                    let
                        reason =
                            getMeasurementValueFunc measurements.sendToHC
                                |> Maybe.andThen
                                    (\value ->
                                        case facility of
                                            FacilityHospital ->
                                                getCurrentReasonForNonReferral NonReferralReasonHospital value.facilityNonReferralReasons

                                            FacilityMentalHealthSpecialist ->
                                                getCurrentReasonForNonReferral NonReferralReasonMentalHealthSpecialist value.facilityNonReferralReasons

                                            FacilityARVProgram ->
                                                getCurrentReasonForNonReferral NonReferralReasonARVProgram value.facilityNonReferralReasons

                                            FacilityNCDProgram ->
                                                getCurrentReasonForNonReferral NonReferralReasonNCDProgram value.facilityNonReferralReasons

                                            FacilityANCServices ->
                                                -- Explicit NCD facility.
                                                Nothing

                                            FacilityUltrasound ->
                                                getCurrentReasonForNonReferral NonReferralReasonUltrasound value.facilityNonReferralReasons

                                            FacilityHealthCenter ->
                                                -- We should never get here.
                                                Nothing
                                    )

                        suffix =
                            Maybe.map
                                (\reason_ ->
                                    if reason_ == NoReasonForNonReferral then
                                        ""

                                    else
                                        " - " ++ (String.toLower <| translate language <| Translate.ReasonForNonReferral reason_)
                                )
                                reason
                                |> Maybe.withDefault ""
                    in
                    diagnosisForProgressReport
                        ++ complications
                        ++ " - "
                        ++ (String.toLower <| translate language <| Translate.ReferredToFacilityNot facility)
                        ++ " "
                        ++ (String.toLower <| translate language Translate.On)
                        ++ " "
                        ++ formatDDMMYYYY date
                        ++ suffix
                        |> wrapWithLI

        undeterminedDiagnosisMessage =
            diagnosisForProgressReport
                ++ " - "
                ++ translate language Translate.UndeterminedDiagnosisMessage
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI

        noTreatmentRecordedMessage =
            noTreatmentRecordedMessageWithComplications ""

        noTreatmentRecordedMessageWithComplications complication =
            diagnosisForProgressReport
                ++ complication
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                ++ " - "
                ++ (String.toLower <| translate language Translate.NoTreatmentRecorded)
                |> wrapWithLI

        noTreatmentAdministeredMessage =
            diagnosisForProgressReport
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                ++ " - "
                ++ (String.toLower <| translate language Translate.NoTreatmentAdministered)
                |> wrapWithLI

        treatmentMessageForMedication =
            translate language Translate.TreatedWith
                |> customTreatmentMessageForMedication

        customTreatmentMessageForMedication treatmentLabel distributionSigns nonAdministrationReasons medication =
            if EverySet.member medication distributionSigns then
                treatmentLabel
                    ++ " "
                    ++ (translate language <| Translate.MedicationDistributionSign medication)
                    |> Just

            else
                Dict.get medication nonAdministrationReasons
                    |> Maybe.map
                        (\nonAdministrationReason ->
                            translate language Translate.TreatedWithNot
                                ++ " "
                                ++ (translate language <| Translate.MedicationDistributionSign medication)
                                ++ " "
                                ++ (String.toLower <| translate language Translate.DueTo)
                                ++ " "
                                ++ (translate language <| Translate.AdministrationNote nonAdministrationReason)
                        )

        diagnosisTreatedWithOnDateMessage recommendedTreatmentSign =
            diagnosisForProgressReport
                ++ " - "
                ++ (String.toLower <| translate language Translate.TreatedWith)
                ++ " "
                ++ (translate language <| Translate.RecommendedTreatmentSignLabel recommendedTreatmentSign)
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI
    in
    case diagnosis of
        DiagnosisHIVInitialPhase ->
            getMeasurementValueFunc measurements.sendToHC
                |> Maybe.map
                    (\value ->
                        let
                            refferedToARVProgram =
                                Maybe.map (EverySet.member ReferToARVProgram)
                                    value.referToFacilitySigns
                                    |> Maybe.withDefault False
                        in
                        if refferedToARVProgram then
                            referredToFacilityMessage FacilityARVProgram

                        else
                            getMeasurementValueFunc measurements.medicationDistribution
                                |> Maybe.andThen
                                    (\value_ ->
                                        let
                                            nonAdministrationReasons =
                                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value_
                                        in
                                        Maybe.map2
                                            (\tdf3TCTreatmentMessage dolutegravirTreatmentmessage ->
                                                let
                                                    diagnosisMessage =
                                                        diagnosisForProgressReport
                                                            ++ " "
                                                            ++ (String.toLower <| translate language Translate.On)
                                                            ++ " "
                                                            ++ formatDDMMYYYY date
                                                in
                                                [ li []
                                                    [ p [] [ text diagnosisMessage ]
                                                    , p [] [ text tdf3TCTreatmentMessage ]
                                                    , p [] [ text dolutegravirTreatmentmessage ]
                                                    ]
                                                ]
                                            )
                                            (treatmentMessageForMedication value_.distributionSigns nonAdministrationReasons TDF3TC)
                                            (treatmentMessageForMedication value_.distributionSigns nonAdministrationReasons Dolutegravir)
                                    )
                                |> Maybe.withDefault noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisHIVRecurrentPhase ->
            viewTreatmentForDiagnosis language date measurements allDiagnoses DiagnosisHIVInitialPhase

        DiagnosisHIVDetectableViralLoadInitialPhase ->
            getMeasurementValueFunc measurements.hivPCRTest
                |> Maybe.andThen .hivViralLoad
                |> Maybe.map
                    (\viralLoad ->
                        diagnosisForProgressReportToString language diagnosis
                            ++ " "
                            ++ (String.toLower <| translate language Translate.On)
                            ++ " "
                            ++ formatDDMMYYYY date
                            ++ " -- "
                            ++ String.fromFloat viralLoad
                            |> wrapWithLI
                    )
                |> Maybe.withDefault []

        DiagnosisHIVDetectableViralLoadRecurrentPhase ->
            viewTreatmentForDiagnosis language date measurements allDiagnoses DiagnosisHIVDetectableViralLoadInitialPhase

        DiagnosisDiscordantPartnershipInitialPhase ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        treatmentMessageForMedication value.distributionSigns nonAdministrationReasons TDF3TC
                            |> Maybe.map
                                (\tdf3TCTreatmentMessage ->
                                    let
                                        diagnosisMessage =
                                            diagnosisForProgressReport
                                                ++ " "
                                                ++ (String.toLower <| translate language Translate.On)
                                                ++ " "
                                                ++ formatDDMMYYYY date
                                    in
                                    [ li []
                                        [ p [] [ text diagnosisMessage ]
                                        , p [] [ text tdf3TCTreatmentMessage ]
                                        ]
                                    ]
                                )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisDiscordantPartnershipRecurrentPhase ->
            viewTreatmentForDiagnosis language date measurements allDiagnoses DiagnosisDiscordantPartnershipInitialPhase

        DiagnosisSyphilisInitialPhase ->
            syphilisTreatmentMessage ""

        DiagnosisSyphilisRecurrentPhase ->
            syphilisTreatmentMessage ""

        DiagnosisSyphilisWithComplicationsInitialPhase ->
            let
                complications =
                    getMeasurementValueFunc measurements.syphilisTest
                        |> Maybe.andThen .symptoms
                        |> Maybe.map
                            (\symptoms ->
                                if EverySet.isEmpty symptoms then
                                    ""

                                else if EverySet.member NoIllnessSymptoms symptoms then
                                    ""

                                else
                                    " - ["
                                        ++ (EverySet.toList symptoms
                                                |> List.map (Translate.IllnessSymptom >> translate language)
                                                |> String.join ", "
                                           )
                                        ++ "]"
                            )
                        |> Maybe.withDefault ""
            in
            syphilisTreatmentMessage complications

        DiagnosisSyphilisWithComplicationsRecurrentPhase ->
            viewTreatmentForDiagnosis language date measurements allDiagnoses DiagnosisSyphilisWithComplicationsInitialPhase

        DiagnosisChronicHypertensionImmediate ->
            hypertensionTreatmentMessage

        DiagnosisChronicHypertensionAfterRecheck ->
            hypertensionTreatmentMessage

        DiagnosisGestationalHypertensionImmediate ->
            hypertensionTreatmentMessage

        DiagnosisGestationalHypertensionAfterRecheck ->
            hypertensionTreatmentMessage

        DiagnosisEclampsia ->
            referredToHospitalMessage

        DiagnosisMiscarriage ->
            referredToHospitalMessage

        DiagnosisMolarPregnancy ->
            referredToHospitalMessage

        DiagnosisPlacentaPrevia ->
            referredToHospitalMessage

        DiagnosisPlacentalAbruption ->
            referredToHospitalMessage

        DiagnosisUterineRupture ->
            referredToHospitalMessage

        DiagnosisObstructedLabor ->
            referredToHospitalMessage

        DiagnosisPostAbortionSepsis ->
            referredToHospitalMessage

        DiagnosisEctopicPregnancy ->
            referredToHospitalMessage

        DiagnosisPROM ->
            referredToHospitalMessage

        DiagnosisPPROM ->
            referredToHospitalMessage

        DiagnosisHyperemesisGravidum ->
            referredToHospitalMessage

        DiagnosisHyperemesisGravidumBySymptoms ->
            referredToHospitalMessage

        DiagnosisSevereVomiting ->
            referredToHospitalMessage

        DiagnosisSevereVomitingBySymptoms ->
            referredToHospitalMessage

        DiagnosisMaternalComplications ->
            referredToHospitalMessage

        DiagnosisInfection ->
            referredToHospitalMessage

        DiagnosisImminentDelivery ->
            referredToHospitalMessage

        DiagnosisLaborAndDelivery ->
            referredToHospitalMessage

        DiagnosisModerateAnemiaInitialPhase ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        Maybe.map2
                            (\ironTreatmentMessage folicAcidTreatmentMessage ->
                                let
                                    diagnosisMessage =
                                        diagnosisForProgressReport
                                            ++ " "
                                            ++ (String.toLower <| translate language Translate.On)
                                            ++ " "
                                            ++ formatDDMMYYYY date
                                in
                                [ li []
                                    [ p [] [ text diagnosisMessage ]
                                    , p [] [ text ironTreatmentMessage ]
                                    , p [] [ text folicAcidTreatmentMessage ]
                                    ]
                                ]
                            )
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons Iron)
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons FolicAcid)
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisModerateAnemiaRecurrentPhase ->
            viewTreatmentForDiagnosis language date measurements allDiagnoses DiagnosisModerateAnemiaInitialPhase

        DiagnosisSevereAnemiaInitialPhase ->
            referredToHospitalMessage

        DiagnosisSevereAnemiaRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisSevereAnemiaWithComplicationsInitialPhase ->
            let
                complication =
                    " - ["
                        ++ (complicationsByExamination
                                ++ complicationsByDangerSigns
                                ++ elevatedRespiratoryRate
                                |> String.join ", "
                           )
                        ++ "]"

                elevatedRespiratoryRate =
                    if respiratoryRateElevated measurements then
                        [ translate language Translate.ElevatedRespiratoryRate ]

                    else
                        []

                complicationsByDangerSigns =
                    getMeasurementValueFunc measurements.dangerSigns
                        |> Maybe.map
                            (\value ->
                                if EverySet.member DifficultyBreathing value.signs then
                                    [ translate language <| Translate.DangerSign DifficultyBreathing ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []

                complicationsByExamination =
                    getMeasurementValueFunc measurements.corePhysicalExam
                        |> Maybe.map
                            (\exam ->
                                let
                                    pallorHands =
                                        if EverySet.member PallorHands exam.hands then
                                            [ translate language Translate.HandPallor ]

                                        else
                                            []

                                    paleConjuctiva =
                                        if EverySet.member PaleConjuctiva exam.eyes then
                                            [ translate language Translate.PaleConjuctiva ]

                                        else
                                            []
                                in
                                pallorHands ++ paleConjuctiva
                            )
                        |> Maybe.withDefault []
            in
            referredToHospitalMessageWithComplications complication

        DiagnosisSevereAnemiaWithComplicationsRecurrentPhase ->
            viewTreatmentForDiagnosis language date measurements allDiagnoses DiagnosisSevereAnemiaWithComplicationsInitialPhase

        DiagnosisMalariaInitialPhase ->
            malariaTreatmentMessage

        DiagnosisMalariaRecurrentPhase ->
            malariaTreatmentMessage

        DiagnosisMalariaMedicatedContinuedInitialPhase ->
            referredToHospitalMessage

        DiagnosisMalariaMedicatedContinuedRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisMalariaWithAnemiaInitialPhase ->
            malariaTreatmentMessage

        DiagnosisMalariaWithAnemiaRecurrentPhase ->
            malariaTreatmentMessage

        DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ->
            referredToHospitalMessage

        DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisMalariaWithSevereAnemiaInitialPhase ->
            malariaTreatmentMessage

        DiagnosisMalariaWithSevereAnemiaRecurrentPhase ->
            malariaTreatmentMessage

        DiagnosisHepatitisBInitialPhase ->
            referredToHospitalMessage

        DiagnosisHepatitisBRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisNeurosyphilisInitialPhase ->
            referredToHospitalMessage

        DiagnosisNeurosyphilisRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisModeratePreeclampsiaInitialPhase ->
            if EverySet.member DiagnosisModeratePreeclampsiaInitialPhase allDiagnoses then
                referredToHospitalMessage

            else
                hypertensionTreatmentMessage

        DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisModeratePreeclampsiaRecurrentPhase ->
            if EverySet.member DiagnosisModeratePreeclampsiaRecurrentPhase allDiagnoses then
                referredToHospitalMessage

            else
                hypertensionTreatmentMessage

        DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaInitialPhase ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisHeartburn ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (\signs ->
                        if EverySet.member TreatmentAluminiumHydroxide signs then
                            diagnosisTreatedWithOnDateMessage TreatmentAluminiumHydroxide

                        else if EverySet.member TreatmentHealthEducationForHeartburn signs then
                            noTreatmentAdministeredMessage

                        else
                            noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisHeartburnPersistent ->
            referredToHospitalMessage

        DiagnosisDeepVeinThrombosis ->
            let
                location =
                    getMeasurementValueFunc measurements.symptomReview
                        |> Maybe.map
                            (\value ->
                                if EverySet.member SymptomQuestionLegPainRednessLeft value.symptomQuestions then
                                    " (" ++ String.toLower (translate language Translate.LegLeft) ++ ") "

                                else
                                    " (" ++ String.toLower (translate language Translate.LegRight) ++ ") "
                            )
                        |> Maybe.withDefault ""
            in
            referredToHospitalMessageWithComplications location

        DiagnosisPelvicPainIntense ->
            referredToHospitalMessage

        DiagnosisPelvicPainContinued ->
            referredToHospitalMessage

        DiagnosisUrinaryTractInfection ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (\signs ->
                        if EverySet.member TreatmentNitrofurantoin signs then
                            diagnosisTreatedWithOnDateMessage TreatmentNitrofurantoin

                        else if EverySet.member TreatmentAmoxicillin signs then
                            diagnosisTreatedWithOnDateMessage TreatmentAmoxicillin

                        else
                            noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisUrinaryTractInfectionContinued ->
            referredToHospitalMessage

        DiagnosisPyelonephritis ->
            referredToHospitalMessage

        DiagnosisCandidiasis ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (\signs ->
                        if EverySet.member TreatmentClotrimaxazole200 signs then
                            diagnosisTreatedWithOnDateMessage TreatmentClotrimaxazole200

                        else if EverySet.member TreatmentClotrimaxazole500 signs then
                            diagnosisTreatedWithOnDateMessage TreatmentClotrimaxazole500

                        else
                            noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisCandidiasisContinued ->
            referredToHospitalMessage

        DiagnosisGonorrhea ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        Maybe.map2
                            (\ceftriaxoneMessage azithromycinMessage ->
                                let
                                    diagnosisMessage =
                                        diagnosisForProgressReport
                                            ++ " "
                                            ++ (String.toLower <| translate language Translate.On)
                                            ++ " "
                                            ++ formatDDMMYYYY date
                                in
                                [ li []
                                    [ p [] [ text diagnosisMessage ]
                                    , p [] [ text ceftriaxoneMessage ]
                                    , p [] [ text azithromycinMessage ]
                                    ]
                                ]
                            )
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons Ceftriaxone)
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons Azithromycin)
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisGonorrheaContinued ->
            referredToHospitalMessage

        DiagnosisTrichomonasOrBacterialVaginosis ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            treatmentMessageForMedicationLower =
                                translate language Translate.TreatedWith
                                    |> String.toLower
                                    |> customTreatmentMessageForMedication

                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        treatmentMessageForMedicationLower value.distributionSigns nonAdministrationReasons Metronidazole
                            |> Maybe.map
                                (\message ->
                                    diagnosisForProgressReport
                                        ++ " - "
                                        ++ message
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                                )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisTrichomonasOrBacterialVaginosisContinued ->
            referredToHospitalMessage

        DiagnosisTuberculosis ->
            diagnosisForProgressReport
                ++ " - "
                ++ translate language Translate.TuberculosisInstructionsFollowed
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI

        DiagnosisDiabetesInitialPhase ->
            referredToHospitalMessage

        DiagnosisDiabetesRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisGestationalDiabetesInitialPhase ->
            referredToHospitalMessage

        DiagnosisGestationalDiabetesRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisRhesusNegativeInitialPhase ->
            referredToHospitalMessage

        DiagnosisRhesusNegativeRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisDepressionNotLikely ->
            mentalHealthMessage

        DiagnosisDepressionPossible ->
            mentalHealthMessage

        DiagnosisDepressionHighlyPossible ->
            mentalHealthMessage

        DiagnosisDepressionProbable ->
            mentalHealthMessage

        DiagnosisSuicideRisk ->
            mentalHealthMessage

        DiagnosisPostpartumAbdominalPain ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumHeadache ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumFatigue ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumFever ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumPerinealPainOrDischarge ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumUrinaryIncontinence ->
            referredToHospitalMessage

        DiagnosisPostpartumInfection ->
            referredToHospitalMessage

        DiagnosisPostpartumExcessiveBleeding ->
            referredToHospitalMessage

        -- Got same treatment options as DiagnosisPostpartumMastitis.
        DiagnosisPostpartumEarlyMastitisOrEngorgment ->
            mastitisMessage

        DiagnosisPostpartumMastitis ->
            mastitisMessage

        DiagnosisOther ->
            -- Other diagnosis is used only at outside care diagnostics.
            []

        NoPrenatalDiagnosis ->
            []


viewTreatmentForOutsideCareDiagnosis :
    Language
    -> NominalDate
    -> Maybe (EverySet OutsideCareMedication)
    -> PrenatalDiagnosis
    -> List (Html any)
viewTreatmentForOutsideCareDiagnosis language date medications diagnosis =
    let
        completePhrase maybeTreatedWithPhrase =
            let
                treatedWith =
                    Maybe.map (\phrase -> ", " ++ phrase)
                        maybeTreatedWithPhrase
                        |> Maybe.withDefault ""
            in
            diagnosisForProgressReportToString language diagnosis
                ++ " - "
                ++ (String.toLower <| translate language <| Translate.DiagnosedByOutsideCare)
                ++ treatedWith
                ++ ", "
                ++ (String.toLower <| translate language Translate.AddedToPatientRecordOn)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI
    in
    if List.member diagnosis outsideCareDiagnosesWithPossibleMedication then
        let
            treatmentForHypertensionMessage =
                treatedWithPhrase outsideCareMedicationOptionsHypertension NoOutsideCareMedicationForHypertension
                    |> Just
                    |> completePhrase

            treatedWithPhrase treartmentOptions noTreatmentOption =
                Maybe.map
                    (EverySet.toList
                        >> List.filter (\treatment -> List.member treatment treartmentOptions)
                        >> (\treatments ->
                                if List.isEmpty treatments || List.member noTreatmentOption treatments then
                                    noTreatmentAdministeredPhrase

                                else
                                    " "
                                        ++ (String.toLower <| translate language Translate.TreatedWith)
                                        ++ " "
                                        ++ (List.map
                                                (Translate.OutsideCareMedicationLabel >> translate language)
                                                treatments
                                                |> String.join ", "
                                           )
                           )
                    )
                    medications
                    |> Maybe.withDefault noTreatmentAdministeredPhrase

            noTreatmentAdministeredPhrase =
                " "
                    ++ (String.toLower <| translate language Translate.NoTreatmentAdministered)
                    ++ " "
        in
        case diagnosis of
            DiagnosisHIVInitialPhase ->
                treatedWithPhrase outsideCareMedicationOptionsHIV NoOutsideCareMedicationForMalaria
                    |> Just
                    |> completePhrase

            DiagnosisHIVRecurrentPhase ->
                viewTreatmentForOutsideCareDiagnosis language date medications DiagnosisHIVInitialPhase

            DiagnosisSyphilisInitialPhase ->
                treatedWithPhrase outsideCareMedicationOptionsSyphilis NoOutsideCareMedicationForSyphilis
                    |> Just
                    |> completePhrase

            DiagnosisSyphilisRecurrentPhase ->
                viewTreatmentForOutsideCareDiagnosis language date medications DiagnosisSyphilisInitialPhase

            DiagnosisMalariaInitialPhase ->
                treatedWithPhrase outsideCareMedicationOptionsMalaria NoOutsideCareMedicationForMalaria
                    |> Just
                    |> completePhrase

            DiagnosisMalariaRecurrentPhase ->
                viewTreatmentForOutsideCareDiagnosis language date medications DiagnosisMalariaInitialPhase

            DiagnosisModerateAnemiaInitialPhase ->
                treatedWithPhrase outsideCareMedicationOptionsAnemia NoOutsideCareMedicationForAnemia
                    |> Just
                    |> completePhrase

            DiagnosisModerateAnemiaRecurrentPhase ->
                viewTreatmentForOutsideCareDiagnosis language date medications DiagnosisModerateAnemiaInitialPhase

            DiagnosisGestationalHypertensionImmediate ->
                treatmentForHypertensionMessage

            DiagnosisChronicHypertensionImmediate ->
                treatmentForHypertensionMessage

            DiagnosisModeratePreeclampsiaInitialPhase ->
                treatmentForHypertensionMessage

            -- Will never get here.
            _ ->
                []

    else if List.member diagnosis outsideCareDiagnoses then
        completePhrase Nothing

    else
        -- Not an outside care diagnosis.
        []


viewTreatmentForPastDiagnosis : Language -> NominalDate -> PrenatalDiagnosis -> List (Html any)
viewTreatmentForPastDiagnosis language date diagnosis =
    diagnosisForProgressReportToString language diagnosis
        ++ " - "
        ++ (String.toLower <| translate language Translate.DiagnosedOn)
        ++ " "
        ++ formatDDMMYYYY date
        ++ " "
        ++ (String.toLower <| translate language Translate.PastDiagnosisReportReason)
        ++ "."
        |> wrapWithLI


generateAllNurseEncountersData : Bool -> AssembledData -> List PreviousEncounterData
generateAllNurseEncountersData isChw assembled =
    assembled.nursePreviousEncountersData
        ++ (if isChw then
                []

            else
                [ { startDate = assembled.encounter.startDate
                  , diagnoses = assembled.encounter.diagnoses
                  , pastDiagnoses = assembled.encounter.pastDiagnoses
                  , measurements = assembled.measurements
                  }
                ]
           )
