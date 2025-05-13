module Pages.WellChild.ProgressReport.View exposing
    ( view
    , viewNutritionSigns
    , viewPaneHeading
    , viewPersonInfoPane
    , viewProgressReport
    )

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication, nutritionAssessmentToComparable)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( generateIndividualChildScoreboardMeasurementsForChild
        , getNewbornExamPregnancySummary
        , getNutritionEncountersForParticipant
        , getWellChildEncountersForParticipant
        )
import Backend.PatientRecord.Model
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInMonths, getHealthCenterName, graduatingAgeInMonth)
import Backend.PrenatalEncounter.Utils exposing (eddToLmpDate)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Utils exposing (ncdaEnabled)
import Backend.WellChildEncounter.Model
    exposing
        ( EncounterWarning(..)
        , PediatricCareMilestone(..)
        , WellChildEncounter
        , ecdMilestoneWarnings
        , headCircumferenceWarnings
        , pediatricCareMilestones
        )
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Utils
import Components.ReportToWhatsAppDialog.View
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, diffMonths, diffWeeks, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (isNothing)
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils
    exposing
        ( generateFutureVaccinationsData
        , generateGroupNutritionAssessmentEntries
        , generateIndividualNutritionAssessmentEntries
        , generateVaccinationProgressDictByChildScoreboard
        , getPreviousMeasurements
        , resolveChildANCPregnancyData
        )
import Pages.AcuteIllness.Participant.Utils exposing (isAcuteIllnessActive)
import Pages.Nutrition.Activity.View exposing (translateNutritionAssement)
import Pages.Nutrition.Encounter.Utils
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Report.Model exposing (DiagnosisMode(..), PaneEntryStatus(..), ReportTab(..))
import Pages.Report.Utils
    exposing
        ( diagnosisEntryStatusToString
        , getAcuteIllnessEncountersForParticipant
        )
import Pages.Report.View exposing (viewAcuteIllnessDiagnosisEntry, viewEntries)
import Pages.Utils
    exposing
        ( viewConfirmationDialog
        , viewEncounterActionButton
        , viewEndEncounterButton
        , viewEndEncounterMenuForProgressReport
        , viewPersonDetailsExtended
        , viewStartEncounterButton
        )
import Pages.WellChild.Activity.Utils
    exposing
        ( expectedECDSignsOnMilestone
        , generateCompletedECDSigns
        , mandatoryNutritionAssessmentTasksCompleted
        )
import Pages.WellChild.Activity.View exposing (viewVaccinationOverview)
import Pages.WellChild.Encounter.Model exposing (AssembledData)
import Pages.WellChild.Encounter.Utils
    exposing
        ( allowEndingEncounter
        , generateAssembledData
        , pediatricCareMilestoneToComparable
        , resolveDateForPediatricCareMilestone
        , resolvePediatricCareMilestoneOnDate
        )
import Pages.WellChild.Encounter.View exposing (partitionActivities)
import Pages.WellChild.ProgressReport.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (TranslationId, translate, translateText)
import Translate.Model exposing (Language)
import Utils.Html exposing (viewModal)
import Utils.NominalDate
    exposing
        ( sortByDateDesc
        , sortDatesDesc
        , sortEncounterTuplesDesc
        , sortTuplesByDate
        , sortTuplesByDateDesc
        )
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Days, Kilograms(..), Length(..), Months(..))
import ZScore.Utils exposing (diffDays, zScoreLengthHeightForAge, zScoreWeightForAge)
import ZScore.View


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features id isChw db model =
    let
        encounter =
            Dict.get id db.wellChildEncounters
                |> Maybe.withDefault NotAsked

        participant =
            RemoteData.andThen
                (\encounter_ ->
                    Dict.get encounter_.participant db.individualParticipants
                        |> Maybe.withDefault NotAsked
                )
                encounter

        childData =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map (\child_ -> ( participant_.person, child_ ))
                    )

        assembledData =
            generateAssembledData site id db
                |> RemoteData.toMaybe

        ( bottomActionData, mandatoryNutritionAssessmentMeasurementsTaken ) =
            Maybe.map
                (\assembled ->
                    let
                        ( _, pendingActivities ) =
                            partitionActivities currentDate zscores site features isChw db assembled
                    in
                    ( Just <|
                        { showEndEncounterDialog = model.showEndEncounterDialog
                        , allowEndEncounter = allowEndingEncounter currentDate site pendingActivities assembled
                        , closeEncounterMsg = CloseEncounter id
                        , setEndEncounterDialogStateMsg = SetEndEncounterDialogState
                        , startEncounterMsg = NoOp
                        }
                    , mandatoryNutritionAssessmentTasksCompleted currentDate assembled
                    )
                )
                assembledData
                |> Maybe.withDefault ( Nothing, False )

        initiator =
            InitiatorWellChild id

        componentsConfig =
            Just { setReportComponentsMsg = SetReportComponents }
    in
    viewWebData language
        (viewProgressReport language
            currentDate
            zscores
            site
            features
            isChw
            initiator
            mandatoryNutritionAssessmentMeasurementsTaken
            db
            model.diagnosisMode
            model.reportToWhatsAppDialog
            model.activeTab
            SetActivePage
            SetActiveTab
            SetDiagnosisMode
            MsgReportToWhatsAppDialog
            componentsConfig
            model.components
            bottomActionData
        )
        identity
        childData


viewProgressReport :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> WellChildProgressReportInitiator
    -> Bool
    -> ModelIndexedDb
    -> DiagnosisMode
    -> Components.ReportToWhatsAppDialog.Model.Model
    -> ReportTab
    -> (Page -> msg)
    -> (ReportTab -> msg)
    -> (DiagnosisMode -> msg)
    -> (Components.ReportToWhatsAppDialog.Model.Msg msg -> msg)
    -> Maybe (Components.ReportToWhatsAppDialog.Model.ReportComponentsConfig msg)
    -> Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentWellChild)
    -> Maybe (BottomActionData msg)
    -> ( PersonId, Person )
    -> Html msg
viewProgressReport language currentDate zscores site features isChw initiator mandatoryNutritionAssessmentMeasurementsTaken db diagnosisMode reportToWhatsAppDialog activeTab setActivePageMsg setActiveTabMsg setDiagnosisModeMsg msgReportToWhatsAppDialogMsg componentsConfig selectedComponents bottomActionData ( childId, child ) =
    let
        content =
            case activeTab of
                TabSPVReport ->
                    viewContent language
                        currentDate
                        zscores
                        site
                        isChw
                        initiator
                        mandatoryNutritionAssessmentMeasurementsTaken
                        db
                        diagnosisMode
                        reportToWhatsAppDialog
                        setActivePageMsg
                        setDiagnosisModeMsg
                        msgReportToWhatsAppDialogMsg
                        componentsConfig
                        selectedComponents
                        ( childId, child )

                TabNCDAScoreboard ->
                    viewNCDAScorecard language currentDate zscores site ( childId, child ) db

        tabs =
            if ncdaEnabled features then
                viewTabs language setActiveTabMsg activeTab

            else
                emptyNode

        actions =
            if isNothing selectedComponents then
                viewActions language features initiator activeTab msgReportToWhatsAppDialogMsg bottomActionData

            else
                -- Actions are hidden when viewing for sharing via WhatsApp.
                []
    in
    div [ class "page-report well-child" ]
        [ viewHeader language initiator diagnosisMode setActivePageMsg setDiagnosisModeMsg
        , tabs
        , div
            [ class "ui report unstackable items"
            , Html.Attributes.id "report-content"
            ]
          <|
            content
                ++ actions
        ]


viewHeader : Language -> WellChildProgressReportInitiator -> DiagnosisMode -> (Page -> msg) -> (DiagnosisMode -> msg) -> Html msg
viewHeader language initiator diagnosisMode setActivePageMsg setDiagnosisModeMsg =
    let
        label =
            case initiator of
                Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord _ _ ->
                    Translate.PatientRecord

                _ ->
                    Translate.ProgressReport

        goBackAction =
            if diagnosisMode == ModeCompletedDiagnosis then
                setDiagnosisModeMsg ModeActiveDiagnosis

            else
                let
                    targetPage =
                        case initiator of
                            InitiatorNutritionIndividual nutritionEncounterId ->
                                UserPage (NutritionEncounterPage nutritionEncounterId)

                            InitiatorWellChild wellChildEncounterId ->
                                UserPage (WellChildEncounterPage wellChildEncounterId)

                            InitiatorNutritionGroup sessionId personId ->
                                UserPage (SessionPage sessionId (ChildPage personId))

                            Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord patientRecordInitiator _ ->
                                case patientRecordInitiator of
                                    Backend.PatientRecord.Model.InitiatorParticipantDirectory ->
                                        UserPage (PersonsPage Nothing ParticipantDirectoryOrigin)

                                    Backend.PatientRecord.Model.InitiatorPatientRecord personId ->
                                        UserPage (PatientRecordPage Backend.PatientRecord.Model.InitiatorParticipantDirectory personId)

                            Pages.WellChild.ProgressReport.Model.InitiatorChildScoreboard childScoreboardEncounterId ->
                                UserPage (ChildScoreboardEncounterPage childScoreboardEncounterId)
                in
                setActivePageMsg targetPage
    in
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ translateText language label ]
        , span
            [ class "link-back"
            , onClick goBackAction
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewTabs : Language -> (ReportTab -> msg) -> ReportTab -> Html msg
viewTabs language setActiveTabMsg activeTab =
    let
        renderButton tab =
            button
                [ classList
                    [ ( "active", tab == activeTab )
                    , ( "primary ui button", True )
                    ]
                , onClick <| setActiveTabMsg tab
                ]
                [ translateText language <| Translate.ReportTab tab ]
    in
    List.map renderButton [ TabSPVReport, TabNCDAScoreboard ]
        |> div [ class "ui segment tabs" ]


assembleProgresReportData site childId db =
    let
        individualParticipants =
            Dict.get childId db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        individualWellChildParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        lastWellChildEncounterId =
            Maybe.andThen
                (getWellChildEncountersForParticipant db
                    >> List.map Tuple.first
                    >> List.head
                )
                individualWellChildParticipantId

        maybeAssembled =
            Maybe.andThen
                (\id ->
                    generateAssembledData site id db
                        |> RemoteData.toMaybe
                )
                lastWellChildEncounterId

        expectedSessions =
            Dict.get childId db.expectedSessions
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.withDefault Dict.empty

        individualNutritionParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        individualChildScoreboardParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first
    in
    { maybeAssembled = maybeAssembled
    , expectedSessions = expectedSessions
    , acuteIllnesses =
        List.filter
            (\( _, participant ) ->
                participant.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            )
            individualParticipants
    , groupNutritionMeasurements =
        Dict.get childId db.childMeasurements
            |> Maybe.andThen RemoteData.toMaybe
            |> Maybe.withDefault emptyChildMeasurementList
    , individualNutritionParticipantId = individualNutritionParticipantId
    , individualNutritionMeasurementsWithDates =
        Maybe.map
            (\participantId ->
                Pages.Nutrition.Encounter.Utils.generatePreviousMeasurements Nothing participantId db
            )
            individualNutritionParticipantId
            |> Maybe.withDefault []
    , wellChildEncounters =
        Maybe.map
            (\participantId ->
                getWellChildEncountersForParticipant db participantId
            )
            individualWellChildParticipantId
            |> Maybe.withDefault []
    , individualWellChildMeasurementsWithDates =
        Maybe.map
            (\assembled ->
                ( assembled.encounter.startDate, ( assembled.id, assembled.measurements ) )
                    :: assembled.previousMeasurementsWithDates
            )
            maybeAssembled
            |> Maybe.withDefault []
    , individualChildScoreboardMeasurementsWithDates =
        generateIndividualChildScoreboardMeasurementsForChild childId db
    , individualChildScoreboardParticipantId = individualChildScoreboardParticipantId
    }


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> Bool
    -> WellChildProgressReportInitiator
    -> Bool
    -> ModelIndexedDb
    -> DiagnosisMode
    -> Components.ReportToWhatsAppDialog.Model.Model
    -> (Page -> msg)
    -> (DiagnosisMode -> msg)
    -> (Components.ReportToWhatsAppDialog.Model.Msg msg -> msg)
    -> Maybe (Components.ReportToWhatsAppDialog.Model.ReportComponentsConfig msg)
    -> Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentWellChild)
    -> ( PersonId, Person )
    -> List (Html msg)
viewContent language currentDate zscores site isChw initiator mandatoryNutritionAssessmentMeasurementsTaken db diagnosisMode reportToWhatsAppDialog setActivePageMsg setDiagnosisModeMsg msgReportToWhatsAppDialogMsg componentsConfig selectedComponents ( childId, child ) =
    let
        reportData =
            assembleProgresReportData site childId db

        individualWellChildMeasurements =
            getPreviousMeasurements reportData.individualWellChildMeasurementsWithDates

        individualNutritionMeasurements =
            getPreviousMeasurements reportData.individualNutritionMeasurementsWithDates

        derivedContent =
            case diagnosisMode of
                ModeActiveDiagnosis ->
                    let
                        -- Drawing SVG charts causes major slowness, specially when
                        -- typing new phone number. Therefore, we do not show it when
                        -- 'Send via WhatsApp' dialog is open, until its final
                        -- confirmation steps.
                        vaccinationProgress =
                            Maybe.map .vaccinationProgress reportData.maybeAssembled
                                |> Maybe.withDefault Dict.empty

                        showGrowthPaneByWhatsAppDialog =
                            Maybe.map
                                (\state ->
                                    case state of
                                        Components.ReportToWhatsAppDialog.Model.ConfirmationBeforeExecuting _ ->
                                            True

                                        Components.ReportToWhatsAppDialog.Model.ExecutionResult _ ->
                                            True

                                        _ ->
                                            False
                                )
                                reportToWhatsAppDialog.state
                                |> Maybe.withDefault True

                        growthPane =
                            if showGrowthPaneByWhatsAppDialog then
                                viewGrowthPane language
                                    currentDate
                                    zscores
                                    child
                                    reportData.groupNutritionMeasurements
                                    reportData.individualNutritionMeasurementsWithDates
                                    reportData.individualWellChildMeasurementsWithDates
                                    |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentWellChildGrowth)

                            else
                                emptyNode
                    in
                    [ viewVaccinationHistoryPane language
                        currentDate
                        site
                        child
                        vaccinationProgress
                        db
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentWellChildImmunizationHistory)
                    , viewECDPane language
                        currentDate
                        child
                        reportData.wellChildEncounters
                        reportData.individualWellChildMeasurementsWithDates
                        db
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentWellChildECD)
                    , growthPane
                    , viewNextAppointmentPane language
                        currentDate
                        child
                        individualWellChildMeasurements
                        db
                        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentWellChildNextAppointment)
                    ]

                ModeCompletedDiagnosis ->
                    []

        showDiagnosisPaneForReportToWhatsApp =
            Maybe.map (EverySet.member Components.ReportToWhatsAppDialog.Model.ComponentWellChildActiveDiagnoses) selectedComponents
                |> Maybe.withDefault False

        showComponent =
            Components.ReportToWhatsAppDialog.Utils.showComponent selectedComponents
    in
    [ viewPersonInfoPane language currentDate child
    , viewDiagnosisPane language
        currentDate
        isChw
        initiator
        mandatoryNutritionAssessmentMeasurementsTaken
        reportData.acuteIllnesses
        reportData.individualNutritionParticipantId
        reportData.wellChildEncounters
        reportData.groupNutritionMeasurements
        individualNutritionMeasurements
        individualWellChildMeasurements
        db
        diagnosisMode
        setActivePageMsg
        setDiagnosisModeMsg
        showDiagnosisPaneForReportToWhatsApp
        reportData.maybeAssembled
        |> showIf (showComponent Components.ReportToWhatsAppDialog.Model.ComponentWellChildActiveDiagnoses)
    ]
        ++ derivedContent
        ++ [ Html.map msgReportToWhatsAppDialogMsg
                (Components.ReportToWhatsAppDialog.View.view
                    language
                    currentDate
                    site
                    ( childId, child )
                    Components.ReportToWhatsAppDialog.Model.ReportWellChild
                    componentsConfig
                    reportToWhatsAppDialog
                )
           ]


viewActions :
    Language
    -> EverySet SiteFeature
    -> WellChildProgressReportInitiator
    -> ReportTab
    -> (Components.ReportToWhatsAppDialog.Model.Msg msg -> msg)
    -> Maybe (BottomActionData msg)
    -> List (Html msg)
viewActions language features initiator activeTab msgReportToWhatsAppDialogMsg bottomActionData =
    Maybe.map
        (\data ->
            let
                bottomActionButton =
                    case initiator of
                        Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord _ _ ->
                            viewStartEncounterButton language data.startEncounterMsg

                        Pages.WellChild.ProgressReport.Model.InitiatorNutritionGroup _ _ ->
                            case activeTab of
                                TabSPVReport ->
                                    viewEncounterActionButton language
                                        Translate.ReportToWhatsApp
                                        "velvet"
                                        True
                                        (msgReportToWhatsAppDialogMsg <|
                                            Components.ReportToWhatsAppDialog.Model.SetState <|
                                                Just Components.ReportToWhatsAppDialog.Model.Consent
                                        )

                                TabNCDAScoreboard ->
                                    emptyNode

                        _ ->
                            case activeTab of
                                TabSPVReport ->
                                    viewEndEncounterMenuForProgressReport language
                                        features
                                        data.allowEndEncounter
                                        data.setEndEncounterDialogStateMsg
                                        (msgReportToWhatsAppDialogMsg <|
                                            Components.ReportToWhatsAppDialog.Model.SetState <|
                                                Just Components.ReportToWhatsAppDialog.Model.Consent
                                        )

                                TabNCDAScoreboard ->
                                    viewEndEncounterButton language data.allowEndEncounter (data.setEndEncounterDialogStateMsg True)

                endEncounterDialog =
                    if data.showEndEncounterDialog then
                        Just <|
                            viewConfirmationDialog language
                                Translate.EndEncounterQuestion
                                Translate.OnceYouEndTheEncounter
                                data.closeEncounterMsg
                                (data.setEndEncounterDialogStateMsg False)

                    else
                        Nothing
            in
            [ bottomActionButton
            , viewModal endEncounterDialog
            ]
        )
        bottomActionData
        |> Maybe.withDefault []


viewPersonInfoPane : Language -> NominalDate -> Person -> Html any
viewPersonInfoPane language currentDate person =
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div [ class "patient-info" ] <|
            viewPersonDetailsExtended language currentDate person
        ]


viewDiagnosisPane :
    Language
    -> NominalDate
    -> Bool
    -> WellChildProgressReportInitiator
    -> Bool
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Maybe IndividualEncounterParticipantId
    -> List ( WellChildEncounterId, WellChildEncounter )
    -> ChildMeasurementList
    -> List NutritionMeasurements
    -> List WellChildMeasurements
    -> ModelIndexedDb
    -> DiagnosisMode
    -> (Page -> msg)
    -> (DiagnosisMode -> msg)
    -> Bool
    -> Maybe AssembledData
    -> Html msg
viewDiagnosisPane language currentDate isChw initiator mandatoryNutritionAssessmentMeasurementsTaken acuteIllnesses individualNutritionParticipantId wellChildEncounters groupNutritionMeasurements individualNutritionMeasurements individualWellChildMeasurements db diagnosisMode setActivePageMsg setDiagnosisModeMsg viewForReportToWhatsApp maybeAssembled =
    let
        ( activeIllnesses, completedIllnesses ) =
            List.partition (Tuple.second >> isAcuteIllnessActive currentDate) acuteIllnesses

        groupNutritionEntries =
            generateGroupNutritionAssessmentEntries groupNutritionMeasurements

        individualNutritionEntries =
            generateIndividualNutritionAssessmentEntries individualNutritionMeasurements

        individuaWellChildEntries =
            generateIndividualNutritionAssessmentEntries individualWellChildMeasurements

        allNutritionAssessmentEntries =
            individualNutritionEntries ++ groupNutritionEntries ++ individuaWellChildEntries

        ( activeWarningEntries, completedWarningEntries ) =
            generatePartitionedWarningEntries db maybeAssembled

        ( activeAssessmentEntries, completedAssessmentEntries ) =
            resolveDateOfLastNutritionAssessment
                currentDate
                initiator
                mandatoryNutritionAssessmentMeasurementsTaken
                individualNutritionParticipantId
                wellChildEncounters
                groupNutritionMeasurements
                db
                |> Maybe.map
                    (\lastNutritionAssessmentDate ->
                        List.partition
                            (\( date, _ ) ->
                                Date.compare date lastNutritionAssessmentDate == EQ
                            )
                            allNutritionAssessmentEntries
                    )
                |> Maybe.withDefault ( allNutritionAssessmentEntries, [] )

        entriesHeading =
            div [ class "heading diagnosis" ]
                [ div [ class "assesment" ] [ text <| translate language Translate.Assessment ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                , div [ class "date" ] [ text <| translate language Translate.DiagnosisDate ]
                , div [ class "see-more" ] [ text <| translate language Translate.SeeMore ]
                ]

        ( label, priorDiagniosisButton ) =
            if viewForReportToWhatsApp || diagnosisMode == ModeActiveDiagnosis then
                ( Translate.ActiveDiagnosis
                , div [ class "pane-action" ]
                    [ button
                        [ class "ui primary button"
                        , onClick <| setDiagnosisModeMsg ModeCompletedDiagnosis
                        ]
                        [ text <| translate language Translate.ReviewPriorDiagnosis ]
                    ]
                )

            else
                ( Translate.PriorDiagnosis
                , emptyNode
                )

        ( selectedDiagnosisEntries, selectedAssessmentEntries, selectedWarningEntries ) =
            if viewForReportToWhatsApp || diagnosisMode == ModeActiveDiagnosis then
                ( List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activeIllnesses
                , List.map (\( date, data ) -> ( date, ( data, StatusOngoing ) )) activeAssessmentEntries
                , List.map (\( date, milestone, data ) -> ( date, ( milestone, data, StatusOngoing ) )) activeWarningEntries
                )

            else
                ( List.map (\( participantId, data ) -> ( ( participantId, StatusResolved ), data )) completedIllnesses
                , List.map (\( date, data ) -> ( date, ( data, StatusResolved ) )) completedAssessmentEntries
                , List.map (\( date, milestone, data ) -> ( date, ( milestone, data, StatusResolved ) )) completedWarningEntries
                )

        entries =
            (daignosisEntries ++ assessmentEntries ++ warningEntries)
                |> List.sortWith sortTuplesByDateDesc
                |> List.map Tuple.second

        daignosisEntries =
            List.map
                (\( data, _ ) ->
                    let
                        acuteIllnessProgressReportInitiator =
                            case initiator of
                                InitiatorNutritionIndividual nutritionEncounterId ->
                                    InitiatorIndividualNutritionProgressReport nutritionEncounterId

                                InitiatorWellChild wellChildEncounterId ->
                                    InitiatorWellChildProgressReport wellChildEncounterId

                                InitiatorNutritionGroup sessionId personId ->
                                    InitiatorGroupNutritionProgressReport sessionId personId

                                Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord patientRecordInitiator personId ->
                                    Backend.AcuteIllnessEncounter.Types.InitiatorPatientRecord patientRecordInitiator personId

                                InitiatorChildScoreboard childScoreboardEncounterId ->
                                    InitiatorChildScoreboardProgressReport childScoreboardEncounterId
                    in
                    viewAcuteIllnessDiagnosisEntry language acuteIllnessProgressReportInitiator db setActivePageMsg data
                )
                selectedDiagnosisEntries
                |> Maybe.Extra.values

        assessmentEntries =
            List.map (viewNutritionAssessmentEntry language) selectedAssessmentEntries

        warningEntries =
            List.map (viewWarningEntry language) selectedWarningEntries
    in
    div [ class "pane diagnosis" ]
        [ viewPaneHeading language label
        , div [ class "pane-content" ] <|
            entriesHeading
                :: viewEntries language entries
        , priorDiagniosisButton |> showIf (not viewForReportToWhatsApp)
        ]


resolveDateOfLastNutritionAssessment :
    NominalDate
    -> WellChildProgressReportInitiator
    -> Bool
    -> Maybe IndividualEncounterParticipantId
    -> List ( WellChildEncounterId, WellChildEncounter )
    -> ChildMeasurementList
    -> ModelIndexedDb
    -> Maybe NominalDate
resolveDateOfLastNutritionAssessment currentDate initiator mandatoryNutritionAssessmentMeasurementsTaken individualNutritionParticipantId wellChildEncounters groupNutritionMeasurements db =
    if mandatoryNutritionAssessmentMeasurementsTaken then
        Just currentDate

    else
        let
            ( individualNutritionFilter, individualWellChildFilter, groupNutritionFilter ) =
                case initiator of
                    InitiatorNutritionIndividual _ ->
                        ( Tuple.second >> .startDate >> (/=) currentDate
                        , always True
                        , always True
                        )

                    InitiatorWellChild _ ->
                        ( always True
                        , Tuple.second >> .startDate >> (/=) currentDate
                        , always True
                        )

                    InitiatorNutritionGroup _ _ ->
                        ( always True
                        , always True
                        , .dateMeasured >> (/=) currentDate
                        )

                    Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord _ _ ->
                        ( always True
                        , always True
                        , always True
                        )

                    InitiatorChildScoreboard _ ->
                        ( always True
                        , always True
                        , always True
                        )

            lastAssessmentDatePerIndividualNutrition =
                Maybe.andThen
                    (\participantId ->
                        getNutritionEncountersForParticipant db participantId
                            |> List.filter individualNutritionFilter
                            -- Sort DESC
                            |> List.sortWith sortEncounterTuplesDesc
                            |> List.head
                            |> Maybe.map (Tuple.second >> .startDate)
                    )
                    individualNutritionParticipantId

            lastAssessmentDatePerWellChild =
                List.filter individualWellChildFilter wellChildEncounters
                    -- Sort DESC
                    |> List.sortWith (sortByDateDesc (Tuple.second >> .startDate))
                    |> List.head
                    |> Maybe.map (Tuple.second >> .startDate)

            lastAssessmentDatePerGroupNutrition =
                Dict.values groupNutritionMeasurements.nutritions
                    |> List.filter groupNutritionFilter
                    |> List.map .dateMeasured
                    |> List.sortWith sortDatesDesc
                    |> List.head
        in
        [ lastAssessmentDatePerIndividualNutrition, lastAssessmentDatePerGroupNutrition, lastAssessmentDatePerWellChild ]
            |> Maybe.Extra.values
            |> List.sortWith sortDatesDesc
            |> List.head


generatePartitionedWarningEntries :
    ModelIndexedDb
    -> Maybe AssembledData
    -> ( List ( NominalDate, PediatricCareMilestone, EncounterWarning ), List ( NominalDate, PediatricCareMilestone, EncounterWarning ) )
generatePartitionedWarningEntries db maybeAssembled =
    Maybe.map
        (\assembled ->
            let
                wellChildEncounters =
                    getWellChildEncountersForParticipant db assembled.encounter.participant
                        -- Sort DESC
                        |> List.sortWith sortEncounterTuplesDesc

                allWarnings =
                    List.filterMap
                        (\( _, encounter ) ->
                            let
                                warnings =
                                    EverySet.toList encounter.encounterWarnings
                                        |> List.filterMap
                                            (\warning ->
                                                if List.member warning [ NoECDMilstoneWarning, NoHeadCircumferenceWarning, NoEncounterWarnings ] then
                                                    Nothing

                                                else
                                                    let
                                                        ecdMilestone =
                                                            if List.member warning ecdMilestoneWarnings then
                                                                Maybe.andThen (resolvePediatricCareMilestoneOnDate encounter.startDate) assembled.person.birthDate

                                                            else
                                                                -- Giving dummy value here, because ecd milestone is
                                                                -- not applicable for Head Circumference warnings.
                                                                Just Milestone4Years
                                                    in
                                                    Maybe.map
                                                        (\milestone ->
                                                            ( encounter.startDate, milestone, warning )
                                                        )
                                                        ecdMilestone
                                            )
                            in
                            if List.isEmpty warnings then
                                Nothing

                            else
                                Just warnings
                        )
                        wellChildEncounters
                        |> List.concat

                lastECDActivityDate =
                    dateOfLastEncounterWithWarningFrom ecdMilestoneWarnings

                lastHeadCircumferenceActivityDate =
                    dateOfLastEncounterWithWarningFrom headCircumferenceWarnings

                dateOfLastEncounterWithWarningFrom warningsSet =
                    List.filterMap
                        (\( _, encounter ) ->
                            if List.any (\warning -> EverySet.member warning encounter.encounterWarnings) warningsSet then
                                Just encounter.startDate

                            else
                                Nothing
                        )
                        wellChildEncounters
                        |> List.head
            in
            List.partition
                (\( date, _, warning ) ->
                    if List.member warning ecdMilestoneWarnings then
                        Maybe.map (\lastAtivityDate -> Date.compare date lastAtivityDate == EQ) lastECDActivityDate
                            |> Maybe.withDefault True

                    else
                        Maybe.map (\lastAtivityDate -> Date.compare date lastAtivityDate == EQ) lastHeadCircumferenceActivityDate
                            |> Maybe.withDefault True
                )
                allWarnings
        )
        maybeAssembled
        |> Maybe.withDefault ( [], [] )


viewNutritionAssessmentEntry : Language -> ( NominalDate, ( List NutritionAssessment, PaneEntryStatus ) ) -> ( NominalDate, Html any )
viewNutritionAssessmentEntry language ( date, ( assessments, status ) ) =
    let
        orderedAssessments =
            List.sortBy nutritionAssessmentToComparable assessments
    in
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] <|
            List.map (translateNutritionAssement language >> List.singleton >> p []) orderedAssessments
        , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
            [ text <| translate language <| Translate.EntryStatusDiagnosis status ]
        , div [ class "cell date" ] [ text <| formatDDMMYYYY date ]
        ]
    )


viewWarningEntry : Language -> ( NominalDate, ( PediatricCareMilestone, EncounterWarning, PaneEntryStatus ) ) -> ( NominalDate, Html any )
viewWarningEntry language ( date, ( milestone, warning, status ) ) =
    let
        milestoneForDaignosisPane =
            translate language <| Translate.WellChildECDMilestoneForDiagnosisPane milestone
    in
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ text <| translate language <| Translate.EncounterWarningForDiagnosisPane warning milestoneForDaignosisPane ]
        , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
            [ text <| translate language <| Translate.EntryStatusDiagnosis status ]
        , div [ class "cell date" ] [ text <| formatDDMMYYYY date ]
        ]
    )


viewVaccinationHistoryPane : Language -> NominalDate -> Site -> Person -> VaccinationProgressDict -> ModelIndexedDb -> Html any
viewVaccinationHistoryPane language currentDate site child vaccinationProgress db =
    div [ class "pane vaccination-history" ] <|
        [ viewPaneHeading language Translate.ImmunizationHistory
        , div [ class "pane-content" ] <|
            viewVaccinationOverview language currentDate site child vaccinationProgress db
        ]


viewECDPane :
    Language
    -> NominalDate
    -> Person
    -> List ( WellChildEncounterId, WellChildEncounter )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> ModelIndexedDb
    -> Html any
viewECDPane language currentDate child wellChildEncounters individualWellChildMeasurementsWithDates db =
    Maybe.map
        (\_ ->
            let
                milestonesToCurrentDateWithStatus =
                    generateECDMilestonesWithStatus currentDate child wellChildEncounters individualWellChildMeasurementsWithDates

                viewMilestone ( milestone, status ) =
                    let
                        statusClass =
                            case status of
                                StatusOnTrack ->
                                    "on-track"

                                StatusECDBehind ->
                                    "ecd-behind"

                                StatusOffTrack ->
                                    "off-track"

                                NoECDStatus ->
                                    "no-status"
                    in
                    div [ class "milestone" ]
                        [ div [ class <| "status " ++ statusClass ]
                            [ text <| translate language <| Translate.ECDStatus status ]
                        , div [ class "description" ]
                            [ text <| translate language <| Translate.PediatricCareMilestone milestone ]
                        ]

                entries =
                    List.map viewMilestone milestonesToCurrentDateWithStatus
            in
            div [ class "pane ecd" ] <|
                [ viewPaneHeading language Translate.EarlyChildhoodDevelopment
                , div [ class "pane-content overflow" ]
                    [ div [ class "ecd-milestones" ] <|
                        viewEntries language entries
                    ]
                ]
        )
        child.birthDate
        |> Maybe.withDefault emptyNode


generateECDMilestonesWithStatus :
    NominalDate
    -> Person
    -> List ( WellChildEncounterId, WellChildEncounter )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> List ( PediatricCareMilestone, ECDStatus )
generateECDMilestonesWithStatus currentDate child wellChildEncounters individualWellChildMeasurementsWithDates =
    Maybe.map
        (\birthDate ->
            let
                milestoneForCurrentDateAsComparable =
                    resolvePediatricCareMilestoneOnDate currentDate birthDate
                        |> Maybe.map pediatricCareMilestoneToComparable

                milestonesToCurrentDate =
                    Maybe.map
                        (\currentMilestoneAsComparable ->
                            List.filter
                                (\milestone ->
                                    pediatricCareMilestoneToComparable milestone <= currentMilestoneAsComparable
                                )
                                pediatricCareMilestones
                        )
                        milestoneForCurrentDateAsComparable
                        |> Maybe.withDefault []

                performedMilestonesWithStatus =
                    List.filterMap
                        (\( _, encounter ) ->
                            let
                                milestoneStatus =
                                    if EverySet.member WarningECDMilestoneReferToSpecialist encounter.encounterWarnings then
                                        Just StatusOffTrack

                                    else if EverySet.member WarningECDMilestoneBehind encounter.encounterWarnings then
                                        Just StatusECDBehind

                                    else if EverySet.member NoECDMilstoneWarning encounter.encounterWarnings then
                                        Just StatusOnTrack

                                    else
                                        Nothing
                            in
                            Maybe.map2
                                (\milestone status -> ( milestone, status ))
                                (resolvePediatricCareMilestoneOnDate encounter.startDate birthDate)
                                milestoneStatus
                        )
                        wellChildEncounters
                        |> Dict.fromList
            in
            List.map
                (\milestone ->
                    let
                        status =
                            Dict.get milestone performedMilestonesWithStatus
                                |> Maybe.withDefault (genrateDefaultECDStatus birthDate milestone individualWellChildMeasurementsWithDates)
                    in
                    ( milestone, status )
                )
                milestonesToCurrentDate
        )
        child.birthDate
        |> Maybe.withDefault []


genrateDefaultECDStatus :
    NominalDate
    -> PediatricCareMilestone
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> ECDStatus
genrateDefaultECDStatus birthDate milestone individualWellChildMeasurementsWithDates =
    let
        milestoneDate =
            resolveDateForPediatricCareMilestone birthDate milestone

        firstEncounterDateAfterMilestone =
            List.filterMap
                (\( date, _ ) ->
                    if not <| Date.compare milestoneDate date == LT then
                        Just date

                    else
                        Nothing
                )
                individualWellChildMeasurementsWithDates
                |> List.sortWith Date.compare
                |> List.head

        -- Take all measurements that were taken before the milestone,
        -- and, these of first encounter after milestone.
        measurementsForPeriod =
            Maybe.map
                (\firstEncounterAfterMilestoneDate ->
                    List.filterMap
                        (\( date, ( _, measurements ) ) ->
                            if Date.compare date firstEncounterAfterMilestoneDate == GT then
                                Nothing

                            else
                                Just measurements
                        )
                        individualWellChildMeasurementsWithDates
                )
                firstEncounterDateAfterMilestone
                |> Maybe.withDefault
                    -- There were no encounters after milestone date, so we just
                    -- take all existing measurements.
                    (List.map (Tuple.second >> Tuple.second) individualWellChildMeasurementsWithDates)

        expectedSigns =
            expectedECDSignsOnMilestone birthDate milestoneDate firstEncounterDateAfterMilestone

        completedSigns =
            generateCompletedECDSigns measurementsForPeriod
    in
    if List.all (\sign -> List.member sign completedSigns) expectedSigns then
        StatusOnTrack

    else
        StatusOffTrack


viewGrowthPane :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> ChildMeasurementList
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> Html any
viewGrowthPane language currentDate zscores child historical nutritionMeasurements wellChildMeasurements =
    let
        --
        -- GROUP CONTEXT
        --
        -- Do we have any kind of measurement for the child for the specified session?
        -- This includes any edits that have been saved locally, but not as-you-type
        -- in the UI before you hit "Save" or "Update".
        valuesIndexedBySession func =
            Dict.values (func historical)
                |> List.filterMap
                    (\measurement ->
                        measurement.encounterId
                            |> Maybe.map
                                (\encounterId ->
                                    ( fromEntityUuid encounterId
                                    , { dateMeasured = measurement.dateMeasured
                                      , encounterId = fromEntityUuid encounterId
                                      , value = measurement.value
                                      }
                                    )
                                )
                    )
                |> Dict.fromList

        heightValuesBySession =
            valuesIndexedBySession .heights

        weightValuesBySession =
            valuesIndexedBySession .weights

        photoValuesBySession =
            valuesIndexedBySession .photos

        nutritionValuesBySession =
            valuesIndexedBySession .nutritions

        --
        -- INDIVIDUAL CONTEXT
        --
        -- Do we have any kind of measurement for the child for the specified encounter?
        valuesIndexedByEncounter func =
            List.filterMap
                (\( _, ( _, measurements ) ) ->
                    func measurements
                        |> Maybe.andThen
                            (Tuple.second
                                >> (\measurement ->
                                        measurement.encounterId
                                            |> Maybe.map
                                                (\encounterId ->
                                                    ( fromEntityUuid encounterId
                                                    , { dateMeasured = measurement.dateMeasured
                                                      , encounterId = fromEntityUuid encounterId
                                                      , value = measurement.value
                                                      }
                                                    )
                                                )
                                   )
                            )
                )
                >> Dict.fromList

        heightValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .height nutritionMeasurements)
                (valuesIndexedByEncounter .height wellChildMeasurements)

        weightValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .weight nutritionMeasurements)
                (valuesIndexedByEncounter .weight wellChildMeasurements)

        photoValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .photo nutritionMeasurements)
                (valuesIndexedByEncounter .photo wellChildMeasurements)

        nutritionValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .nutrition nutritionMeasurements)
                (valuesIndexedByEncounter .nutrition wellChildMeasurements)

        --
        -- COMMON CONTEXT
        --
        heightValues =
            Dict.values heightValuesBySession ++ Dict.values heightValuesByEncounter

        weightValues =
            Dict.values weightValuesBySession ++ Dict.values weightValuesByEncounter

        nutritionValues =
            Dict.values nutritionValuesBySession
                ++ Dict.values nutritionValuesByEncounter
                |> List.map (\measurement -> ( measurement.dateMeasured, measurement.value.signs ))

        photoValues =
            Dict.values photoValuesBySession ++ Dict.values photoValuesByEncounter

        heightForAgeData =
            List.filterMap (chartHeightForAge child) heightValues

        heightForAgeDaysData =
            heightForAgeData
                |> List.map (\( days, _, height ) -> ( days, height ))

        heightForAgeMonthsData =
            heightForAgeData
                |> List.map (\( _, month, height ) -> ( month, height ))

        weightForAgeData =
            List.filterMap (chartWeightForAge child) weightValues

        weightForAgeDaysData =
            weightForAgeData
                |> List.map (\( days, _, weight ) -> ( days, weight ))

        weightForAgeMonthsData =
            weightForAgeData
                |> List.map (\( _, month, weight ) -> ( month, weight ))

        weightForLengthAndHeightData =
            List.filterMap (chartWeightForLengthAndHeight heightValues) weightValues

        weightForLengthData =
            weightForLengthAndHeightData
                |> List.map (\( length, _, weight ) -> ( length, weight ))

        weightForHeightData =
            weightForLengthAndHeightData
                |> List.map (\( _, height, weight ) -> ( height, weight ))

        charts =
            Maybe.map
                (\birthDate ->
                    let
                        headCircumferenceValuesByEncounter =
                            valuesIndexedByEncounter .headCircumference wellChildMeasurements

                        headCircumferenceValues =
                            Dict.values headCircumferenceValuesByEncounter

                        zScoreViewCharts =
                            case child.gender of
                                Male ->
                                    { heightForAge = ZScore.View.viewHeightForAgeBoys
                                    , heightForAge0To5 = ZScore.View.viewHeightForAgeBoys0To5
                                    , heightForAge5To19 = ZScore.View.viewHeightForAgeBoys5To19
                                    , weightForAge = ZScore.View.viewWeightForAgeBoys
                                    , weightForAge0To5 = ZScore.View.viewWeightForAgeBoys0To5
                                    , weightForAge5To10 = ZScore.View.viewWeightForAgeBoys5To10
                                    , weightForHeight = ZScore.View.viewWeightForHeightBoys
                                    , weightForHeight0To5 = ZScore.View.viewWeightForHeight0To5Boys
                                    , viewHeadCircumferenceForAge0To13Weeks = ZScore.View.viewHeadCircumferenceForAge0To13WeeksBoys
                                    , headCircumferenceForAge0To2 = ZScore.View.viewHeadCircumferenceForAge0To2Boys
                                    , headCircumferenceForAge0To5 = ZScore.View.viewHeadCircumferenceForAge0To5Boys
                                    }

                                Female ->
                                    { heightForAge = ZScore.View.viewHeightForAgeGirls
                                    , heightForAge0To5 = ZScore.View.viewHeightForAgeGirls0To5
                                    , heightForAge5To19 = ZScore.View.viewHeightForAgeGirls5To19
                                    , weightForAge = ZScore.View.viewWeightForAgeGirls
                                    , weightForAge0To5 = ZScore.View.viewWeightForAgeGirls0To5
                                    , weightForAge5To10 = ZScore.View.viewWeightForAgeGirls5To10
                                    , weightForHeight = ZScore.View.viewWeightForHeightGirls
                                    , weightForHeight0To5 = ZScore.View.viewWeightForHeight0To5Girls
                                    , viewHeadCircumferenceForAge0To13Weeks = ZScore.View.viewHeadCircumferenceForAge0To13WeeksGirls
                                    , headCircumferenceForAge0To2 = ZScore.View.viewHeadCircumferenceForAge0To2Girls
                                    , headCircumferenceForAge0To5 = ZScore.View.viewHeadCircumferenceForAge0To5Girls
                                    }

                        headCircumferenceForAgeData =
                            List.filterMap (chartHeadCircumferenceForAge child) headCircumferenceValues

                        childAgeInMonths =
                            diffMonths birthDate currentDate
                    in
                    -- With exception of Sortwathe, children graduate from all
                    -- groups at the age of 26 month. Therefore, we will show
                    -- 0-2 chart for all children that are less than 26 month old.
                    -- For head circumference, we'll show 0 - 13 weeks chart for
                    -- childern with age below 13 weeks.
                    if childAgeInMonths < graduatingAgeInMonth then
                        let
                            childAgeInWeeks =
                                diffWeeks birthDate currentDate

                            headCircumferenceChart =
                                if childAgeInWeeks < 13 then
                                    zScoreViewCharts.viewHeadCircumferenceForAge0To13Weeks language zscores headCircumferenceForAgeData

                                else
                                    zScoreViewCharts.headCircumferenceForAge0To2 language zscores headCircumferenceForAgeData
                        in
                        [ ZScore.View.viewMarkers
                        , zScoreViewCharts.heightForAge language zscores heightForAgeDaysData
                        , zScoreViewCharts.weightForAge language zscores weightForAgeDaysData
                        , zScoreViewCharts.weightForHeight language zscores weightForLengthData
                        , headCircumferenceChart
                        ]

                    else if childAgeInMonths < 60 then
                        [ ZScore.View.viewMarkers
                        , zScoreViewCharts.heightForAge0To5 language zscores heightForAgeDaysData
                        , zScoreViewCharts.weightForAge0To5 language zscores weightForAgeDaysData
                        , zScoreViewCharts.weightForHeight0To5 language zscores weightForHeightData
                        , zScoreViewCharts.headCircumferenceForAge0To5 language zscores headCircumferenceForAgeData
                        ]

                    else
                        -- Child is older than 5 years.
                        [ ZScore.View.viewMarkers
                        , zScoreViewCharts.heightForAge5To19 language zscores heightForAgeMonthsData
                        , zScoreViewCharts.weightForAge5To10 language zscores weightForAgeMonthsData
                        , zScoreViewCharts.headCircumferenceForAge0To5 language zscores headCircumferenceForAgeData
                        ]
                )
                child.birthDate
                |> Maybe.withDefault []
    in
    div [ class "pane growth" ]
        [ viewPaneHeading language Translate.Growth
        , div [ class "pane-content" ]
            [ div [ class "growth-nutrition-signs" ] <|
                viewNutritionSigns language nutritionValues
            , div [ class "growth-charts" ]
                charts
            , div [ class "growth-photos" ] <|
                viewPhotos photoValues
            ]
        ]


chartHeightForAge : Person -> { dateMeasured : NominalDate, encounterId : String, value : HeightInCm } -> Maybe ( Days, Months, Centimetres )
chartHeightForAge child height =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( diffDays birthDate height.dateMeasured
                , diffMonths birthDate height.dateMeasured |> Months
                , case height.value of
                    HeightInCm cm ->
                        Centimetres cm
                )
            )


chartWeightForAge : Person -> { dateMeasured : NominalDate, encounterId : String, value : WeightInKg } -> Maybe ( Days, Months, Kilograms )
chartWeightForAge child weight =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( diffDays birthDate weight.dateMeasured
                , diffMonths birthDate weight.dateMeasured |> Months
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )


chartHeadCircumferenceForAge : Person -> { dateMeasured : NominalDate, encounterId : String, value : HeadCircumferenceValue } -> Maybe ( Days, Centimetres )
chartHeadCircumferenceForAge child headCircumference =
    if EverySet.member NoteNotTaken headCircumference.value.notes then
        Nothing

    else
        Maybe.map
            (\birthDate ->
                ( diffDays birthDate headCircumference.dateMeasured
                , case headCircumference.value.headCircumference of
                    HeadCircumferenceInCm cm ->
                        Centimetres cm
                )
            )
            child.birthDate


chartWeightForLengthAndHeight :
    List { dateMeasured : NominalDate, encounterId : String, value : HeightInCm }
    -> { dateMeasured : NominalDate, encounterId : String, value : WeightInKg }
    -> Maybe ( Length, ZScore.Model.Height, Kilograms )
chartWeightForLengthAndHeight heights weight =
    -- For each weight, we try to find a height with a matching sessionID.
    heights
        |> List.Extra.find (\height -> height.encounterId == weight.encounterId)
        |> Maybe.map
            (\height ->
                let
                    cm =
                        case height.value of
                            HeightInCm val ->
                                val
                in
                ( Length cm
                , ZScore.Model.Height cm
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )


viewNutritionSigns : Language -> List ( NominalDate, EverySet ChildNutritionSign ) -> List (Html any)
viewNutritionSigns language measurements =
    let
        entriesHeading =
            div [ class "heading nutrition-signs" ]
                [ div [ class "name" ] [ text <| translate language Translate.NutritionSigns ]
                , div [ class "date" ] [ text <| translate language Translate.Date ]
                ]

        entries =
            List.sortWith (sortByDateDesc Tuple.first) measurements
                |> List.filterMap
                    (\( dateMeasured, signs ) ->
                        case EverySet.toList signs of
                            [] ->
                                Nothing

                            [ NormalChildNutrition ] ->
                                Nothing

                            signs_ ->
                                div [ class "entry nutrition-signs" ]
                                    [ List.map (Translate.ChildNutritionSignLabel >> translate language) signs_
                                        |> List.sort
                                        |> String.join ", "
                                        |> text
                                        |> List.singleton
                                        |> div [ class "cell name" ]
                                    , div [ class "cell date" ]
                                        [ text <| formatDDMMYYYY dateMeasured ]
                                    ]
                                    |> Just
                    )
    in
    entriesHeading :: viewEntries language entries


viewPhotos : List { a | dateMeasured : NominalDate, value : ImageUrl } -> List (Html any)
viewPhotos measurements =
    let
        viewImageUrl (ImageUrl url) =
            div
                [ classList
                    [ ( "image", True )
                    , ( "cache-upload", String.contains "cache-upload/images" url )
                    ]
                ]
                [ img [ src url, class "orientation" ] [] ]
    in
    List.sortWith (sortByDateDesc .dateMeasured) measurements
        |> List.map
            (\photo ->
                div
                    [ class "report card" ]
                    [ div [ class "content" ]
                        [ text <| formatDDMMYYYY photo.dateMeasured ]
                    , viewImageUrl photo.value
                    ]
            )
        |> div [ class "ui cards" ]
        |> List.singleton


viewNextAppointmentPane : Language -> NominalDate -> Person -> List WellChildMeasurements -> ModelIndexedDb -> Html any
viewNextAppointmentPane language currentDate child individualWellChildMeasurements db =
    let
        entriesHeading =
            div [ class "heading next-appointment" ]
                [ div [ class "type" ] [ text <| translate language Translate.Type ]
                , div [ class "location" ] [ text <| translate language Translate.Location ]
                , div [ class "date" ] [ text <| translate language Translate.Date ]
                ]

        entries =
            List.head individualWellChildMeasurements
                |> Maybe.andThen
                    (.nextVisit >> getMeasurementValueFunc)
                |> Maybe.map
                    (\value ->
                        let
                            healthCenter =
                                getHealthCenterName child.healthCenterId db
                                    |> Maybe.withDefault ""

                            immunisationEntry =
                                Maybe.map (viewEntry Translate.Immunisation)
                                    value.immunisationDate

                            pediatricVisitDate =
                                Maybe.map (viewEntry Translate.PediatricVisit)
                                    value.pediatricVisitDate

                            viewEntry label date =
                                div [ class "entry next-appointment" ]
                                    [ div [ class "cell type" ] [ text <| translate language label ]
                                    , div [ class "cell location" ] [ text healthCenter ]
                                    , div [ class "cell date" ] [ text <| formatDDMMYYYY date ]
                                    ]
                        in
                        Maybe.Extra.values [ immunisationEntry, pediatricVisitDate ]
                    )
                |> Maybe.withDefault []
    in
    div [ class "pane next-appointment" ] <|
        [ viewPaneHeading language Translate.NextAppointment
        , div [ class "pane-content" ] <|
            entriesHeading
                :: viewEntries language entries
        ]


viewPaneHeading : Language -> TranslationId -> Html any
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]


viewNCDAScorecard :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> ( PersonId, Person )
    -> ModelIndexedDb
    -> List (Html msg)
viewNCDAScorecard language currentDate zscores site ( childId, child ) db =
    let
        reportData =
            assembleProgresReportData site childId db

        groupNCDAs =
            Dict.values reportData.groupNutritionMeasurements.ncda

        nutritionNCDAs =
            List.map (Tuple.second >> Tuple.second >> .ncda >> Maybe.map Tuple.second)
                reportData.individualNutritionMeasurementsWithDates
                |> Maybe.Extra.values

        wellChildNCDAs =
            List.map (Tuple.second >> Tuple.second >> .ncda >> Maybe.map Tuple.second)
                reportData.individualWellChildMeasurementsWithDates
                |> Maybe.Extra.values

        childScoreboardNCDAs =
            List.map (Tuple.second >> Tuple.second >> .ncda >> Maybe.map Tuple.second)
                reportData.individualChildScoreboardMeasurementsWithDates
                |> Maybe.Extra.values

        nurseNCDAQuestionnaires =
            List.map (\ncda -> ( ncda.dateMeasured, ncda.value )) groupNCDAs
                ++ List.map (\ncda -> ( ncda.dateMeasured, ncda.value )) nutritionNCDAs
                ++ List.map (\ncda -> ( ncda.dateMeasured, ncda.value )) wellChildNCDAs

        chwNCDAQuestionnaires =
            List.map (\ncda -> ( ncda.dateMeasured, ncda.value )) childScoreboardNCDAs

        allNCDAQuestionnaires =
            nurseNCDAQuestionnaires ++ chwNCDAQuestionnaires

        allQuestionnairesByAgeInMonths =
            distributeByAgeInMonths child allNCDAQuestionnaires

        nurseQuestionnairesByAgeInMonths =
            distributeByAgeInMonths child nurseNCDAQuestionnaires

        chwQuestionnairesByAgeInMonthsWithDate =
            distributeByAgeInMonthsWithDate child chwNCDAQuestionnaires

        vaccinationProgressDict =
            Maybe.Extra.or
                (Maybe.map .vaccinationProgress reportData.maybeAssembled)
                (Maybe.map (generateVaccinationProgressDictByChildScoreboard site db)
                    reportData.individualChildScoreboardParticipantId
                )
                |> Maybe.withDefault Dict.empty
    in
    [ viewChildIdentificationPane language currentDate site allNCDAQuestionnaires db ( childId, child )
    , viewANCNewbornPane language currentDate db childId child allNCDAQuestionnaires
    , viewUniversalInterventionsPane language
        currentDate
        site
        child
        db
        nurseQuestionnairesByAgeInMonths
        chwQuestionnairesByAgeInMonthsWithDate
        vaccinationProgressDict
    , viewNutritionBehaviorPane language currentDate child allNCDAQuestionnaires allQuestionnairesByAgeInMonths
    , viewTargetedInterventionsPane language
        currentDate
        child
        db
        allQuestionnairesByAgeInMonths
        chwQuestionnairesByAgeInMonthsWithDate
        reportData.groupNutritionMeasurements
        reportData.individualNutritionMeasurementsWithDates
        reportData.individualWellChildMeasurementsWithDates
        reportData.acuteIllnesses
    , viewInfrastructureEnvironmentWashPane language currentDate child allQuestionnairesByAgeInMonths
    , viewFillTheBlanksPane language
        currentDate
        zscores
        child
        db
        allNCDAQuestionnaires
        reportData.groupNutritionMeasurements
        reportData.individualNutritionMeasurementsWithDates
        reportData.individualWellChildMeasurementsWithDates
    ]


viewChildIdentificationPane :
    Language
    -> NominalDate
    -> Site
    -> List ( NominalDate, NCDAValue )
    -> ModelIndexedDb
    -> ( PersonId, Person )
    -> Html any
viewChildIdentificationPane language currentDate site allNCDAQuestionnaires db ( childId, child ) =
    let
        ( mother, father ) =
            let
                parentsIds =
                    Dict.get childId db.relationshipsByPerson
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (Dict.values
                                >> List.filter (.relatedBy >> (==) MyParent)
                                >> EverySet.fromList
                                >> EverySet.toList
                                >> List.map .relatedTo
                            )
                        |> Maybe.withDefault []

                parents =
                    List.filterMap
                        (\personId ->
                            Dict.get personId db.people
                                |> Maybe.andThen RemoteData.toMaybe
                        )
                        parentsIds
            in
            ( List.filter (.gender >> (==) Female) parents
                |> List.head
            , List.filter (.gender >> (==) Male) parents
                |> List.head
            )

        dateOfBirthEntry =
            Maybe.map
                (\birthDate ->
                    viewEntry Translate.DateOfBirth (formatDDMMYYYY birthDate)
                )
                child.birthDate
                |> Maybe.withDefault emptyNode

        motherInfoEntry =
            Maybe.map
                (\person ->
                    [ viewEntry Translate.MotherNameLabel person.name
                    , viewEntry Translate.MotherId (Maybe.withDefault "" person.nationalIdNumber)
                    ]
                )
                mother
                |> Maybe.withDefault []

        fatherInfoEntry =
            Maybe.map
                (\person ->
                    [ viewEntry Translate.FatherOrChiefName person.name
                    , viewEntry Translate.FatherOrChiefId (Maybe.withDefault "" person.nationalIdNumber)
                    ]
                )
                father
                |> Maybe.withDefault []

        childNameEntry =
            viewEntry Translate.ChildName child.name

        bornUnderweightByNewbornExam =
            Maybe.andThen (\value -> Maybe.map (\(WeightInGrm birthWeight) -> birthWeight < 2500) value.birthWeight)
                newbornExamPregnancySummary

        birthDefectByNewbornExam =
            Maybe.map
                (\value ->
                    (not <| EverySet.isEmpty value.birthDefects)
                        && (not <| value.birthDefects == EverySet.singleton NoBirthDefects)
                )
                newbornExamPregnancySummary

        newbornExamPregnancySummary =
            getNewbornExamPregnancySummary childId db

        bornUnderweightByNCDA =
            List.filterMap
                (\( _, value ) ->
                    Maybe.map (\(WeightInGrm birthWeight) -> birthWeight) value.birthWeight
                )
                allNCDAQuestionnaires
                |> List.head
                |> Maybe.map (\weight -> weight < 2500)

        birthDefectByNCDA =
            if List.isEmpty allNCDAQuestionnaires then
                Nothing

            else
                List.any
                    (\( _, value ) -> EverySet.member BornWithBirthDefect value.signs)
                    allNCDAQuestionnaires
                    |> Just

        bornUnderweighEntry =
            Maybe.Extra.or bornUnderweightByNewbornExam bornUnderweightByNCDA
                |> Maybe.map
                    (\confirmed ->
                        let
                            answer =
                                if confirmed then
                                    Translate.Yes

                                else
                                    Translate.No
                        in
                        [ viewEntry Translate.BornUnderweight (translate language answer) ]
                    )
                |> Maybe.withDefault []

        birthDefectEntry =
            Maybe.Extra.or birthDefectByNewbornExam birthDefectByNCDA
                |> Maybe.map
                    (\confirmed ->
                        let
                            answer =
                                if confirmed then
                                    Translate.Yes

                                else
                                    Translate.No
                        in
                        [ viewEntry Translate.BirthDefectLabel (translate language answer) ]
                    )
                |> Maybe.withDefault []

        genderEntry =
            viewEntry Translate.GenderLabel (translate language <| Translate.Gender child.gender)

        ubudeheEntry =
            if site == SiteRwanda then
                Maybe.map
                    (Translate.UbudeheNumber
                        >> translate language
                        >> viewEntry Translate.UbudeheLabel
                        >> List.singleton
                    )
                    child.ubudehe
                    |> Maybe.withDefault []

            else
                []

        viewEntry labelTransId content =
            p []
                [ span [ class "label" ] [ text <| translate language labelTransId ++ ": " ]
                , span [] [ text content ]
                ]
    in
    div [ class "pane child-identification" ]
        [ viewPaneHeading language Translate.ChildIdentification
        , div [ class "pane-content" ]
            [ div [ class "column" ] <|
                [ childNameEntry
                , genderEntry
                , dateOfBirthEntry
                ]
                    ++ bornUnderweighEntry
                    ++ birthDefectEntry
                    ++ ubudeheEntry
            , div [ class "column" ] <|
                motherInfoEntry
                    ++ fatherInfoEntry
            ]
        ]


viewANCNewbornPane :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> PersonId
    -> Person
    -> List ( NominalDate, NCDAValue )
    -> Html any
viewANCNewbornPane language currentDate db childId child allNCDAQuestionnaires =
    let
        pregnancyValuesForRegularPrenatalVisits =
            Maybe.map
                (\birthDate ->
                    let
                        ( eddDate, encountersDatesFromANCData ) =
                            resolveChildANCPregnancyData childId db

                        encountersDatesFromNCDAdata =
                            List.filterMap
                                (\( _, value ) ->
                                    if EverySet.isEmpty value.ancVisitsDates then
                                        Nothing

                                    else
                                        Just value.ancVisitsDates
                                )
                                allNCDAQuestionnaires
                                |> List.head
                                |> Maybe.withDefault EverySet.empty

                        allEncountersDates =
                            EverySet.union encountersDatesFromANCData encountersDatesFromNCDAdata
                                |> EverySet.toList
                    in
                    if List.isEmpty allEncountersDates then
                        List.repeat 9 NCDACellValueEmpty

                    else
                        let
                            pregnancyStartDate =
                                Maybe.map eddToLmpDate eddDate
                                    |> Maybe.withDefault
                                        -- If we don't have LMP date, we'll assume that
                                        -- pregnancy was complete (lasted 9 months).
                                        (eddToLmpDate birthDate)

                            cellValueForMonth =
                                -- Per requirements, if there were at least 4 encounters, we put
                                -- green V. If less, red X.
                                if List.length allEncountersDates < 4 then
                                    NCDACellValueX

                                else
                                    NCDACellValueV
                        in
                        List.repeat 9 NCDACellValueDash
                            |> List.indexedMap
                                (\index cellValue ->
                                    if
                                        List.any
                                            (\encounterDate ->
                                                diffMonths pregnancyStartDate encounterDate == index
                                            )
                                            allEncountersDates
                                    then
                                        cellValueForMonth

                                    else
                                        cellValue
                                )
                )
                child.birthDate
                |> Maybe.withDefault (List.repeat 9 NCDACellValueEmpty)

        pregnancyValuesForIron =
            if List.isEmpty allNCDAQuestionnaires then
                List.repeat 9 NCDACellValueEmpty

            else
                let
                    takenSupplements =
                        List.any (Tuple.second >> .signs >> EverySet.member TakenSupplementsPerGuidance)
                            allNCDAQuestionnaires
                in
                if takenSupplements then
                    List.repeat 9 NCDACellValueV

                else
                    List.repeat 9 NCDACellValueX

        zeroToFiveValues =
            List.repeat 6 NCDACellValueDash

        sixToTwentyFourValues =
            List.repeat 19 NCDACellValueDash
    in
    div [ class "pane anc-newborn" ]
        [ viewPaneHeading language Translate.ANCNewborn
        , div [ class "pane-content" ]
            [ viewTableHeader language
            , viewTableRow language
                (Translate.NCDAANCNewbornItemLabel RegularCheckups)
                pregnancyValuesForRegularPrenatalVisits
                zeroToFiveValues
                sixToTwentyFourValues
            , viewTableRow language
                (Translate.NCDAANCNewbornItemLabel IronDuringPregnancy)
                pregnancyValuesForIron
                zeroToFiveValues
                sixToTwentyFourValues
            ]
        ]


viewNutritionBehaviorPane :
    Language
    -> NominalDate
    -> Person
    -> List ( NominalDate, NCDAValue )
    -> Maybe (Dict Int NCDAValue)
    -> Html any
viewNutritionBehaviorPane language currentDate child allNCDAQuestionnaires allQuestionnairesByAgeInMonths =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        appropriateComplementaryFeedingValues =
            generateValues currentDate
                child
                allQuestionnairesByAgeInMonths
                (.signs >> EverySet.member Backend.Measurement.Model.AppropriateComplementaryFeeding)

        diverseDietValues =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member FiveFoodGroups)

        mealsADayValues =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member MealsAtRecommendedTimes)

        -- Here we are interested only at 0 to 5 months period.
        -- Months after that, will show dahses, in case child has
        -- reached the age for which value is given (empty value otherwise).
        ( breastfedForSixMonthsFirstPeriod, breastfedForSixMonthsSecondPeriod ) =
            let
                breastfedForSixMonthsByAgeInMonths =
                    if List.isEmpty allNCDAQuestionnaires then
                        Dict.empty

                    else
                        let
                            breastfedForSixMonths =
                                List.map (Tuple.second >> .signs >> EverySet.member BreastfedForSixMonths) allNCDAQuestionnaires
                                    |> List.member True

                            firstPeriodIndicator =
                                if breastfedForSixMonths then
                                    NCDACellValueV

                                else
                                    NCDACellValueX

                            first =
                                List.range 0 5
                                    |> List.map (\month -> ( month, firstPeriodIndicator ))

                            second =
                                List.range 6 24
                                    |> List.map (\month -> ( month, NCDACellValueDash ))
                        in
                        first
                            ++ second
                            |> Dict.fromList

                breastfedForSixMonthsValues =
                    generateValues currentDate child (Just breastfedForSixMonthsByAgeInMonths) ((==) NCDACellValueV)

                firstPeriod =
                    List.take 6 breastfedForSixMonthsValues

                secondPeriod =
                    List.drop 6 breastfedForSixMonthsValues
                        |> List.map setDashIfNotEmpty
            in
            ( firstPeriod, secondPeriod )

        -- generateValues() may generate values at certain periods that are
        -- not relevant, which we want to replace them with dashes.
        -- However, if child has not yet reach the age of month for which
        -- value is presented, generateValues() will preperly set
        -- NCDACellValueEmpty there, and we want to keep it.
        setDashIfNotEmpty value =
            if value == NCDACellValueEmpty then
                value

            else
                NCDACellValueDash
    in
    div [ class "pane nutrition-behavior" ]
        [ viewPaneHeading language Translate.NutritionBehavior
        , div [ class "pane-content" ]
            [ viewTableHeader language
            , viewTableRow language
                (Translate.NCDANutritionBehaviorItemLabel Pages.WellChild.ProgressReport.Model.BreastfedSixMonths)
                pregnancyValues
                breastfedForSixMonthsFirstPeriod
                breastfedForSixMonthsSecondPeriod
            , viewTableRow language
                (Translate.NCDANutritionBehaviorItemLabel Pages.WellChild.ProgressReport.Model.AppropriateComplementaryFeeding)
                pregnancyValues
                (List.take 6 appropriateComplementaryFeedingValues)
                (List.drop 6 appropriateComplementaryFeedingValues)
            , viewTableRow language
                (Translate.NCDANutritionBehaviorItemLabel DiverseDiet)
                pregnancyValues
                (List.take 6 diverseDietValues)
                (List.drop 6 diverseDietValues)
            , viewTableRow language
                (Translate.NCDANutritionBehaviorItemLabel MealsADay)
                pregnancyValues
                (List.take 6 mealsADayValues)
                (List.drop 6 mealsADayValues)
            ]
        ]


viewInfrastructureEnvironmentWashPane :
    Language
    -> NominalDate
    -> Person
    -> Maybe (Dict Int NCDAValue)
    -> Html any
viewInfrastructureEnvironmentWashPane language currentDate child allQuestionnairesByAgeInMonths =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        hasToilets =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member Backend.Measurement.Model.HasToilets)

        hasCleanWater =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member Backend.Measurement.Model.HasCleanWater)

        hasHandwashingFacility =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member Backend.Measurement.Model.HasHandwashingFacility)

        hasKitchenGarden =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member Backend.Measurement.Model.HasKitchenGarden)

        insecticideTreatedBedNets =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member Backend.Measurement.Model.InsecticideTreatedBednets)
    in
    div [ class "pane infrastructure-environment-wash" ]
        [ viewPaneHeading language Translate.InfrastructureEnvironmentWash
        , div [ class "pane-content" ]
            [ viewTableHeader language
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel Pages.WellChild.ProgressReport.Model.HasToilets)
                pregnancyValues
                (List.take 6 hasToilets)
                (List.drop 6 hasToilets)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel Pages.WellChild.ProgressReport.Model.HasCleanWater)
                pregnancyValues
                (List.take 6 hasCleanWater)
                (List.drop 6 hasCleanWater)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel Pages.WellChild.ProgressReport.Model.HasHandwashingFacility)
                pregnancyValues
                (List.take 6 hasHandwashingFacility)
                (List.drop 6 hasHandwashingFacility)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel Pages.WellChild.ProgressReport.Model.InsecticideTreatedBedNets)
                pregnancyValues
                (List.take 6 insecticideTreatedBedNets)
                (List.drop 6 insecticideTreatedBedNets)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel Pages.WellChild.ProgressReport.Model.HasKitchenGarden)
                pregnancyValues
                (List.take 6 hasKitchenGarden)
                (List.drop 6 hasKitchenGarden)
            ]
        ]


viewTargetedInterventionsPane :
    Language
    -> NominalDate
    -> Person
    -> ModelIndexedDb
    -> Maybe (Dict Int NCDAValue)
    -> Maybe (Dict Int ( NominalDate, NCDAValue ))
    -> ChildMeasurementList
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html any
viewTargetedInterventionsPane language currentDate child db allQuestionnairesByAgeInMonths chwQuestionnairesByAgeInMonthsWithDate groupNutritionMeasurements individualNutritionMeasurementsWithDates individualWellChildMeasurementsWithDates acuteIllnesses =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        chwQuestionnairesByAgeInMonths =
            Maybe.map (Dict.map (\_ value -> Tuple.second value))
                chwQuestionnairesByAgeInMonthsWithDate

        -- Malnutrition treatment mapping logic:
        -- Per requirements, treatment question appears only at Child Scorecard
        -- encounter, and only in case Malnutrition was not diagnosed previously
        -- (at any of different types of Nutrition encounters).
        -- Therefore, we need to analyze only those questionnaires that
        -- were filled before the first diagnosis of Malnutrition.
        -- This way we solve possibility 'false negative' at questionnaires
        -- that don't show Malnutrition question (and don't have Malnutrition
        -- sign set, which is interpretred as 'No' answer).
        --
        -- So, as first step, generate malnutrition data from Nutrition encounters.
        malnutritionTreatmentData =
            groupMalnutritionTreatmentData
                ++ individualMalnutritionTreatmentData
                |> List.sortWith sortTuplesByDate

        -- Distribute data by age of child, in months.
        malnutritionTreatmentsByAgeInMonthsWithDate =
            distributeByAgeInMonthsWithDate child malnutritionTreatmentData

        -- Generate malnutrition data from questionnaires.
        malnutritionTreatmentsByAgeInMonthsWithDateFromQuestionnaire =
            Maybe.map
                (Dict.map
                    (\_ ( date, value ) ->
                        if EverySet.member TreatedForAcuteMalnutrition value.signs then
                            ( date, NCDACellValueV )

                        else
                            ( date, NCDACellValueX )
                    )
                )
                chwQuestionnairesByAgeInMonthsWithDate

        -- Resolve date of first malnutrition diagnosis.
        firstMalnutritionDiagnosisDate =
            List.head malnutritionTreatmentData
                |> Maybe.map Tuple.first

        malnutritionTreatmentsByAgeInMonthsWithDateBeforeFirstDiagnosis =
            case firstMalnutritionDiagnosisDate of
                Just firstDiagnosisDate ->
                    -- Since first malnutrition diagnosis exists filter out all
                    -- malnutrition data from questionnaires, that was generated
                    -- after first diagnosis was made.
                    Maybe.map
                        (Dict.filter
                            (\_ ( date, _ ) ->
                                Date.compare date firstDiagnosisDate == LT
                            )
                        )
                        malnutritionTreatmentsByAgeInMonthsWithDateFromQuestionnaire

                Nothing ->
                    malnutritionTreatmentsByAgeInMonthsWithDateFromQuestionnaire

        -- Generate final malnutrition data.
        malnutritionTreatmentsByAgeInMonths =
            mergeValuesByAgeInMonthsWithDateDicts
                malnutritionTreatmentsByAgeInMonthsWithDate
                malnutritionTreatmentsByAgeInMonthsWithDateBeforeFirstDiagnosis

        groupMalnutritionTreatmentData =
            let
                malnutritionAssessmentDates =
                    Dict.values groupNutritionMeasurements.nutritions
                        |> List.filterMap
                            (\nutrition ->
                                if
                                    List.any (\assessment -> EverySet.member assessment nutrition.value.assesment)
                                        [ AssesmentAcuteMalnutritionModerate
                                        , AssesmentAcuteMalnutritionSevere
                                        ]
                                then
                                    Just nutrition.dateMeasured

                                else
                                    Nothing
                            )
            in
            Dict.values groupNutritionMeasurements.sendToHC
                |> List.filterMap
                    (\sendToHC ->
                        if
                            -- Sent to HC measurement was taken on same day
                            -- malnutrition assessment was made.
                            List.member sendToHC.dateMeasured malnutritionAssessmentDates
                        then
                            if EverySet.member ReferToHealthCenter sendToHC.value.signs then
                                Just ( sendToHC.dateMeasured, NCDACellValueV )

                            else
                                Just ( sendToHC.dateMeasured, NCDACellValueX )

                        else
                            Nothing
                    )

        individualMalnutritionTreatmentData =
            generateIndividualMalnutritionTreatmentData individualNutritionMeasurementsWithDates
                ++ generateIndividualMalnutritionTreatmentData individualWellChildMeasurementsWithDates

        generateIndividualMalnutritionTreatmentData measurementsWithDates =
            List.filterMap
                (\( date, ( _, measurements ) ) ->
                    getMeasurementValueFunc measurements.nutrition
                        |> Maybe.andThen
                            (\nutritionValue ->
                                if
                                    List.any (\assessment -> EverySet.member assessment nutritionValue.assesment)
                                        [ AssesmentAcuteMalnutritionModerate
                                        , AssesmentAcuteMalnutritionSevere
                                        ]
                                then
                                    getMeasurementValueFunc measurements.sendToHC
                                        |> Maybe.map
                                            (\sendToHCValue ->
                                                if EverySet.member ReferToHealthCenter sendToHCValue.signs then
                                                    ( date, NCDACellValueV )

                                                else
                                                    ( date, NCDACellValueX )
                                            )

                                else
                                    Nothing
                            )
                )
                measurementsWithDates

        diarrheaTreatmenByAgeInMonths =
            Maybe.andThen
                (\birthDate ->
                    List.filter
                        (\( _, participant ) ->
                            diffMonths birthDate participant.startDate < 24
                        )
                        acuteIllnesses
                        |> List.concatMap
                            (\( participantId, _ ) ->
                                getAcuteIllnessEncountersForParticipant db participantId
                                    |> List.filterMap
                                        (\( encounterId, encounter ) ->
                                            -- We need to fetch measurements of encounters where Uncomplicated
                                            -- Gastrointestinal Infection was diagnosed, to check if treatment was given.
                                            if encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated then
                                                Dict.get encounterId db.acuteIllnessMeasurements
                                                    |> Maybe.andThen RemoteData.toMaybe
                                                    |> Maybe.andThen
                                                        (.medicationDistribution
                                                            >> getMeasurementValueFunc
                                                            >> Maybe.map
                                                                (\value ->
                                                                    if
                                                                        List.any (\sign -> EverySet.member sign value.distributionSigns)
                                                                            [ ORS, Zinc ]
                                                                    then
                                                                        ( encounter.startDate, NCDACellValueV )

                                                                    else
                                                                        ( encounter.startDate, NCDACellValueX )
                                                                )
                                                        )

                                            else
                                                Nothing
                                        )
                            )
                        |> distributeByAgeInMonths child
                )
                child.birthDate

        fbfValues =
            generateValues currentDate child chwQuestionnairesByAgeInMonths (.signs >> EverySet.member ChildTakingFBF)

        malnutritionTreatmentValues =
            generateValues currentDate child malnutritionTreatmentsByAgeInMonths ((==) NCDACellValueV)

        diarrheaTreatmentValues =
            generateValues currentDate child diarrheaTreatmenByAgeInMonths ((==) NCDACellValueV)

        supportChildWithDisabilityValues =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member ReceivingSupport)

        conditionalCashTransferValues =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member ReceivingCashTransfer)

        conditionalFoodItemsValues =
            generateValues currentDate child allQuestionnairesByAgeInMonths (.signs >> EverySet.member Backend.Measurement.Model.ConditionalFoodItems)
    in
    div [ class "pane targeted-interventions" ]
        [ viewPaneHeading language Translate.TargetedInterventions
        , div [ class "pane-content" ]
            [ viewTableHeader language
            , viewTableRow language
                (Translate.NCDATargetedInterventionsItemLabel FBFGiven)
                pregnancyValues
                (List.take 6 fbfValues)
                (List.drop 6 fbfValues)
            , viewTableRow language
                (Translate.NCDATargetedInterventionsItemLabel TreatmentForAcuteMalnutrition)
                pregnancyValues
                (List.take 6 malnutritionTreatmentValues)
                (List.drop 6 malnutritionTreatmentValues)
            , viewTableRow language
                (Translate.NCDATargetedInterventionsItemLabel TreatmentForDiarrhea)
                pregnancyValues
                (List.take 6 diarrheaTreatmentValues)
                (List.drop 6 diarrheaTreatmentValues)
            , viewTableRow language
                (Translate.NCDATargetedInterventionsItemLabel SupportChildWithDisability)
                pregnancyValues
                (List.take 6 supportChildWithDisabilityValues)
                (List.drop 6 supportChildWithDisabilityValues)
            , viewTableRow language
                (Translate.NCDATargetedInterventionsItemLabel ConditionalCashTransfer)
                pregnancyValues
                (List.take 6 conditionalCashTransferValues)
                (List.drop 6 conditionalCashTransferValues)
            , viewTableRow language
                (Translate.NCDATargetedInterventionsItemLabel Pages.WellChild.ProgressReport.Model.ConditionalFoodItems)
                pregnancyValues
                (List.take 6 conditionalFoodItemsValues)
                (List.drop 6 conditionalFoodItemsValues)
            ]
        ]


viewUniversalInterventionsPane :
    Language
    -> NominalDate
    -> Site
    -> Person
    -> ModelIndexedDb
    -> Maybe (Dict Int NCDAValue)
    -> Maybe (Dict Int ( NominalDate, NCDAValue ))
    -> VaccinationProgressDict
    -> Html any
viewUniversalInterventionsPane language currentDate site child db nurseQuestionnairesByAgeInMonths chwQuestionnairesByAgeInMonthsWithDate vaccinationProgress =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        chwQuestionnairesByAgeInMonths =
            Maybe.map (Dict.map (\_ value -> Tuple.second value)) chwQuestionnairesByAgeInMonthsWithDate

        immunizationByAgeInMonths =
            Maybe.andThen
                (\birthDate ->
                    List.repeat 25 ""
                        |> List.indexedMap
                            (\index _ ->
                                let
                                    referenceDate =
                                        -- We use it to determine if child was
                                        -- behind on any of vaccines at that month.
                                        resolveLastDayForMonthX index birthDate

                                    -- Filter out vaccinations that were performed
                                    -- after the reference date.
                                    vaccinationProgressOnReferrenceDate =
                                        Dict.map
                                            (\_ dosesDict ->
                                                Dict.filter
                                                    (\_ administeredDate ->
                                                        Date.compare administeredDate referenceDate == LT
                                                    )
                                                    dosesDict
                                            )
                                            vaccinationProgress

                                    futureVaccinations =
                                        generateFutureVaccinationsData currentDate site child.birthDate child.gender False vaccinationProgressOnReferrenceDate

                                    closestDateForVaccination =
                                        List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinations
                                            |> List.sortWith Date.compare
                                            |> List.head
                                in
                                Maybe.map
                                    (\closestDate ->
                                        if Date.compare closestDate referenceDate == GT then
                                            -- Closest date when vaccine is required is after
                                            -- current month, which means that at current month
                                            -- we're not behind on vaccination.
                                            ( referenceDate, NCDACellValueV )

                                        else
                                            ( referenceDate, NCDACellValueX )
                                    )
                                    closestDateForVaccination
                                    |> Maybe.withDefault
                                        -- This indicates that there're no future vaccinations to be
                                        -- done, and therefore, we're on track at current month.
                                        ( referenceDate, NCDACellValueV )
                            )
                        |> distributeByAgeInMonths child
                )
                child.birthDate

        -- Resolves the date for last day of month X after child birth date.
        -- For example, for X = 0, this is
        -- the last day, before child turns 1 month old.
        resolveLastDayForMonthX monthX childBirthDate =
            -- Get to first day of the birth months.
            Date.floor Date.Month childBirthDate
                |> -- Add required number of months.
                   Date.add Date.Months (monthX + 1)
                |> -- Substract one day
                   Date.add Date.Days -1

        immunizationValues =
            generateValues currentDate child immunizationByAgeInMonths ((==) NCDACellValueV)

        -- When CHW conducts NCDA, at Vitamin A section, there's an option for marking as not
        -- applicable. The requirements if this option is selected is to have dash on scorecard.
        -- Dash is set also in case there's not questioneer for the month, so, if in case
        -- 'not applicable', is selected, we filter out the questioneer.
        chwQuestionnairesByAgeInMonthsEliminatingVitaminANotApplicable =
            Maybe.map (Dict.filter (\_ value -> value.receivesVitaminA /= Just OptionNotApplicable))
                chwQuestionnairesByAgeInMonths

        vitaminAValues =
            generateValues currentDate
                child
                chwQuestionnairesByAgeInMonthsEliminatingVitaminANotApplicable
                (.receivesVitaminA >> (==) (Just OptionReceive))
                |> List.indexedMap
                    -- Vitamin A should not be administered before age of 6 months.
                    (postProcessMedicineRawValue 6)

        dewormerValues =
            generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member ChildReceivesDewormer)
                |> List.indexedMap
                    -- Dewormer should not be administered before age of 12 months.
                    (postProcessMedicineRawValue 12)

        postProcessMedicineRawValue startingMonth processingMonth value =
            if List.member value [ NCDACellValueV, NCDACellValueEmpty ] then
                value

            else if processingMonth < startingMonth then
                -- Child is not eligible - too young.
                NCDACellValueDash

            else
                value

        -- When nurse conducts NCDA, questionnaire only asks if Ongera-MNP was
        -- distributed. There's no follow up question asking if it was actually
        -- consumed, while this data is what we need to map.
        -- Therefore, in case we have YES answer for Ongera-MNP distributed question,
        -- we don't know if it was consumed.
        -- To prevent 'false negative' results, we'll filter out nurse questionnaires
        -- that got YES answer for 'Ongera-MNP distributed' question.
        nurseQuestionnairesByAgeInMonthsEliminatingFalseNegatives =
            Maybe.map
                (Dict.filter
                    (\_ value ->
                        not <| EverySet.member Backend.Measurement.Model.OngeraMNP value.signs
                    )
                )
                nurseQuestionnairesByAgeInMonths

        questionnairesByAgeInMonths =
            case ( nurseQuestionnairesByAgeInMonthsEliminatingFalseNegatives, chwQuestionnairesByAgeInMonths ) of
                ( Just nurseDict, Just chwDict ) ->
                    Dict.merge
                        (\key value -> Dict.insert key value)
                        -- In case we got both values for months, we give preference to
                        -- CHW value, because it's the one that can tell us if supplement was
                        -- taken, or not.
                        (\key _ chwValue -> Dict.insert key chwValue)
                        (\key value -> Dict.insert key value)
                        nurseDict
                        chwDict
                        Dict.empty
                        |> Just

                _ ->
                    Maybe.Extra.or nurseQuestionnairesByAgeInMonthsEliminatingFalseNegatives chwQuestionnairesByAgeInMonths

        ongeraMNPValues =
            generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member TakingOngeraMNP)

        ecdValues =
            generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member ChildReceivesECD)
    in
    div [ class "pane universal-interventions" ]
        [ viewPaneHeading language Translate.UniversalInterventions
        , div [ class "pane-content" ]
            [ viewTableHeader language
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel Immunization)
                pregnancyValues
                (List.take 6 immunizationValues)
                (List.drop 6 immunizationValues)
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel Pages.WellChild.ProgressReport.Model.VitaminA)
                pregnancyValues
                (List.take 6 vitaminAValues)
                (List.drop 6 vitaminAValues)
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel Deworming)
                pregnancyValues
                (List.take 6 dewormerValues)
                (List.drop 6 dewormerValues)
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel Pages.WellChild.ProgressReport.Model.OngeraMNP)
                pregnancyValues
                (List.take 6 ongeraMNPValues)
                (List.drop 6 ongeraMNPValues)
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel ECDServices)
                pregnancyValues
                (List.take 6 ecdValues)
                (List.drop 6 ecdValues)
            ]
        ]


viewFillTheBlanksPane :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> ModelIndexedDb
    -> List ( NominalDate, NCDAValue )
    -> ChildMeasurementList
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> Html any
viewFillTheBlanksPane language currentDate zscores child db allNCDAQuestionnaires groupNutritionMeasurements individualNutritionMeasurementsWithDates individualWellChildMeasurementsWithDates =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                child.birthDate

        heightsValues =
            generateFillTheBlanksValues heightsByAgeInMonths

        weightsValues =
            generateFillTheBlanksValues weightsByAgeInMonths

        muacsValues =
            generateFillTheBlanksValues muacsByAgeInMonths

        nutritionsValues =
            generateFillTheBlanksValues nutritionsByAgeInMonths

        generateFillTheBlanksValues valuesByAgeInMonths =
            Maybe.map2
                (\values ageMonths ->
                    List.indexedMap
                        (\month _ ->
                            if ageMonths < month then
                                NCDACellValueEmpty

                            else
                                Dict.get month values
                                    |> Maybe.withDefault NCDACellValueDash
                        )
                        emptyNCDAValuesForChild
                )
                valuesByAgeInMonths
                (ageInMonths currentDate child)
                |> Maybe.withDefault emptyNCDAValuesForChild

        heightsByAgeInMonths =
            mergeValuesByAgeInMonthsWithDateDicts
                heightsByAgeInMonthsWithDateFromNutrition
                heightsByAgeInMonthsWithDateFromNCDA

        heightsByAgeInMonthsWithDateFromNutrition =
            Maybe.map
                (\ageInDays ->
                    List.filterMap
                        (\( date, set ) ->
                            Maybe.andThen
                                (\(HeightInCm height) ->
                                    zScoreLengthHeightForAge zscores ageInDays child.gender (Centimetres height)
                                        |> Maybe.map (\zscore -> ( date, cellValueByZscore zscore ))
                                )
                                set.height
                        )
                        allValuesSetFromNutrition
                )
                maybeAgeInDays
                |> Maybe.withDefault []
                |> distributeByAgeInMonthsWithDate child

        heightsByAgeInMonthsWithDateFromNCDA =
            List.filterMap
                (\( date, set ) ->
                    Maybe.map
                        (\stuntingLevel ->
                            case stuntingLevel of
                                LevelRed ->
                                    ( date, NCDACellValueT )

                                LevelYellow ->
                                    ( date, NCDACellValueH )

                                LevelGreen ->
                                    ( date, NCDACellValueC )
                        )
                        set.stuntingLevel
                )
                ncdaValuesSet
                |> distributeByAgeInMonthsWithDate child

        weightsByAgeInMonths =
            mergeValuesByAgeInMonthsWithDateDicts
                weightsByAgeInMonthsWithDateFromNutrition
                weightsByAgeInMonthsWithDateFromNCDA

        weightsByAgeInMonthsWithDateFromNutrition =
            Maybe.map
                (\ageInDays ->
                    List.filterMap
                        (\( date, set ) ->
                            Maybe.andThen
                                (\(WeightInKg weight) ->
                                    zScoreWeightForAge zscores ageInDays child.gender (Kilograms weight)
                                        |> Maybe.map (\zscore -> ( date, cellValueByZscore zscore ))
                                )
                                set.weight
                        )
                        allValuesSetFromNutrition
                )
                maybeAgeInDays
                |> Maybe.withDefault []
                |> distributeByAgeInMonthsWithDate child

        weightsByAgeInMonthsWithDateFromNCDA =
            Maybe.map
                (\ageInDays ->
                    List.filterMap
                        (\( date, set ) ->
                            Maybe.andThen
                                (\(WeightInKg weight) ->
                                    zScoreWeightForAge zscores ageInDays child.gender (Kilograms weight)
                                        |> Maybe.map (\zscore -> ( date, cellValueByZscore zscore ))
                                )
                                set.weight
                        )
                        ncdaValuesSet
                )
                maybeAgeInDays
                |> Maybe.withDefault []
                |> distributeByAgeInMonthsWithDate child

        cellValueByZscore zscore =
            if zscore < -3 then
                NCDACellValueT

            else if zscore < -2 then
                NCDACellValueH

            else
                NCDACellValueC

        muacsByAgeInMonths =
            mergeValuesByAgeInMonthsWithDateDicts
                muacsByAgeInMonthsWithDateFromNutrition
                muacsByAgeInMonthsWithDateFromNCDA

        muacsByAgeInMonthsWithDateFromNutrition =
            List.filterMap
                (\( date, set ) ->
                    Maybe.map
                        (\value ->
                            let
                                cellValue =
                                    case muacIndication value of
                                        ColorAlertRed ->
                                            NCDACellValueT

                                        ColorAlertYellow ->
                                            NCDACellValueH

                                        ColorAlertGreen ->
                                            NCDACellValueC
                            in
                            ( date, cellValue )
                        )
                        set.muac
                )
                allValuesSetFromNutrition
                |> distributeByAgeInMonthsWithDate child

        muacsByAgeInMonthsWithDateFromNCDA =
            List.filterMap
                (\( date, set ) ->
                    Maybe.map
                        (\value ->
                            let
                                cellValue =
                                    case muacIndication value of
                                        ColorAlertRed ->
                                            NCDACellValueT

                                        ColorAlertYellow ->
                                            NCDACellValueH

                                        ColorAlertGreen ->
                                            NCDACellValueC
                            in
                            ( date, cellValue )
                        )
                        set.muac
                )
                ncdaValuesSet
                |> distributeByAgeInMonthsWithDate child

        nutritionsByAgeInMonths =
            mergeValuesByAgeInMonthsWithDateDicts
                nutritionsByAgeInMonthsWithDateFromNutrition
                nutritionsByAgeInMonthsWithDateFromNCDA

        nutritionsByAgeInMonthsWithDateFromNutrition =
            List.filterMap
                (\( date, set ) ->
                    Maybe.map
                        (\value ->
                            let
                                cellValue =
                                    if EverySet.member Edema value.signs then
                                        NCDACellValueT

                                    else
                                        NCDACellValueC
                            in
                            ( date, cellValue )
                        )
                        set.nutrition
                )
                allValuesSetFromNutrition
                |> distributeByAgeInMonthsWithDate child

        nutritionsByAgeInMonthsWithDateFromNCDA =
            List.filterMap
                (\( date, set ) ->
                    Maybe.map
                        (\signs ->
                            let
                                cellValue =
                                    if List.member Edema signs then
                                        NCDACellValueT

                                    else
                                        NCDACellValueC
                            in
                            ( date, cellValue )
                        )
                        set.nutrition
                )
                ncdaValuesSet
                |> distributeByAgeInMonthsWithDate child

        ncdaValuesSet =
            List.map
                (\( date, value ) ->
                    ( date
                    , { stuntingLevel = value.stuntingLevel
                      , weight = value.weight
                      , muac = value.muac
                      , nutrition =
                            if EverySet.member ShowsEdemaSigns value.signs then
                                Just [ Edema ]

                            else
                                Nothing
                      }
                    )
                )
                allNCDAQuestionnaires

        allValuesSetFromNutrition =
            nutritionValuesSets ++ wellChildValuesSets ++ groupsValuesSets

        nutritionValuesSets =
            List.map
                (\( date, ( _, measurements ) ) ->
                    generateIndividualValuesSet date measurements
                )
                individualNutritionMeasurementsWithDates

        wellChildValuesSets =
            List.map
                (\( date, ( _, measurements ) ) ->
                    generateIndividualValuesSet date measurements
                )
                individualWellChildMeasurementsWithDates

        generateIndividualValuesSet date measurements =
            ( date
            , { height = getMeasurementValueFunc measurements.height
              , weight = getMeasurementValueFunc measurements.weight
              , muac = getMeasurementValueFunc measurements.muac
              , nutrition = getMeasurementValueFunc measurements.nutrition
              }
            )

        groupsValuesSets =
            List.map
                (\date ->
                    ( date
                    , { height = Dict.get date groupHeightsByDate
                      , weight = Dict.get date groupWeightsByDate
                      , muac = Dict.get date groupMuacsByDate
                      , nutrition = Dict.get date groupNutritionsByDate
                      }
                    )
                )
                groupEncounterDates

        groupEncounterDates =
            Dict.keys groupHeightsByDate
                ++ Dict.keys groupWeightsByDate
                ++ Dict.keys groupMuacsByDate
                ++ Dict.keys groupNutritionsByDate
                |> Pages.Utils.unique

        groupHeightsByDate =
            Dict.values groupNutritionMeasurements.heights
                |> List.map (\height -> ( height.dateMeasured, height.value ))
                |> Dict.fromList

        groupWeightsByDate =
            Dict.values groupNutritionMeasurements.weights
                |> List.map (\weight -> ( weight.dateMeasured, weight.value ))
                |> Dict.fromList

        groupMuacsByDate =
            Dict.values groupNutritionMeasurements.muacs
                |> List.map (\muac -> ( muac.dateMeasured, muac.value ))
                |> Dict.fromList

        groupNutritionsByDate =
            Dict.values groupNutritionMeasurements.nutritions
                |> List.map (\nutrition -> ( nutrition.dateMeasured, nutrition.value ))
                |> Dict.fromList
    in
    div [ class "pane fill-the-blanks" ]
        [ viewPaneHeading language Translate.FillTheBlanks
        , div [ class "pane-content" ]
            [ viewTableHeader language
            , viewTableRow language
                (Translate.NCDAFillTheBlanksItemLabel HeightToAge)
                pregnancyValues
                (List.take 6 heightsValues)
                (List.drop 6 heightsValues)
            , viewTableRow language
                (Translate.NCDAFillTheBlanksItemLabel WeightToAge)
                pregnancyValues
                (List.take 6 weightsValues)
                (List.drop 6 weightsValues)
            , viewTableRow language
                (Translate.NCDAFillTheBlanksItemLabel MuacValue)
                pregnancyValues
                (List.take 6 muacsValues)
                (List.drop 6 muacsValues)
            , viewTableRow language
                (Translate.NCDAFillTheBlanksItemLabel EdemaPresent)
                pregnancyValues
                (List.take 6 nutritionsValues)
                (List.drop 6 nutritionsValues)
            ]
        ]


viewTableHeader : Language -> Html any
viewTableHeader language =
    div [ class "table-header" ]
        [ div [ class "activity" ] [ text <| translate language Translate.Activity ]
        , div [ class "flex-column pregnancy" ]
            [ div [ class "column-heading" ] [ text <| translate language Translate.Pregnancy ]
            , List.repeat 9 ""
                |> List.indexedMap
                    (\index _ ->
                        div [ class "month" ] [ text <| String.fromInt <| index + 1 ]
                    )
                |> div [ class "months" ]
            ]
        , div [ class "flex-column 0-5" ]
            [ div [ class "column-heading" ] [ text <| translate language Translate.Child0to5 ]
            , List.repeat 6 ""
                |> List.indexedMap
                    (\index _ ->
                        div [ class "month" ] [ text <| String.fromInt index ]
                    )
                |> div [ class "months" ]
            ]
        , div [ class "flex-column 6-24" ]
            [ div [ class "column-heading" ] [ text <| translate language Translate.Child6to24 ]
            , List.repeat 19 ""
                |> List.indexedMap
                    (\index _ ->
                        div [ class "month" ] [ text <| String.fromInt <| index + 6 ]
                    )
                |> div [ class "months" ]
            ]
        ]


viewTableRow : Language -> TranslationId -> List NCDACellValue -> List NCDACellValue -> List NCDACellValue -> Html any
viewTableRow language itemTransId pregnancyValues zeroToFiveValues sixToTwentyFourValues =
    let
        viewCellValue cellValue =
            case cellValue of
                NCDACellValueV ->
                    span [ class "green" ] [ text "v" ]

                NCDACellValueX ->
                    span [ class "red" ] [ text "x" ]

                NCDACellValueDash ->
                    span [] [ text "-" ]

                NCDACellValueC ->
                    span [ class "green" ] [ text "c" ]

                NCDACellValueH ->
                    span [ class "orange" ] [ text "h" ]

                NCDACellValueT ->
                    span [ class "red" ] [ text "t" ]

                NCDACellValueEmpty ->
                    emptyNode
    in
    div [ class "table-row" ]
        [ div [ class "activity" ] [ text <| translate language itemTransId ]
        , List.indexedMap
            (\index value ->
                div [ class "month" ]
                    [ span [ class "hidden" ] [ text <| String.fromInt <| index + 1 ]
                    , viewCellValue value
                    ]
            )
            pregnancyValues
            |> div [ class "months" ]
        , List.indexedMap
            (\index value ->
                div [ class "month" ]
                    [ span [ class "hidden" ] [ text <| String.fromInt index ]
                    , viewCellValue value
                    ]
            )
            zeroToFiveValues
            |> div [ class "months" ]
        , List.indexedMap
            (\index value ->
                div [ class "month" ]
                    [ span [ class "hidden" ] [ text <| String.fromInt <| index + 6 ]
                    , viewCellValue value
                    ]
            )
            sixToTwentyFourValues
            |> div [ class "months" ]
        ]


distributeByAgeInMonthsWithDate : Person -> List ( NominalDate, a ) -> Maybe (Dict Int ( NominalDate, a ))
distributeByAgeInMonthsWithDate child values =
    Maybe.map
        (\birthDate ->
            List.sortWith sortTuplesByDateDesc values
                |> List.foldl
                    (\( date, value ) accum ->
                        let
                            ageMonths =
                                diffMonths birthDate date
                        in
                        Dict.get ageMonths accum
                            |> Maybe.map
                                -- We want to get most recent value for a month.
                                -- Since values are sorted DESC by date, we
                                -- know that if we already recorded value for that
                                -- month, it's more recent than the one we are checking,
                                -- and therefore this value can be skipped.
                                (always accum)
                            |> Maybe.withDefault (Dict.insert ageMonths ( date, value ) accum)
                    )
                    Dict.empty
        )
        child.birthDate


distributeByAgeInMonths : Person -> List ( NominalDate, a ) -> Maybe (Dict Int a)
distributeByAgeInMonths child values =
    distributeByAgeInMonthsWithDate child values
        |> Maybe.map (Dict.map (\_ value -> Tuple.second value))


generateValues : NominalDate -> Person -> Maybe (Dict Int a) -> (a -> Bool) -> List NCDACellValue
generateValues currentDate child valuesByAgeInMonths resolutionFunc =
    Maybe.map2
        (\values ageMonths ->
            List.indexedMap
                (\month _ ->
                    if ageMonths < month then
                        NCDACellValueEmpty

                    else
                        Dict.get month values
                            |> Maybe.map
                                (\value ->
                                    if resolutionFunc value then
                                        NCDACellValueV

                                    else
                                        NCDACellValueX
                                )
                            |> Maybe.withDefault NCDACellValueDash
                )
                emptyNCDAValuesForChild
        )
        valuesByAgeInMonths
        (ageInMonths currentDate child)
        |> Maybe.withDefault emptyNCDAValuesForChild


emptyNCDAValuesForChild : List NCDACellValue
emptyNCDAValuesForChild =
    List.repeat 25 NCDACellValueEmpty


mergeValuesByAgeInMonthsWithDateDicts : Maybe (Dict Int ( NominalDate, a )) -> Maybe (Dict Int ( NominalDate, a )) -> Maybe (Dict Int a)
mergeValuesByAgeInMonthsWithDateDicts dict1 dict2 =
    case ( dict1, dict2 ) of
        ( Just dict1_, Just dict2_ ) ->
            Dict.merge
                (\key value -> Dict.insert key (Tuple.second value))
                -- In case we got both values for months, we give preference to
                -- the one with more recent date.
                (\key value1 value2 ->
                    if Date.compare (Tuple.first value1) (Tuple.first value2) == GT then
                        Dict.insert key (Tuple.second value1)

                    else
                        Dict.insert key (Tuple.second value2)
                )
                (\key value -> Dict.insert key (Tuple.second value))
                dict1_
                dict2_
                Dict.empty
                |> Just

        _ ->
            Maybe.Extra.or dict1 dict2
                |> Maybe.map (Dict.map (\_ value -> Tuple.second value))
