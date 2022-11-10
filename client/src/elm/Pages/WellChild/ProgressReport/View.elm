module Pages.WellChild.ProgressReport.View exposing
    ( view
    , viewContent
    , viewHeader
    , viewNCDAScorecard
    , viewNutritionSigns
    , viewPaneHeading
    , viewPersonInfoPane
    , viewProgressReport
    )

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementDateMeasuredFunc, getMeasurementValueFunc, muacIndication, nutritionAssessmentToComparable)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getNutritionEncountersForParticipant
        , getWellChildEncountersForParticipant
        , sortByDate
        , sortByDateDesc
        , sortDatesDesc
        , sortEncounterTuplesDesc
        , sortTuplesByDateDesc
        )
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, getHealthCenterName, graduatingAgeInMonth, isChildUnderAgeOf5, isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Session.Model exposing (Session)
import Backend.WellChildEncounter.Model
    exposing
        ( EncounterWarning(..)
        , PediatricCareMilestone(..)
        , WellChildEncounter
        , WellChildEncounterType(..)
        , ecdMilestoneWarnings
        , headCircumferenceWarnings
        , pediatricCareMilestones
        )
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, diffWeeks, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isNothing)
import Measurement.View exposing (renderDatePart, viewActionTakenLabel)
import Pages.AcuteIllness.Participant.Utils exposing (isAcuteIllnessActive)
import Pages.Nutrition.Activity.View exposing (translateNutritionAssement)
import Pages.Nutrition.Encounter.Utils
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Report.Model exposing (DiagnosisMode(..), PaneEntryStatus(..))
import Pages.Report.Utils
    exposing
        ( diagnosisEntryStatusToString
        , getAcuteIllnessDiagnosisForEncounters
        , getAcuteIllnessEncountersForParticipant
        )
import Pages.Report.View exposing (viewAcuteIllnessDiagnosisEntry, viewEntries)
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetailsExtended, viewStartEncounterButton)
import Pages.WellChild.Activity.Types exposing (VaccinationStatus(..))
import Pages.WellChild.Activity.Utils
    exposing
        ( expectedECDSignsOnMilestone
        , generateCompletedECDSigns
        , generateFutureVaccinationsData
        , getPreviousMeasurements
        , mandatoryNutritionAssessmentTasksCompleted
        )
import Pages.WellChild.Activity.View exposing (viewVaccinationOverview)
import Pages.WellChild.Encounter.Model exposing (AssembledData, VaccinationProgressDict)
import Pages.WellChild.Encounter.Utils
    exposing
        ( generateAssembledData
        , pediatricCareMilestoneToComparable
        , resolveDateForPediatricCareMilestone
        , resolvePediatricCareMilestoneOnDate
        )
import Pages.WellChild.Encounter.View exposing (allowEndingEcounter, partitionActivities)
import Pages.WellChild.ProgressReport.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Days(..), Kilograms(..), Length(..), Months(..), ZScore)
import ZScore.Utils exposing (diffDays, zScoreLengthHeightForAge, zScoreWeightForAge)
import ZScore.View


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
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
            generateAssembledData id db
                |> RemoteData.toMaybe

        ( bottomActionData, mandatoryNutritionAssessmentMeasurementsTaken ) =
            Maybe.map
                (\assembled ->
                    let
                        ( _, pendingActivities ) =
                            partitionActivities currentDate zscores isChw db assembled
                    in
                    ( Just <|
                        { showEndEncounterDialog = model.showEndEncounterDialog
                        , allowEndEncounter = allowEndingEcounter pendingActivities
                        , closeEncounterMsg = CloseEncounter id
                        , setEndEncounterDialogStateMsg = SetEndEncounterDialogState
                        , startEncounterMsg = NoOp
                        }
                    , mandatoryNutritionAssessmentTasksCompleted currentDate isChw assembled db
                    )
                )
                assembledData
                |> Maybe.withDefault ( Nothing, False )

        initiator =
            InitiatorWellChild id
    in
    viewWebData language
        (viewProgressReport language
            currentDate
            zscores
            isChw
            initiator
            mandatoryNutritionAssessmentMeasurementsTaken
            db
            model.diagnosisMode
            SetActivePage
            SetDiagnosisMode
            bottomActionData
        )
        identity
        childData


viewProgressReport :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> WellChildProgressReportInitiator
    -> Bool
    -> ModelIndexedDb
    -> DiagnosisMode
    -> (Page -> msg)
    -> (DiagnosisMode -> msg)
    -> Maybe (BottomActionData msg)
    -> ( PersonId, Person )
    -> Html msg
viewProgressReport language currentDate zscores isChw initiator mandatoryNutritionAssessmentMeasurementsTaken db diagnosisMode setActivePageMsg setDiagnosisModeMsg bottomActionData ( childId, child ) =
    div [ class "page-report well-child" ]
        [ viewHeader language initiator diagnosisMode setActivePageMsg setDiagnosisModeMsg
        , viewContent language
            currentDate
            zscores
            isChw
            initiator
            mandatoryNutritionAssessmentMeasurementsTaken
            db
            diagnosisMode
            setActivePageMsg
            setDiagnosisModeMsg
            bottomActionData
            ( childId, child )
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
            case diagnosisMode of
                ModeActiveDiagnosis ->
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
                    in
                    setActivePageMsg targetPage

                ModeCompletedDiagnosis ->
                    setDiagnosisModeMsg ModeActiveDiagnosis
    in
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label
            ]
        , span
            [ class "link-back"
            , onClick goBackAction
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


assembleProgresReportData childId db =
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
                    generateAssembledData id db
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
    }


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> WellChildProgressReportInitiator
    -> Bool
    -> ModelIndexedDb
    -> DiagnosisMode
    -> (Page -> msg)
    -> (DiagnosisMode -> msg)
    -> Maybe (BottomActionData msg)
    -> ( PersonId, Person )
    -> Html msg
viewContent language currentDate zscores isChw initiator mandatoryNutritionAssessmentMeasurementsTaken db diagnosisMode setActivePageMsg setDiagnosisModeMsg bottomActionData ( childId, child ) =
    let
        reportData =
            assembleProgresReportData childId db

        individualWellChildMeasurements =
            getPreviousMeasurements reportData.individualWellChildMeasurementsWithDates

        individualNutritionMeasurements =
            getPreviousMeasurements reportData.individualNutritionMeasurementsWithDates

        vaccinationProgress =
            Maybe.map .vaccinationProgress reportData.maybeAssembled
                |> Maybe.withDefault Dict.empty

        derivedContent =
            case diagnosisMode of
                ModeActiveDiagnosis ->
                    [ viewVaccinationHistoryPane language
                        currentDate
                        child
                        vaccinationProgress
                        db
                    , viewECDPane language
                        currentDate
                        child
                        reportData.wellChildEncounters
                        reportData.individualWellChildMeasurementsWithDates
                        db
                    , viewGrowthPane language
                        currentDate
                        zscores
                        ( childId, child )
                        reportData.expectedSessions
                        reportData.groupNutritionMeasurements
                        reportData.individualNutritionMeasurementsWithDates
                        reportData.individualWellChildMeasurementsWithDates
                    , viewNextAppointmentPane language
                        currentDate
                        child
                        individualWellChildMeasurements
                        db
                    ]

                ModeCompletedDiagnosis ->
                    []

        ( endEncounterDialog, bottomActionButton ) =
            Maybe.map
                (\data ->
                    ( if data.showEndEncounterDialog then
                        Just <|
                            viewEndEncounterDialog language
                                Translate.EndEncounterQuestion
                                Translate.OnceYouEndTheEncounter
                                data.closeEncounterMsg
                                (data.setEndEncounterDialogStateMsg False)

                      else
                        Nothing
                    , case initiator of
                        Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord _ _ ->
                            viewStartEncounterButton language data.startEncounterMsg

                        _ ->
                            viewEndEncounterButton language data.allowEndEncounter data.setEndEncounterDialogStateMsg
                    )
                )
                bottomActionData
                |> Maybe.withDefault ( Nothing, emptyNode )
    in
    div [ class "ui report unstackable items" ] <|
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
            reportData.maybeAssembled
        , viewModal endEncounterDialog
        ]
            ++ derivedContent
            ++ [ bottomActionButton ]


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
    -> Maybe AssembledData
    -> Html msg
viewDiagnosisPane language currentDate isChw initiator mandatoryNutritionAssessmentMeasurementsTaken acuteIllnesses individualNutritionParticipantId wellChildEncounters groupNutritionMeasurements individualNutritionMeasurements individualWellChildMeasurements db diagnosisMode setActivePageMsg setDiagnosisModeMsg maybeAssembled =
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
                isChw
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
            case diagnosisMode of
                ModeActiveDiagnosis ->
                    ( Translate.ActiveDiagnosis
                    , div [ class "pane-action" ]
                        [ button
                            [ class "ui primary button"
                            , onClick <| setDiagnosisModeMsg ModeCompletedDiagnosis
                            ]
                            [ text <| translate language Translate.ReviewPriorDiagnosis ]
                        ]
                    )

                ModeCompletedDiagnosis ->
                    ( Translate.PriorDiagnosis
                    , emptyNode
                    )

        ( selectedDiagnosisEntries, selectedAssessmentEntries, selectedWarningEntries ) =
            case diagnosisMode of
                ModeActiveDiagnosis ->
                    ( List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activeIllnesses
                    , List.map (\( date, data ) -> ( date, ( data, StatusOngoing ) )) activeAssessmentEntries
                    , List.map (\( date, milestone, data ) -> ( date, ( milestone, data, StatusOngoing ) )) activeWarningEntries
                    )

                ModeCompletedDiagnosis ->
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
                                    Backend.AcuteIllnessEncounter.Model.InitiatorPatientRecord patientRecordInitiator personId
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
        , priorDiagniosisButton
        ]


generateIndividualNutritionAssessmentEntries :
    List
        { c
            | nutrition :
                Maybe
                    ( id
                    , { b
                        | dateMeasured : NominalDate
                        , value : NutritionValue
                      }
                    )
        }
    -> List ( NominalDate, List NutritionAssessment )
generateIndividualNutritionAssessmentEntries measurementList =
    List.map
        (\measurements ->
            Maybe.map2 filterNutritionAssessments
                (getMeasurementDateMeasuredFunc measurements.nutrition)
                (getMeasurementValueFunc measurements.nutrition)
                |> Maybe.Extra.join
        )
        measurementList
        |> Maybe.Extra.values


filterNutritionAssessments : NominalDate -> NutritionValue -> Maybe ( NominalDate, List NutritionAssessment )
filterNutritionAssessments dateMeasured value =
    let
        assesments =
            EverySet.toList value.assesment
                |> List.filterMap
                    (\assesment ->
                        case assesment of
                            NoNutritionAssessment ->
                                Nothing

                            AssesmentMalnutritionSigns _ ->
                                Just <| AssesmentMalnutritionSigns (EverySet.toList value.signs)

                            _ ->
                                Just assesment
                    )
    in
    if List.isEmpty assesments then
        Nothing

    else
        Just ( dateMeasured, assesments )


generateGroupNutritionAssessmentEntries : ChildMeasurementList -> List ( NominalDate, List NutritionAssessment )
generateGroupNutritionAssessmentEntries measurementList =
    Dict.values measurementList.nutritions
        |> List.filterMap
            (\nutrition -> filterNutritionAssessments nutrition.dateMeasured nutrition.value)


resolveDateOfLastNutritionAssessment :
    NominalDate
    -> Bool
    -> WellChildProgressReportInitiator
    -> Bool
    -> Maybe IndividualEncounterParticipantId
    -> List ( WellChildEncounterId, WellChildEncounter )
    -> ChildMeasurementList
    -> ModelIndexedDb
    -> Maybe NominalDate
resolveDateOfLastNutritionAssessment currentDate isChw initiator mandatoryNutritionAssessmentMeasurementsTaken individualNutritionParticipantId wellChildEncounters groupNutritionMeasurements db =
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


viewVaccinationHistoryPane : Language -> NominalDate -> Person -> VaccinationProgressDict -> ModelIndexedDb -> Html any
viewVaccinationHistoryPane language currentDate child vaccinationProgress db =
    div [ class "pane vaccination-history" ] <|
        [ viewPaneHeading language Translate.ImmunisationHistory
        , div [ class "pane-content" ] <|
            viewVaccinationOverview language currentDate child vaccinationProgress db
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

                milestonesToCurrentDateWithStatus =
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
                            if Date.compare date milestoneDate == GT then
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
    -> ( PersonId, Person )
    -> Dict SessionId Session
    -> ChildMeasurementList
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> Html any
viewGrowthPane language currentDate zscores ( childId, child ) expected historical nutritionMeasurements wellChildMeasurements =
    let
        --
        -- GROUP CONTEXT
        --
        expectedSessions =
            Dict.toList expected
                |> List.map (\( uuid, expectedSession ) -> ( fromEntityUuid uuid, expectedSession.startDate ))
                |> List.filter hasGroupMeasurement

        -- Do we have any kind of measurement for the child for the specified session?
        hasGroupMeasurement ( id, _ ) =
            Dict.member id heightValuesBySession
                || Dict.member id muacValuesBySession
                || Dict.member id weightValuesBySession
                || Dict.member id nutritionValuesBySession
                || Dict.member id photoValuesBySession

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

        muacValuesBySession =
            valuesIndexedBySession .muacs

        photoValuesBySession =
            valuesIndexedBySession .photos

        nutritionValuesBySession =
            valuesIndexedBySession .nutritions

        --
        -- INDIVIDUAL CONTEXT
        --
        expectedIndividualEncounters =
            List.map (\( startDate, ( uuid, _ ) ) -> ( fromEntityUuid uuid, startDate ))
                >> List.filter hasEncounterMeasurement

        -- Do we have any kind of measurement for the child for the specified encounter?
        hasEncounterMeasurement ( id, _ ) =
            Dict.member id heightValuesByEncounter
                || Dict.member id muacValuesByEncounter
                || Dict.member id weightValuesByEncounter
                || Dict.member id nutritionValuesByEncounter
                || Dict.member id photoValuesByEncounter

        valuesIndexedByEncounter func =
            List.filterMap
                (\( startDate, ( uuid, measurements ) ) ->
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

        muacValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .muac nutritionMeasurements)
                (valuesIndexedByEncounter .muac wellChildMeasurements)

        photoValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .photo nutritionMeasurements)
                (valuesIndexedByEncounter .photo wellChildMeasurements)

        nutritionValuesByEncounter =
            Dict.union
                (valuesIndexedByEncounter .nutrition nutritionMeasurements)
                (valuesIndexedByEncounter .nutrition wellChildMeasurements)

        headCircumferenceValuesByEncounter =
            valuesIndexedByEncounter .headCircumference wellChildMeasurements

        --
        -- COMMON CONTEXT
        --
        sessionsAndEncounters =
            expectedSessions
                ++ expectedIndividualEncounters nutritionMeasurements
                ++ expectedIndividualEncounters wellChildMeasurements
                |> List.sortWith (\s1 s2 -> Date.compare (Tuple.second s1) (Tuple.second s2))
                |> List.reverse

        heightValues =
            Dict.values heightValuesBySession ++ Dict.values heightValuesByEncounter

        muacValues =
            Dict.values muacValuesBySession ++ Dict.values muacValuesByEncounter

        weightValues =
            Dict.values weightValuesBySession ++ Dict.values weightValuesByEncounter

        nutritionValues =
            Dict.values nutritionValuesBySession
                ++ Dict.values nutritionValuesByEncounter
                |> List.map (\measurement -> ( measurement.dateMeasured, measurement.value.signs ))

        photoValues =
            Dict.values photoValuesBySession ++ Dict.values photoValuesByEncounter

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

        heightForAgeData =
            List.filterMap (chartHeightForAge child) heightValues

        heightForAgeDaysData =
            heightForAgeData
                |> List.map (\( days, month, height ) -> ( days, height ))

        heightForAgeMonthsData =
            heightForAgeData
                |> List.map (\( days, month, height ) -> ( month, height ))

        weightForAgeData =
            List.filterMap (chartWeightForAge child) weightValues

        weightForAgeDaysData =
            weightForAgeData
                |> List.map (\( days, month, weight ) -> ( days, weight ))

        weightForAgeMonthsData =
            weightForAgeData
                |> List.map (\( days, month, weight ) -> ( month, weight ))

        weightForLengthAndHeightData =
            List.filterMap (chartWeightForLengthAndHeight heightValues) weightValues

        weightForLengthData =
            weightForLengthAndHeightData
                |> List.map (\( length, height, weight ) -> ( length, weight ))

        weightForHeightData =
            weightForLengthAndHeightData
                |> List.map (\( length, height, weight ) -> ( height, weight ))

        headCircumferenceForAgeData =
            List.filterMap (chartHeadCircumferenceForAge child) headCircumferenceValues

        charts =
            Maybe.map
                (\birthDate ->
                    let
                        childAgeInWeeks =
                            diffWeeks birthDate currentDate

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
                viewNutritionSigns language child nutritionValues
            , div [ class "growth-charts" ]
                charts
            , div [ class "growth-photos" ] <|
                viewPhotos language child photoValues
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


viewNutritionSigns : Language -> Person -> List ( NominalDate, EverySet ChildNutritionSign ) -> List (Html any)
viewNutritionSigns language child measurements =
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


viewPhotos : Language -> Person -> List { a | dateMeasured : NominalDate, value : PhotoUrl } -> List (Html any)
viewPhotos language child measurements =
    let
        viewPhotoUrl (PhotoUrl url) =
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
                    , viewPhotoUrl photo.value
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
    -> ModelIndexedDb
    -> ( PersonId, Person )
    -> Html any
viewNCDAScorecard language currentDate db ( childId, child ) =
    let
        reportData =
            assembleProgresReportData childId db

        allNCDAQuestionnaires =
            List.map
                (\ncda ->
                    ( ncda.dateMeasured, ncda.value )
                )
                groupNCDAs
                ++ List.map
                    (\ncda ->
                        ( ncda.dateMeasured, ncda.value )
                    )
                    nutritionNCDAs
                ++ List.map
                    (\ncda ->
                        ( ncda.dateMeasured, ncda.value )
                    )
                    wellChildNCDAs

        recentQuestionnaire =
            List.head allNCDAQuestionnaires
                |> Maybe.map Tuple.second

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

        questionnairesByAgeInMonths =
            distributeByAgeInMonths child allNCDAQuestionnaires
    in
    div [ class "ui report unstackable items" ]
        [ viewChildIdentificationPane language currentDate recentQuestionnaire db ( childId, child )
        , viewANCNewbornPane language currentDate db child
        , viewUniversalInterventionsPane language
            currentDate
            child
            db
            questionnairesByAgeInMonths
            reportData.maybeAssembled
        , viewNutritionBehaviorPane language currentDate child questionnairesByAgeInMonths
        , viewTargetedInterventionsPane language
            currentDate
            child
            db
            questionnairesByAgeInMonths
            reportData.groupNutritionMeasurements
            reportData.individualNutritionMeasurementsWithDates
            reportData.individualWellChildMeasurementsWithDates
            reportData.acuteIllnesses
        , viewInfrastructureEnvironmentWashPane language currentDate child questionnairesByAgeInMonths
        ]


viewChildIdentificationPane :
    Language
    -> NominalDate
    -> Maybe NCDAValue
    -> ModelIndexedDb
    -> ( PersonId, Person )
    -> Html any
viewChildIdentificationPane language currentDate ncdaQuestionnaire db ( childId, child ) =
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

        ( mother, father ) =
            let
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
                    [ viewEntry Translate.FatherOrCheifName person.name
                    , viewEntry Translate.FatherOrCheifId (Maybe.withDefault "" person.nationalIdNumber)
                    ]
                )
                father
                |> Maybe.withDefault []

        childNameEntry =
            viewEntry Translate.ChildName child.name

        questionnaireEntries =
            Maybe.map
                (\questionnaire ->
                    let
                        bornUnderweight =
                            EverySet.member NCDABornUnderweight questionnaire

                        bornUnderweightAnswer =
                            if bornUnderweight then
                                Translate.Yes

                            else
                                Translate.No

                        birthDefect =
                            EverySet.member NCDABornWithBirthDefect questionnaire

                        birthDefectAnswer =
                            if birthDefect then
                                Translate.Yes

                            else
                                Translate.No
                    in
                    [ viewEntry Translate.BornUnderweight (translate language bornUnderweightAnswer)
                    , viewEntry Translate.BirthDefect (translate language birthDefectAnswer)
                    ]
                )
                ncdaQuestionnaire
                |> Maybe.withDefault []

        genderEntry =
            viewEntry Translate.GenderLabel (translate language <| Translate.Gender child.gender)

        ubudeheEntry =
            Maybe.map (Translate.UbudeheNumber >> translate language >> viewEntry Translate.UbudeheLabel) child.ubudehe
                |> Maybe.withDefault emptyNode

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
                    ++ questionnaireEntries
                    ++ [ ubudeheEntry ]
            , div [ class "column" ] <|
                motherInfoEntry
                    ++ fatherInfoEntry
            ]
        ]


viewANCNewbornPane :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> Person
    -> Html any
viewANCNewbornPane language currentDate db child =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueEmpty

        zeroToFiveValues =
            List.repeat 6 NCDACellValueDash

        sixToTwentyFourValues =
            List.repeat 19 NCDACellValueDash
    in
    div [ class "pane anc-newborn" ]
        [ viewPaneHeading language Translate.ANCNewborn
        , div [ class "pane-content" ]
            [ viewTableHeader
            , viewTableRow language (Translate.NCDAANCNewbornItemLabel RegularCheckups) pregnancyValues zeroToFiveValues sixToTwentyFourValues
            , viewTableRow language (Translate.NCDAANCNewbornItemLabel IronDuringPregnancy) pregnancyValues zeroToFiveValues sixToTwentyFourValues
            ]
        ]


viewTableHeader : Html any
viewTableHeader =
    div [ class "table-header" ]
        [ div [ class "activity" ] [ text "Activity" ]
        , div [ class "flex-column pregnancy" ]
            [ div [ class "column-heading" ] [ text "Pregnancy (1-9)" ]
            , List.repeat 9 ""
                |> List.indexedMap
                    (\index _ ->
                        div [ class "month" ] [ text <| String.fromInt <| index + 1 ]
                    )
                |> div [ class "months" ]
            ]
        , div [ class "flex-column 0-5" ]
            [ div [ class "column-heading" ] [ text "Child (0-5)" ]
            , List.repeat 6 ""
                |> List.indexedMap
                    (\index _ ->
                        div [ class "month" ] [ text <| String.fromInt index ]
                    )
                |> div [ class "months" ]
            ]
        , div [ class "flex-column 6-24" ]
            [ div [ class "column-heading" ] [ text "Child (6-24 months)" ]
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


viewNutritionBehaviorPane :
    Language
    -> NominalDate
    -> Person
    -> Maybe (Dict Int NCDAValue)
    -> Html any
viewNutritionBehaviorPane language currentDate child questionnairesByAgeInMonths =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        breastfedForSixMonthsValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDABreastfedForSixMonths)

        appropriateComplementaryFeedingValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAAppropriateComplementaryFeeding)

        mealsADayValues =
            generateValues currentDate
                child
                questionnairesByAgeInMonths
                (\questionnaire ->
                    List.any (\sign -> EverySet.member sign questionnaire)
                        [ NCDAMealFrequency6to8Months
                        , NCDAMealFrequency9to11Months
                        , NCDAMealFrequency12MonthsOrMore
                        ]
                )

        diverseDietValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAFiveFoodGroups)

        -- Here we are interested only at answer given when child was 6 months old.
        -- For months before that, and after, will show dahses, in case child has
        -- reached the age for which value is given (empty value otherwise).
        ( breastfedForSixMonthsFirstPeriod, breastfedForSixMonthsSecondPeriod ) =
            let
                firstPeriod =
                    List.take 6 breastfedForSixMonthsValues
                        |> List.map setDashIfNotEmpty

                secondPeriod =
                    let
                        generated =
                            List.drop 6 breastfedForSixMonthsValues
                    in
                    List.take 1 generated
                        ++ (List.drop 1 generated
                                |> List.map setDashIfNotEmpty
                           )
            in
            ( firstPeriod, secondPeriod )

        -- Here we are interested only at answer given when child has reached
        -- the age of 7 months.
        -- For prior period we show dahses, in case child has reached
        -- the age for which value is given (empty value otherwise).
        ( appropriateComplementaryFeedingFirstPeriod, appropriateComplementaryFeedingSecondPeriod ) =
            let
                firstPeriod =
                    List.take 6 appropriateComplementaryFeedingValues
                        |> List.map setDashIfNotEmpty

                secondPeriod =
                    let
                        generated =
                            List.drop 6 appropriateComplementaryFeedingValues
                    in
                    (List.take 1 generated
                        |> List.map setDashIfNotEmpty
                    )
                        ++ List.drop 1 generated
            in
            ( firstPeriod, secondPeriod )

        -- generateValues() may generate values at certain periods that are
        -- not relevant, which we want to replace them with dashes.
        -- However, if child has not yeat reach the age of month for which
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
            [ viewTableHeader
            , viewTableRow language
                (Translate.NCDANutritionBehaviorItemLabel BreastfedSixMonths)
                pregnancyValues
                breastfedForSixMonthsFirstPeriod
                breastfedForSixMonthsSecondPeriod
            , viewTableRow language
                (Translate.NCDANutritionBehaviorItemLabel AppropriateComplementaryFeeding)
                pregnancyValues
                appropriateComplementaryFeedingFirstPeriod
                appropriateComplementaryFeedingSecondPeriod
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
viewInfrastructureEnvironmentWashPane language currentDate child questionnairesByAgeInMonths =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        hasToilets =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAHasToilets)

        hasCleanWater =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAHasCleanWater)

        hasHandwashingFacility =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAHasHandwashingFacility)

        hasKitchenGarden =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAHasKitchenGarden)
    in
    div [ class "pane infrastructure-environment-wash" ]
        [ viewPaneHeading language Translate.InfrastructureEnvironmentWash
        , div [ class "pane-content" ]
            [ viewTableHeader
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasToilets)
                pregnancyValues
                (List.take 6 hasToilets)
                (List.drop 6 hasToilets)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasCleanWater)
                pregnancyValues
                (List.take 6 hasCleanWater)
                (List.drop 6 hasCleanWater)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasHandwashingFacility)
                pregnancyValues
                (List.take 6 hasHandwashingFacility)
                (List.drop 6 hasHandwashingFacility)
            , viewTableRow language
                (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasKitchenGarden)
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
    -> ChildMeasurementList
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html any
viewTargetedInterventionsPane language currentDate child db questionnairesByAgeInMonths groupNutritionMeasurements individualNutritionMeasurementsWithDates individualWellChildMeasurementsWithDates acuteIllnesses =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        fbfsByAgeInMonths =
            Dict.values groupNutritionMeasurements.fbfs
                |> List.map
                    (\fbf ->
                        if fbf.value.distributedAmount > 0 then
                            ( fbf.dateMeasured, NCDACellValueV )

                        else
                            ( fbf.dateMeasured, NCDACellValueX )
                    )
                |> distributeByAgeInMonths child

        malnutritionTreatmentsByAgeInMonths =
            groupMalnutritionTreatmentData
                ++ individualMalnutritionTreatmentData
                |> distributeByAgeInMonths child

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
                        (\( participantId, participant ) ->
                            diffMonths birthDate participant.startDate < 24
                        )
                        acuteIllnesses
                        |> List.map
                            (\( participantId, participant ) ->
                                Dict.get participantId db.acuteIllnessEncountersByParticipant
                                    |> Maybe.andThen RemoteData.toMaybe
                                    |> Maybe.map
                                        (Dict.toList
                                            >> List.filterMap
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
                                    |> Maybe.withDefault []
                            )
                        |> List.concat
                        |> distributeByAgeInMonths child
                )
                child.birthDate

        fbfValues =
            generateValues currentDate child fbfsByAgeInMonths ((==) NCDACellValueV)

        malnutritionTreatmentValues =
            generateValues currentDate child malnutritionTreatmentsByAgeInMonths ((==) NCDACellValueV)

        diarrheaTreatmentValues =
            generateValues currentDate child diarrheaTreatmenByAgeInMonths ((==) NCDACellValueV)

        supportChildWithDisabilityValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDASupportChildWithDisability)

        conditionalCashTransferValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAConditionalCashTransfer)

        conditionalFoodItemsValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAConditionalFoodItems)
    in
    div [ class "pane targeted-interventions" ]
        [ viewPaneHeading language Translate.TargetedInterventions
        , div [ class "pane-content" ]
            [ viewTableHeader
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
                (Translate.NCDATargetedInterventionsItemLabel ConditionalFoodItems)
                pregnancyValues
                (List.take 6 conditionalFoodItemsValues)
                (List.drop 6 conditionalFoodItemsValues)
            ]
        ]


viewUniversalInterventionsPane :
    Language
    -> NominalDate
    -> Person
    -> ModelIndexedDb
    -> Maybe (Dict Int NCDAValue)
    -> Maybe AssembledData
    -> Html any
viewUniversalInterventionsPane language currentDate child db questionnairesByAgeInMonths maybeAssembled =
    let
        pregnancyValues =
            List.repeat 9 NCDACellValueDash

        immunizationByAgeInMonths =
            Maybe.andThen
                (\birthDate ->
                    Maybe.map
                        (\assembled ->
                            List.repeat 25 ""
                                |> List.indexedMap
                                    (\index _ ->
                                        let
                                            -- This is the date for last day of month
                                            -- 'index'. For example, for index = 0, this is
                                            -- the last day, before child turns 1 month old.
                                            -- We use it to determine if child was
                                            -- behind on any of vaccines at that month.
                                            referrenceDate =
                                                Date.add Date.Months (index + 1) birthDate
                                                    |> Date.add Date.Days -1

                                            -- Filter out vaccinations that were performed
                                            -- after the referrence date.
                                            vaccinationProgressOnReferrenceDate =
                                                Dict.map
                                                    (\vaccineType dosesDict ->
                                                        Dict.filter
                                                            (\dose administeredDate ->
                                                                Date.compare administeredDate referrenceDate == LT
                                                            )
                                                            dosesDict
                                                    )
                                                    assembled.vaccinationProgress

                                            futureVaccinations =
                                                generateFutureVaccinationsData currentDate child False vaccinationProgressOnReferrenceDate

                                            closestDateForVaccination =
                                                List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinations
                                                    |> List.sortWith Date.compare
                                                    |> List.head
                                        in
                                        Maybe.map
                                            (\closestDate ->
                                                if Date.compare closestDate referrenceDate == GT then
                                                    -- Closest date when vaccine is required is after
                                                    -- current month, which means that att current month
                                                    -- we're not behind on vaccination.
                                                    ( referrenceDate, NCDACellValueV )

                                                else
                                                    ( referrenceDate, NCDACellValueX )
                                            )
                                            closestDateForVaccination
                                            |> Maybe.withDefault
                                                -- This indicates that there're no future vaccinations to be
                                                -- done, and therefore, we're on track at current month.
                                                ( referrenceDate, NCDACellValueV )
                                    )
                        )
                        maybeAssembled
                        |> Maybe.withDefault
                            -- We get here if there were no SPV encounters performed,
                            -- which means that no vaccinations were recorded.
                            -- Therefore, we're for sure behind on vaccinations
                            -- for any given month.
                            (List.repeat 25 ""
                                |> List.indexedMap
                                    (\index _ ->
                                        ( Date.add Date.Months (index + 1) birthDate
                                            |> Date.add Date.Days -1
                                        , NCDACellValueX
                                        )
                                    )
                            )
                        |> distributeByAgeInMonths child
                )
                child.birthDate

        immunizationValues =
            generateValues currentDate child immunizationByAgeInMonths ((==) NCDACellValueV)

        _ =
            Debug.log "immunizationByAgeInMonths" immunizationByAgeInMonths

        ongeraMNPValues =
            generateValues currentDate child questionnairesByAgeInMonths (EverySet.member NCDAOngeraMNP)
    in
    div [ class "pane universal-interventions" ]
        [ viewPaneHeading language Translate.UniversalInterventions
        , div [ class "pane-content" ]
            [ viewTableHeader
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel OngeraMNP)
                pregnancyValues
                (List.take 6 ongeraMNPValues)
                (List.drop 6 ongeraMNPValues)
            , viewTableRow language
                (Translate.NCDAUniversalInterventionsItemLabel Immunization)
                pregnancyValues
                (List.take 6 immunizationValues)
                (List.drop 6 immunizationValues)
            ]
        ]


distributeByAgeInMonths : Person -> List ( NominalDate, a ) -> Maybe (Dict Int a)
distributeByAgeInMonths child values =
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
                            |> Maybe.withDefault (Dict.insert ageMonths value accum)
                    )
                    Dict.empty
        )
        child.birthDate


generateValues : NominalDate -> Person -> Maybe (Dict Int a) -> (a -> Bool) -> List NCDACellValue
generateValues currentDate child valuesByAgeInMonths resolutionFunc =
    let
        emptyValues =
            List.repeat 25 NCDACellValueEmpty
    in
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
                emptyValues
        )
        valuesByAgeInMonths
        (ageInMonths currentDate child)
        |> Maybe.withDefault emptyValues
