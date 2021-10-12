module Pages.WellChildProgressReport.View exposing (view, viewNutritionSigns, viewPaneHeading, viewPersonInfoPane, viewProgressReport)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementDateMeasuredFunc, getMeasurementValueFunc, muacIndication)
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
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, getHealthCenterName, graduatingAgeInMonth, isChildUnderAgeOf5, isPersonAnAdult)
import Backend.Session.Model exposing (Session)
import Backend.WellChildEncounter.Model
    exposing
        ( EncounterWarning(..)
        , PediatricCareMilestone
        , WellChildEncounter
        , WellChildEncounterType(..)
        , ecdMilestoneWarnings
        , headCircumferenceWarnings
        , pediatricCareMilestones
        )
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, diffWeeks, formatDDMMYY, formatDDMMyyyy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isNothing)
import Measurement.View exposing (renderDatePart, viewActionTakenLabel)
import Pages.AcuteIllnessEncounter.Utils
    exposing
        ( getAcuteIllnessDiagnosisForEncounters
        , getAcuteIllnessDiagnosisForParticipant
        , getAcuteIllnessEncountersForParticipant
        )
import Pages.AcuteIllnessParticipant.Utils exposing (isAcuteIllnessActive)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.NutritionActivity.View exposing (translateNutritionAssement)
import Pages.NutritionEncounter.Utils
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterDialog)
import Pages.WellChildActivity.Model exposing (VaccinationStatus(..))
import Pages.WellChildActivity.Utils exposing (getPreviousMeasurements, mandatoryNutritionAssessmentTasksCompleted)
import Pages.WellChildActivity.View exposing (viewVaccinationOverview)
import Pages.WellChildEncounter.Model exposing (AssembledData, VaccinationProgressDict)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData, pediatricCareMilestoneToComparable, resolvePediatricCareMilestoneOnDate)
import Pages.WellChildEncounter.View exposing (viewPersonDetails)
import Pages.WellChildProgressReport.Model exposing (..)
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

        initiator =
            InitiatorWellChild id

        mandatoryNutritionAssessmentMeasurementsTaken =
            generateAssembledData id db
                |> RemoteData.toMaybe
                |> Maybe.map (\assembled -> mandatoryNutritionAssessmentTasksCompleted currentDate isChw assembled db)
                |> Maybe.withDefault False
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
    -> ( PersonId, Person )
    -> Html msg
viewProgressReport language currentDate zscores isChw initiator mandatoryNutritionAssessmentMeasurementsTaken db diagnosisMode setActivePageMsg setDiagnosisModeMsg ( childId, child ) =
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

        groupNutritionMeasurements =
            Dict.get childId db.childMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.withDefault emptyChildMeasurementList

        acuteIllnesses =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                )
                individualParticipants

        individualNutritionParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        individualNutritionMeasurementsWithDates =
            Maybe.map
                (\participantId ->
                    Pages.NutritionEncounter.Utils.generatePreviousMeasurements Nothing participantId db
                )
                individualNutritionParticipantId
                |> Maybe.withDefault []

        wellChildEncounters =
            Maybe.map
                (\participantId ->
                    getWellChildEncountersForParticipant db participantId
                )
                individualWellChildParticipantId
                |> Maybe.withDefault []

        individualWellChildMeasurementsWithDates =
            Maybe.map
                (\assembled ->
                    ( assembled.encounter.startDate, ( assembled.id, assembled.measurements ) )
                        :: assembled.previousMeasurementsWithDates
                )
                maybeAssembled
                |> Maybe.withDefault []

        individualWellChildMeasurements =
            getPreviousMeasurements individualWellChildMeasurementsWithDates

        vaccinationProgress =
            Maybe.map .vaccinationProgress maybeAssembled
                |> Maybe.withDefault Dict.empty

        derrivedContent =
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
                        wellChildEncounters
                        db
                    , viewGrowthPane language
                        currentDate
                        zscores
                        ( childId, child )
                        expectedSessions
                        groupNutritionMeasurements
                        individualNutritionMeasurementsWithDates
                        individualWellChildMeasurementsWithDates
                    , viewNextAppointmentPane language
                        currentDate
                        child
                        individualWellChildMeasurements
                        db
                    ]

                ModeCompletedDiagnosis ->
                    []
    in
    div [ class "page-report well-child" ]
        [ viewHeader language initiator diagnosisMode setActivePageMsg setDiagnosisModeMsg
        , div [ class "ui report unstackable items" ] <|
            [ viewPersonInfoPane language currentDate child
            , viewDiagnosisPane language
                currentDate
                isChw
                initiator
                mandatoryNutritionAssessmentMeasurementsTaken
                acuteIllnesses
                individualNutritionParticipantId
                wellChildEncounters
                groupNutritionMeasurements
                (getPreviousMeasurements individualNutritionMeasurementsWithDates)
                individualWellChildMeasurements
                db
                diagnosisMode
                setActivePageMsg
                setDiagnosisModeMsg
                maybeAssembled
            ]
                ++ derrivedContent

        -- , viewModal endEncounterDialog
        ]


viewHeader : Language -> WellChildProgressReportInitiator -> DiagnosisMode -> (Page -> msg) -> (DiagnosisMode -> msg) -> Html msg
viewHeader language initiator diagnosisMode setActivePageMsg setDiagnosisModeMsg =
    let
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
                    in
                    setActivePageMsg targetPage

                ModeCompletedDiagnosis ->
                    setDiagnosisModeMsg ModeActiveDiagnosis
    in
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language <| Translate.ProgressReport
            ]
        , span
            [ class "link-back"
            , onClick goBackAction
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPersonInfoPane : Language -> NominalDate -> Person -> Html any
viewPersonInfoPane language currentDate person =
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div [ class "patient-info" ] <|
            viewPersonDetails language currentDate person
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
            List.map (Tuple.first >> viewAcuteIllnessDiagnosisEntry language initiator db setActivePageMsg) selectedDiagnosisEntries
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
    ->
        ( List ( NominalDate, PediatricCareMilestone, EncounterWarning )
        , List ( NominalDate, PediatricCareMilestone, EncounterWarning )
        )
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
                                                    Maybe.andThen (resolvePediatricCareMilestoneOnDate encounter.startDate) assembled.person.birthDate
                                                        |> Maybe.map
                                                            (\ecdMilestone ->
                                                                ( encounter.startDate, ecdMilestone, warning )
                                                            )
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


viewAcuteIllnessDiagnosisEntry :
    Language
    -> WellChildProgressReportInitiator
    -> ModelIndexedDb
    -> (Page -> msg)
    -> ( IndividualEncounterParticipantId, DiagnosisEntryStatus )
    -> Maybe ( NominalDate, Html msg )
viewAcuteIllnessDiagnosisEntry language initiator db setActivePageMsg ( participantId, status ) =
    let
        encounters =
            getAcuteIllnessEncountersForParticipant db participantId

        maybeLastEncounterId =
            List.head encounters
                |> Maybe.map Tuple.first

        diagnosisData =
            getAcuteIllnessDiagnosisForEncounters encounters
    in
    Maybe.map2
        (\( date, diagnosis ) lastEncounterId ->
            let
                acuteIllnessProgressReportInitiator =
                    case initiator of
                        InitiatorNutritionIndividual nutritionEncounterId ->
                            InitiatorIndividualNutritionProgressReport nutritionEncounterId

                        InitiatorWellChild wellChildEncounterId ->
                            InitiatorWellChildProgressReport wellChildEncounterId

                        InitiatorNutritionGroup sessionId personId ->
                            InitiatorGroupNutritionProgressReport sessionId personId
            in
            ( date
            , div [ class "entry diagnosis" ]
                [ div [ class "cell assesment" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
                , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
                    [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
                , div [ class "cell date" ] [ text <| formatDDMMYY date ]
                , div
                    [ class "icon-forward"
                    , onClick <|
                        setActivePageMsg <|
                            UserPage <|
                                AcuteIllnessProgressReportPage
                                    acuteIllnessProgressReportInitiator
                                    lastEncounterId
                    ]
                    []
                ]
            )
        )
        diagnosisData
        maybeLastEncounterId


diagnosisEntryStatusToString : DiagnosisEntryStatus -> String
diagnosisEntryStatusToString status =
    case status of
        StatusOngoing ->
            "ongoing"

        StatusResolved ->
            "resolved"


viewNutritionAssessmentEntry : Language -> ( NominalDate, ( List NutritionAssessment, DiagnosisEntryStatus ) ) -> ( NominalDate, Html any )
viewNutritionAssessmentEntry language ( date, ( assessments, status ) ) =
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] <|
            List.map (translateNutritionAssement language >> List.singleton >> p []) assessments
        , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
            [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
        , div [ class "cell date" ] [ text <| formatDDMMYY date ]
        ]
    )


viewWarningEntry : Language -> ( NominalDate, ( PediatricCareMilestone, EncounterWarning, DiagnosisEntryStatus ) ) -> ( NominalDate, Html any )
viewWarningEntry language ( date, ( milestone, warning, status ) ) =
    let
        milestoneForDaignosisPane =
            translate language <| Translate.WellChildECDMilestoneForDiagnosisPane milestone
    in
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ text <| translate language <| Translate.EncounterWarningForDiagnosisPane warning milestoneForDaignosisPane ]
        , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
            [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
        , div [ class "cell date" ] [ text <| formatDDMMYY date ]
        ]
    )


viewVaccinationHistoryPane : Language -> NominalDate -> Person -> VaccinationProgressDict -> ModelIndexedDb -> Html any
viewVaccinationHistoryPane language currentDate child vaccinationProgress db =
    div [ class "pane vaccination-history" ] <|
        [ viewPaneHeading language Translate.ImmunisationHistory
        , div [ class "pane-content" ] <|
            viewVaccinationOverview language currentDate child vaccinationProgress db
        ]


viewECDPane : Language -> NominalDate -> Person -> List ( WellChildEncounterId, WellChildEncounter ) -> ModelIndexedDb -> Html any
viewECDPane language currentDate child wellChildEncounters db =
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
                                        |> Maybe.withDefault NoECDStatus
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
                    -- childern with age bellow 13 weeks.
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
                                        |> String.join ", "
                                        |> text
                                        |> List.singleton
                                        |> div [ class "cell name" ]
                                    , div [ class "cell date" ]
                                        [ text <| formatDDMMYY dateMeasured ]
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
                        [ text <| formatDDMMYY photo.dateMeasured ]
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
                                    , div [ class "cell date" ] [ text <| formatDDMMYY date ]
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


viewEntries : Language -> List (Html any) -> List (Html any)
viewEntries language entries =
    if List.isEmpty entries then
        [ div [ class "entry no-matches" ] [ text <| translate language Translate.NoMatchesFound ] ]

    else
        entries
