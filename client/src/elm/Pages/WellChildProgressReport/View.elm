module Pages.WellChildProgressReport.View exposing (view)

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
        , sortDatesDesc
        , sortEncounterTuplesDesc
        , sortTuplesByDateDesc
        )
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, isChildUnderAgeOf5, isPersonAnAdult)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), WellChildEncounterType(..), ecdMilestoneWarnings, headCircumferenceWarnings)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, formatDDMMYY, formatDDMMyyyy)
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
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterDialog)
import Pages.WellChildActivity.Model
import Pages.WellChildActivity.Utils exposing (generateFutureVaccinationsData, getPreviousMeasurements, mandatoryNutritionAssessmentTasksCompleted)
import Pages.WellChildEncounter.Model exposing (AssembledData)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData)
import Pages.WellChildProgressReport.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderDate)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 140
    , height = 140
    }


view : Language -> NominalDate -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id isChw db model) identity assembled


viewContent : Language -> NominalDate -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id isChw db model assembled =
    let
        derrivedContent =
            case model.diagnosisMode of
                ModeActiveDiagnosis ->
                    [ viewVaccinationHistoryPane language currentDate id db model assembled ]

                ModeCompletedDiagnosis ->
                    []
    in
    div [ class "page-report well-child" ]
        [ viewHeader language id model
        , div [ class "ui report unstackable items" ] <|
            [ viewPersonInfoPane language currentDate assembled.person
            , viewDiagnosisPane language currentDate id isChw db model assembled
            ]
                ++ derrivedContent

        -- , viewModal endEncounterDialog
        ]


viewHeader : Language -> WellChildEncounterId -> Model -> Html Msg
viewHeader language id model =
    let
        goBackAction =
            case model.diagnosisMode of
                ModeActiveDiagnosis ->
                    SetActivePage (UserPage (WellChildEncounterPage id))

                ModeCompletedDiagnosis ->
                    SetDiagnosisMode ModeActiveDiagnosis
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


viewPersonInfoPane : Language -> NominalDate -> Person -> Html Msg
viewPersonInfoPane language currentDate person =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )

        viewAge =
            maybeAge
                |> Maybe.map
                    (\age ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ": " ]
                            , span [] [ text age ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        viewVillage =
            person.village
                |> Maybe.map
                    (\village ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.Village ++ ": " ]
                            , span [] [ text village ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div
            [ class "patient-info" ]
            [ div [ class "ui image" ]
                [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
            , div [ class "details" ]
                [ h2 [ class "ui header" ]
                    [ text person.name ]
                , viewAge
                , viewVillage
                ]
            ]
        ]


viewDiagnosisPane : Language -> NominalDate -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewDiagnosisPane language currentDate id isChw db model assembled =
    let
        individualParticipants =
            Dict.get assembled.participant.person db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        ( activeIllnesses, completedIllnesses ) =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                )
                individualParticipants
                |> List.partition (Tuple.second >> isAcuteIllnessActive currentDate)

        individualNutritionParticipant =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        individualNutritionMeasurements =
            Maybe.map
                (\participantId ->
                    Pages.NutritionEncounter.Utils.generatePreviousMeasurements Nothing participantId db
                        |> getPreviousMeasurements
                )
                individualNutritionParticipant
                |> Maybe.withDefault []

        groupNutritionMeasurements =
            Dict.get assembled.participant.person db.childMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        individualNutritionEntries =
            generateIndividualNutritionAssessmentEntries individualNutritionMeasurements

        groupNutritionEntries =
            generateGroupNutritionAssessmentEntries groupNutritionMeasurements

        individuaWellChildEntries =
            assembled.measurements
                :: getPreviousMeasurements assembled.previousMeasurementsWithDates
                |> generateIndividualNutritionAssessmentEntries

        allNutritionAssessmentEntries =
            individualNutritionEntries ++ groupNutritionEntries ++ individuaWellChildEntries

        ( activeWarningEntries, completedWarningEntries ) =
            generatePartitionedWarningEntries db assembled

        ( activeAssessmentEntries, completedAssessmentEntries ) =
            resolveDateOfLastNutritionAssessment currentDate isChw individualNutritionParticipant groupNutritionMeasurements assembled db
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
                , div [ class "date" ] [ text <| translate language Translate.DiagnosisDate ]
                , div [ class "see-more" ] [ text <| translate language Translate.SeeMore ]
                ]

        ( label, priorDiagniosisButton ) =
            case model.diagnosisMode of
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

        ( selectedDiagnosisEntries, selectedAssessmentEntries, selectedWarningEntries ) =
            case model.diagnosisMode of
                ModeActiveDiagnosis ->
                    ( activeIllnesses
                    , activeAssessmentEntries
                    , activeWarningEntries
                    )

                ModeCompletedDiagnosis ->
                    ( completedIllnesses
                    , completedAssessmentEntries
                    , completedWarningEntries
                    )

        entries =
            (daignosisEntries ++ assessmentEntries ++ warningEntries)
                |> List.sortWith sortTuplesByDateDesc
                |> List.map Tuple.second

        daignosisEntries =
            List.map (Tuple.first >> viewAcuteIllnessDaignosisEntry language id db) selectedDiagnosisEntries
                |> Maybe.Extra.values

        assessmentEntries =
            List.map
                (\( dateMeasured, assessments ) ->
                    List.map (\assessment -> ( dateMeasured, assessment )) assessments
                )
                selectedAssessmentEntries
                |> List.concat
                |> List.map (viewNutritionAssessmentEntry language)

        warningEntries =
            List.map (viewWarningEntry language) selectedWarningEntries
    in
    div [ class "pane diagnosis" ]
        [ viewPaneHeading language label
        , div [ class "pane-content" ] <|
            entriesHeading
                :: entries
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

                            AssesmentDangerSignsNotPresent ->
                                Nothing

                            AssesmentDangerSignsPresent ->
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


generateGroupNutritionAssessmentEntries : Maybe ChildMeasurementList -> List ( NominalDate, List NutritionAssessment )
generateGroupNutritionAssessmentEntries measurementList =
    Maybe.map
        (\measurements ->
            Dict.values measurements.nutritions
                |> List.filterMap
                    (\nutrition -> filterNutritionAssessments nutrition.dateMeasured nutrition.value)
        )
        measurementList
        |> Maybe.withDefault []


resolveDateOfLastNutritionAssessment :
    NominalDate
    -> Bool
    -> Maybe IndividualEncounterParticipantId
    -> Maybe ChildMeasurementList
    -> AssembledData
    -> ModelIndexedDb
    -> Maybe NominalDate
resolveDateOfLastNutritionAssessment currentDate isChw individualNutritionParticipant childGroupMeasurements assembled db =
    if mandatoryNutritionAssessmentTasksCompleted currentDate isChw assembled db then
        Just currentDate

    else
        let
            lastAssessmentDatePerIndividualNutrition =
                Maybe.andThen
                    (\participantId ->
                        getNutritionEncountersForParticipant db participantId
                            -- Sort DESC
                            |> List.sortWith sortEncounterTuplesDesc
                            |> List.head
                            |> Maybe.map (Tuple.second >> .startDate)
                    )
                    individualNutritionParticipant

            lastAssessmentDatePerGroupNutrition =
                Maybe.andThen
                    (\measurements ->
                        Dict.values measurements.nutritions
                            |> List.map .dateMeasured
                            |> List.sortWith sortDatesDesc
                            |> List.head
                    )
                    childGroupMeasurements

            lastAssessmentDatePerWellChild =
                getWellChildEncountersForParticipant db assembled.encounter.participant
                    -- Sort DESC
                    |> List.sortWith sortEncounterTuplesDesc
                    |> List.head
                    |> Maybe.map (Tuple.second >> .startDate)
        in
        [ lastAssessmentDatePerIndividualNutrition, lastAssessmentDatePerGroupNutrition, lastAssessmentDatePerWellChild ]
            |> Maybe.Extra.values
            |> List.sortWith sortDatesDesc
            |> List.head


generatePartitionedWarningEntries :
    ModelIndexedDb
    -> AssembledData
    ->
        ( List ( NominalDate, WellChildEncounterType, EncounterWarning )
        , List ( NominalDate, WellChildEncounterType, EncounterWarning )
        )
generatePartitionedWarningEntries db assembled =
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
                                |> List.filter
                                    (\warning ->
                                        not (List.member warning [ NoECDMilstoneWarning, NoHeadCircumferenceWarning, NoEncounterWarnings ])
                                    )
                    in
                    if List.isEmpty warnings then
                        Nothing

                    else
                        Just <| List.map (\warning -> ( encounter.startDate, encounter.encounterType, warning )) warnings
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


viewAcuteIllnessDaignosisEntry : Language -> WellChildEncounterId -> ModelIndexedDb -> IndividualEncounterParticipantId -> Maybe ( NominalDate, Html Msg )
viewAcuteIllnessDaignosisEntry language id db participantId =
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
            ( date
            , div [ class "entry diagnosis" ]
                [ div [ class "cell assesment" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
                , div [ class "cell date" ] [ text <| formatDDMMYY date ]
                , div
                    [ class "icon-forward"
                    , onClick <|
                        SetActivePage <|
                            UserPage <|
                                AcuteIllnessProgressReportPage
                                    (InitiatorWellChildProgressReport id)
                                    lastEncounterId
                    ]
                    []
                ]
            )
        )
        diagnosisData
        maybeLastEncounterId


viewNutritionAssessmentEntry : Language -> ( NominalDate, NutritionAssessment ) -> ( NominalDate, Html Msg )
viewNutritionAssessmentEntry language ( date, assessment ) =
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ translateNutritionAssement language assessment ]
        , div [ class "cell date" ] [ text <| formatDDMMYY date ]
        ]
    )


viewWarningEntry : Language -> ( NominalDate, WellChildEncounterType, EncounterWarning ) -> ( NominalDate, Html Msg )
viewWarningEntry language ( date, encounterType, warning ) =
    let
        encounterTypeForDaignosisPane =
            translate language <| Translate.WellChildEncounterTypeForDiagnosisPane encounterType
    in
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ text <| translate language <| Translate.EncounterWarningForDiagnosisPane warning encounterTypeForDaignosisPane ]
        , div [ class "cell date" ] [ text <| formatDDMMYY date ]
        ]
    )


viewVaccinationHistoryPane : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewVaccinationHistoryPane language currentDate id db model assembled =
    let
        entriesHeading =
            div [ class "heading vaccination" ]
                [ div [ class "name" ] [ text <| translate language Translate.Immunisation ]
                , div [ class "date" ] [ text <| translate language Translate.DateReceived ]
                , div [ class "next-due" ] [ text <| translate language Translate.NextDue ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                ]

        futureVaccinationsData =
            generateFutureVaccinationsData currentDate assembled.person False assembled.vaccinationProgress
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
                    Maybe.map formatDDMMYY nextDue
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
                        |> Maybe.withDefault ( StatusDone, "done" )
            in
            div [ class "entry vaccination" ]
                [ div [ class "cell name" ] [ text <| translate language <| Translate.VaccineType vaccineType ]
                , Dict.values doses
                    |> List.sortWith Date.compare
                    |> List.map (formatDDMMYY >> text >> List.singleton >> p [])
                    |> div [ class "cell date" ]
                , div [ class "cell next-due" ]
                    [ text nextDueText ]
                , div [ class <| "cell status " ++ statusClass ]
                    [ text <| translate language <| Translate.VaccinationStatus status ]
                ]
    in
    div [ class "pane vaccination-history" ] <|
        [ viewPaneHeading language Translate.ImmunisationHistory
        , div [ class "pane-content" ] <|
            entriesHeading
                :: entries
        ]


viewPaneHeading : Language -> TranslationId -> Html Msg
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]
