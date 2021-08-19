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
import Backend.Person.Utils exposing (ageInMonths, ageInYears, graduatingAgeInMonth, isChildUnderAgeOf5, isPersonAnAdult)
import Backend.Session.Model exposing (Session)
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
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), Length(..), ZScore)
import ZScore.Utils exposing (zScoreLengthHeightForAge, zScoreWeightForAge)
import ZScore.View


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 140
    , height = 140
    }


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate zscores id isChw db model) identity assembled


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw db model assembled =
    let
        childId =
            assembled.participant.person

        expectedSessions =
            Dict.get childId db.expectedSessions
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.withDefault Dict.empty

        groupNutritionMeasurements =
            Dict.get childId db.childMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.withDefault emptyChildMeasurementList

        individualParticipants =
            Dict.get childId db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        acuteIllnesses =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                )
                individualParticipants

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
                )
                individualNutritionParticipant
                |> Maybe.withDefault []

        individualWellChildMeasurements =
            ( currentDate, ( id, assembled.measurements ) )
                :: assembled.previousMeasurementsWithDates

        derrivedContent =
            case model.diagnosisMode of
                ModeActiveDiagnosis ->
                    [ viewVaccinationHistoryPane language currentDate id db model assembled
                    , viewGrowthPane language
                        currentDate
                        zscores
                        ( childId, assembled.person )
                        expectedSessions
                        groupNutritionMeasurements
                        individualNutritionMeasurements
                        individualWellChildMeasurements
                    ]

                ModeCompletedDiagnosis ->
                    []
    in
    div [ class "page-report well-child" ]
        [ viewHeader language id model
        , div [ class "ui report unstackable items" ] <|
            [ viewPersonInfoPane language currentDate assembled.person
            , viewDiagnosisPane language
                currentDate
                id
                isChw
                acuteIllnesses
                individualNutritionParticipant
                groupNutritionMeasurements
                (getPreviousMeasurements individualNutritionMeasurements)
                (getPreviousMeasurements individualWellChildMeasurements)
                db
                model
                assembled
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


viewDiagnosisPane :
    Language
    -> NominalDate
    -> WellChildEncounterId
    -> Bool
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Maybe IndividualEncounterParticipantId
    -> ChildMeasurementList
    -> List NutritionMeasurements
    -> List WellChildMeasurements
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewDiagnosisPane language currentDate id isChw acuteIllnesses individualNutritionParticipantId groupNutritionMeasurements individualNutritionMeasurements individuaWellChild db model assembled =
    let
        ( activeIllnesses, completedIllnesses ) =
            List.partition (Tuple.second >> isAcuteIllnessActive currentDate) acuteIllnesses

        groupNutritionEntries =
            generateGroupNutritionAssessmentEntries groupNutritionMeasurements

        individualNutritionEntries =
            generateIndividualNutritionAssessmentEntries individualNutritionMeasurements

        individuaWellChildEntries =
            generateIndividualNutritionAssessmentEntries individuaWellChild

        allNutritionAssessmentEntries =
            individualNutritionEntries ++ groupNutritionEntries ++ individuaWellChildEntries

        ( activeWarningEntries, completedWarningEntries ) =
            generatePartitionedWarningEntries db assembled

        ( activeAssessmentEntries, completedAssessmentEntries ) =
            resolveDateOfLastNutritionAssessment currentDate isChw individualNutritionParticipantId groupNutritionMeasurements assembled db
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
                    ( List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activeIllnesses
                    , List.map (\( date, data ) -> ( date, ( data, StatusOngoing ) )) activeAssessmentEntries
                    , List.map (\( date, encounterType, data ) -> ( date, ( encounterType, data, StatusOngoing ) )) activeWarningEntries
                    )

                ModeCompletedDiagnosis ->
                    ( List.map (\( participantId, data ) -> ( ( participantId, StatusResolved ), data )) completedIllnesses
                    , List.map (\( date, data ) -> ( date, ( data, StatusResolved ) )) completedAssessmentEntries
                    , List.map (\( date, encounterType, data ) -> ( date, ( encounterType, data, StatusResolved ) )) completedWarningEntries
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
                (\( dateMeasured, ( assessments, status ) ) ->
                    List.map (\assessment -> ( dateMeasured, ( assessment, status ) )) assessments
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


generateGroupNutritionAssessmentEntries : ChildMeasurementList -> List ( NominalDate, List NutritionAssessment )
generateGroupNutritionAssessmentEntries measurementList =
    Dict.values measurementList.nutritions
        |> List.filterMap
            (\nutrition -> filterNutritionAssessments nutrition.dateMeasured nutrition.value)


resolveDateOfLastNutritionAssessment :
    NominalDate
    -> Bool
    -> Maybe IndividualEncounterParticipantId
    -> ChildMeasurementList
    -> AssembledData
    -> ModelIndexedDb
    -> Maybe NominalDate
resolveDateOfLastNutritionAssessment currentDate isChw individualNutritionParticipant groupNutritionMeasurements assembled db =
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
                Dict.values groupNutritionMeasurements.nutritions
                    |> List.map .dateMeasured
                    |> List.sortWith sortDatesDesc
                    |> List.head

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


viewAcuteIllnessDaignosisEntry : Language -> WellChildEncounterId -> ModelIndexedDb -> ( IndividualEncounterParticipantId, DiagnosisEntryStatus ) -> Maybe ( NominalDate, Html Msg )
viewAcuteIllnessDaignosisEntry language id db ( participantId, status ) =
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
                , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
                    [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
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


diagnosisEntryStatusToString : DiagnosisEntryStatus -> String
diagnosisEntryStatusToString status =
    case status of
        StatusOngoing ->
            "ongoing"

        StatusResolved ->
            "resolved"


viewNutritionAssessmentEntry : Language -> ( NominalDate, ( NutritionAssessment, DiagnosisEntryStatus ) ) -> ( NominalDate, Html Msg )
viewNutritionAssessmentEntry language ( date, ( assessment, status ) ) =
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ translateNutritionAssement language assessment ]
        , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
            [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
        , div [ class "cell date" ] [ text <| formatDDMMYY date ]
        ]
    )


viewWarningEntry : Language -> ( NominalDate, ( WellChildEncounterType, EncounterWarning, DiagnosisEntryStatus ) ) -> ( NominalDate, Html Msg )
viewWarningEntry language ( date, ( encounterType, warning, status ) ) =
    let
        encounterTypeForDaignosisPane =
            translate language <| Translate.WellChildEncounterTypeForDiagnosisPane encounterType
    in
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ text <| translate language <| Translate.EncounterWarningForDiagnosisPane warning encounterTypeForDaignosisPane ]
        , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
            [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
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


viewGrowthPane :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> ( PersonId, Person )
    -> Dict SessionId Session
    -> ChildMeasurementList
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> Html Msg
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

        --
        -- COMMON CONTEXT
        --
        sessionsAndEncounters =
            expectedSessions
                ++ expectedIndividualEncounters nutritionMeasurements
                ++ expectedIndividualEncounters wellChildMeasurements
                |> List.sortWith (\s1 s2 -> Date.compare (Tuple.second s1) (Tuple.second s2))
                |> List.reverse

        heightValuesIndexed =
            Dict.union heightValuesBySession heightValuesByEncounter

        muacValuesIndexed =
            Dict.union muacValuesBySession muacValuesByEncounter

        weightValuesIndexed =
            Dict.union weightValuesBySession weightValuesByEncounter

        heightValues =
            Dict.values heightValuesIndexed

        muacValues =
            Dict.values muacValuesIndexed

        weightValues =
            Dict.values weightValuesIndexed

        photoValues =
            Dict.values photoValuesBySession ++ Dict.values photoValuesByEncounter

        photos =
            viewPhotos language child photoValues

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

        weightForLengtAndHeighthData =
            List.filterMap (chartWeightForLengthAndHeight heightValues) weightValues

        weightForLengthData =
            weightForLengtAndHeighthData
                |> List.map (\( length, height, weight ) -> ( length, weight ))

        weightForHeightData =
            weightForLengtAndHeighthData
                |> List.map (\( length, height, weight ) -> ( height, weight ))

        childAgeInMonths =
            case child.birthDate of
                Just birthDate ->
                    diffMonths birthDate currentDate

                Nothing ->
                    0

        charts =
            -- With exception of Sortwathe, children graduate from all
            -- groups at the age of 26 month. Therefore, we will show
            -- 0-2 graph for all children that are less than 26 month old.
            if childAgeInMonths < graduatingAgeInMonth then
                div
                    [ class "image-report" ]
                    [ ZScore.View.viewMarkers
                    , zScoreViewCharts.heightForAge language zscores heightForAgeDaysData
                    , zScoreViewCharts.weightForAge language zscores weightForAgeDaysData
                    , zScoreViewCharts.weightForHeight language zscores weightForLengthData
                    ]

            else if childAgeInMonths < 60 then
                div
                    [ class "image-report" ]
                    [ ZScore.View.viewMarkers
                    , zScoreViewCharts.heightForAge0To5 language zscores heightForAgeDaysData
                    , zScoreViewCharts.weightForAge0To5 language zscores weightForAgeDaysData
                    , zScoreViewCharts.weightForHeight0To5 language zscores weightForHeightData
                    ]

            else
                -- Child is older than 5 years.
                div
                    [ class "image-report" ]
                    [ ZScore.View.viewMarkers
                    , zScoreViewCharts.heightForAge5To19 language zscores heightForAgeMonthsData
                    , zScoreViewCharts.weightForAge5To10 language zscores weightForAgeMonthsData
                    ]
    in
    div [ class "pane growth" ]
        [ viewPaneHeading language Translate.Growth
        , div [ class "pane-content" ]
            [ charts
            , photos
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


viewPhotos : Language -> Person -> List { a | dateMeasured : NominalDate, value : PhotoUrl } -> Html Msg
viewPhotos language child photos =
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
    photos
        |> List.sortWith (\m1 m2 -> Date.compare m1.dateMeasured m2.dateMeasured)
        |> List.map
            (\photo ->
                div
                    [ class "report card" ]
                    [ div
                        [ class "content" ]
                        [ child.birthDate
                            |> Maybe.map (\birthDate -> text <| renderAgeMonthsDays language birthDate photo.dateMeasured)
                            |> Maybe.withDefault emptyNode
                        ]
                    , viewPhotoUrl photo.value
                    ]
            )
        |> div [ class "ui five report cards" ]


viewPaneHeading : Language -> TranslationId -> Html Msg
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]
