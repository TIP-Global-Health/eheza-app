module Pages.WellChildProgressReport.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant, sortEncounterTuplesDesc, sortTuplesByDateDesc)
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
import Pages.NutritionEncounter.Utils
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterDialog)
import Pages.WellChildActivity.Model
import Pages.WellChildActivity.Utils exposing (generateFutureVaccinationsData)
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


view : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id db model) identity assembled


viewContent : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id db model assembled =
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
            , viewDiagnosisPane language currentDate id db model assembled
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


viewDiagnosisPane : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewDiagnosisPane language currentDate id db model assembled =
    let
        individualParticipants =
            Dict.get assembled.participant.person db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList

        ( activeIllnesses, completedIllnesses ) =
            Maybe.map
                (List.filter
                    (\( sessionId, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                    )
                )
                individualParticipants
                |> Maybe.withDefault []
                |> List.partition (Tuple.second >> isAcuteIllnessActive currentDate)

        individualNutritionMeasurements =
            Maybe.andThen
                (List.filter
                    (\( sessionId, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                    )
                    >> List.head
                )
                individualParticipants
                |> Maybe.map
                    (\( participantId, _ ) ->
                        Pages.NutritionEncounter.Utils.generatePreviousMeasurements Nothing participantId db
                    )
                |> Maybe.withDefault []

        _ =
            Debug.log "groupNutritionAssessments" groupNutritionAssessments

        groupNutritionMeasurements =
            Dict.get assembled.participant.person db.childMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        groupNutritionAssessments =
            Maybe.map
                (\measurements ->
                    let
                        followUps =
                            Dict.values measurements.followUp

                        nutritions =
                            Dict.values measurements.nutritions
                                |> List.map
                                    (\nutrition ->
                                        ( nutrition.participantId, nutrition.value )
                                    )
                                |> Dict.fromList
                    in
                    Dict.values measurements.followUp
                        |> List.filterMap
                            (\followUp ->
                                Dict.get followUp.participantId nutritions
                                    |> Maybe.map
                                        (\nutritionValue ->
                                            ( followUp.dateMeasured, followUp.value.assesment, nutritionValue )
                                        )
                            )
                )
                groupNutritionMeasurements

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

        ( selectedDiagnosisEntries, selectedWarningEntries ) =
            case model.diagnosisMode of
                ModeActiveDiagnosis ->
                    ( activeIllnesses
                    , activeWarnings
                    )

                ModeCompletedDiagnosis ->
                    ( completedIllnesses
                    , completedWarnings
                    )

        ( activeWarnings, completedWarnings ) =
            generatePartitionedWarnings db assembled

        entries =
            (daignosisEntries ++ warningEntries)
                |> List.sortWith sortTuplesByDateDesc
                |> List.map Tuple.second

        daignosisEntries =
            List.map (Tuple.first >> viewDaignosisEntry language id db) selectedDiagnosisEntries
                |> Maybe.Extra.values

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


viewDaignosisEntry : Language -> WellChildEncounterId -> ModelIndexedDb -> IndividualEncounterParticipantId -> Maybe ( NominalDate, Html Msg )
viewDaignosisEntry language id db participantId =
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


generatePartitionedWarnings :
    ModelIndexedDb
    -> AssembledData
    ->
        ( List ( NominalDate, WellChildEncounterType, EncounterWarning )
        , List ( NominalDate, WellChildEncounterType, EncounterWarning )
        )
generatePartitionedWarnings db assembled =
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


viewWarningEntry : Language -> ( NominalDate, WellChildEncounterType, EncounterWarning ) -> ( NominalDate, Html Msg )
viewWarningEntry language ( date, encounterType, warning ) =
    ( date
    , div [ class "entry diagnosis" ]
        [ div [ class "cell assesment" ] [ text <| translate language <| Translate.EncounterWarningForDiagnosisPane warning ]
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
