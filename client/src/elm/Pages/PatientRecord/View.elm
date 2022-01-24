module Pages.PatientRecord.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortByDate, sortTuplesByDateDesc)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInYears, generateFullName, isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllnessParticipant.Utils exposing (isAcuteIllnessActive)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)
import Pages.PrenatalEncounter.Utils exposing (getPrenatalEncountersForParticipant)
import Pages.PrenatalParticipant.Utils exposing (isPregnancyActive)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import Pages.WellChildEncounter.View exposing (thumbnailDimensions)
import Pages.WellChildProgressReport.Model exposing (DiagnosisEntryStatus(..), WellChildProgressReportInitiator(..))
import Pages.WellChildProgressReport.View exposing (diagnosisEntryStatusToString, viewAcuteIllnessDiagnosisEntry, viewEntries, viewPaneHeading, viewProgressReport)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (spinner, thumbnailImage, viewModal)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    Dict.get id db.people
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\person ->
                if isPersonAnAdult currentDate person == Just False then
                    viewContentForChild language currentDate zscores id person isChw db model

                else
                    viewContentForAdult language currentDate id person db model
            )
        |> Maybe.withDefault spinner


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PatientRecord ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PersonsPage Nothing ParticipantDirectoryOrigin
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContentForChild : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> Bool -> ModelIndexedDb -> Model -> Html Msg
viewContentForChild language currentDate zscores childId child isChw db model =
    let
        endEncounterData =
            Just <|
                { showEndEncounterDialog = False
                , allowEndEcounter = False
                , closeEncounterMsg = NoOp
                , setEndEncounterDialogStateMsg = always NoOp
                }

        mandatoryNutritionAssessmentMeasurementsTaken =
            False

        initiator =
            InitiatorPatientRecordChild childId
    in
    viewProgressReport language
        currentDate
        zscores
        isChw
        initiator
        mandatoryNutritionAssessmentMeasurementsTaken
        db
        model.diagnosisMode
        SetActivePage
        SetDiagnosisMode
        endEncounterData
        ( childId, child )


viewContentForAdult : Language -> NominalDate -> PersonId -> Person -> ModelIndexedDb -> Model -> Html Msg
viewContentForAdult language currentDate personId person db model =
    let
        individualParticipants =
            Dict.get personId db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        acuteIllnesses =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                )
                individualParticipants

        pregnancies =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.AntenatalEncounter
                )
                individualParticipants

        pane =
            case model.filter of
                FilterAcuteIllness ->
                    viewAcuteIllnessPane language currentDate personId acuteIllnesses db

                FilterAntenatal ->
                    viewAntenatalPane language currentDate personId pregnancies db

                FilterDemographics ->
                    emptyNode
    in
    div [ class "page-activity patient-record" ]
        [ viewHeader language
        , div [ class "ui unstackable items" ]
            [ div [ class "item" ] <|
                viewAdultDetails
                    language
                    currentDate
                    personId
                    person
                    db
            , div [ class "pane" ]
                [ div [ class "pane-heading" ]
                    [ text <| translate language Translate.ProgressReports ]
                , viewFilters language patientRecordFilters model
                ]
            , pane
            ]
        ]


viewAdultDetails : Language -> NominalDate -> PersonId -> Person -> ModelIndexedDb -> List (Html Msg)
viewAdultDetails language currentDate personId person db =
    let
        ( thumbnailClass, ageEntry ) =
            ( "mother"
            , ageInYears currentDate person
                |> Maybe.map (\ageYears -> viewTransEntry Translate.AgeWord (Translate.YearsOld ageYears |> translate language))
                |> Maybe.withDefault emptyNode
            )

        dateOfBirthEntry =
            Maybe.map
                (\birthDate ->
                    viewTransEntry Translate.DateOfBirth (formatDDMMYYYY birthDate)
                )
                person.birthDate
                |> Maybe.withDefault emptyNode

        ubudeheEntry =
            Maybe.map (Translate.UbudeheNumber >> translate language >> viewTransEntry Translate.UbudeheLabel) person.ubudehe
                |> Maybe.withDefault emptyNode

        educationLevelEntry =
            Maybe.map (Translate.LevelOfEducation >> translate language >> viewTransEntry Translate.LevelOfEducationLabel) person.educationLevel
                |> Maybe.withDefault emptyNode

        childrenData =
            Dict.get personId db.relationshipsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.values
                        >> List.filterMap
                            (\relationship ->
                                if relationship.relatedBy == MyChild then
                                    Dict.get relationship.relatedTo db.people
                                        |> Maybe.andThen RemoteData.toMaybe
                                        |> Maybe.map (\child -> ( relationship.relatedTo, child ))

                                else
                                    Nothing
                            )
                        >> List.sortWith (sortByDate (Tuple.second >> .birthDate >> Maybe.withDefault currentDate))
                    )
                |> Maybe.withDefault []

        childrenList =
            List.indexedMap
                (\index ( _, child ) ->
                    viewEntry (translate language Translate.Baby ++ " " ++ String.fromInt (index + 1)) child.name
                )
                childrenData

        familyLinks =
            let
                childrenMarkup =
                    List.indexedMap viewChildMarkup childrenData

                viewChildMarkup index ( childId, _ ) =
                    li [ onClick <| SetActivePage <| UserPage <| PatientRecordPage childId ]
                        [ span [ class "icon" ]
                            [ span [ class "icon-baby" ] []
                            , span
                                [ class "count" ]
                                [ text <| String.fromInt (index + 1) ]
                            ]
                        ]

                motherMarkup =
                    li [ class "active" ]
                        [ span [ class "icon" ]
                            [ span
                                [ class "icon-mother" ]
                                []
                            ]
                        ]
            in
            motherMarkup
                :: childrenMarkup
                |> ul [ class "links-body" ]

        viewTransEntry labelTransId content =
            viewEntry (translate language labelTransId) content

        viewEntry label content =
            p []
                [ span [ class "label" ] [ text <| label ++ ": " ]
                , span [] [ text content ]
                ]
    in
    [ div [ class "ui image" ]
        [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
    , div [ class "details" ] <|
        [ h2 [ class "ui header" ]
            [ text person.name ]
        , ageEntry
        , dateOfBirthEntry
        , ubudeheEntry
        , educationLevelEntry
        ]
            ++ childrenList
            ++ [ familyLinks ]
    ]


viewFilters : Language -> List PatientRecordFilter -> Model -> Html Msg
viewFilters language filters model =
    let
        renderButton filter =
            button
                [ classList
                    [ ( "active", model.filter == filter )
                    , ( "primary ui button", True )
                    ]
                , onClick <| SetFilter filter
                ]
                [ translateText language <| Translate.PatientRecordFilter filter ]
    in
    List.map renderButton filters
        |> div [ class "ui segment filters" ]


viewAcuteIllnessPane :
    Language
    -> NominalDate
    -> PersonId
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> ModelIndexedDb
    -> Html Msg
viewAcuteIllnessPane language currentDate personId acuteIllnesses db =
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

        entries =
            List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activeIllnesses
                ++ List.map (\( participantId, data ) -> ( ( participantId, StatusResolved ), data )) completedIllnesses

        initiator =
            InitiatorPatientRecordAdult personId

        daignosisEntries =
            List.map (Tuple.first >> viewAcuteIllnessDiagnosisEntry language initiator db SetActivePage) entries
                |> Maybe.Extra.values
                |> List.sortWith sortTuplesByDateDesc
                |> List.map Tuple.second
    in
    div [ class "pane acute-illness" ]
        [ viewPaneHeading language <| Translate.PatientRecordFilter FilterAcuteIllness
        , div [ class "pane-content" ] <|
            entriesHeading
                :: viewEntries language daignosisEntries
        ]


viewAntenatalPane :
    Language
    -> NominalDate
    -> PersonId
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> ModelIndexedDb
    -> Html Msg
viewAntenatalPane language currentDate personId pregnancies db =
    let
        ( activePregnancies, completedPregnancies ) =
            List.partition (Tuple.second >> isPregnancyActive currentDate) pregnancies

        entriesHeading =
            div [ class "heading diagnosis" ]
                [ div [ class "assesment" ] [ text <| translate language Translate.PregnancyStart ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                , div [ class "date" ] [ text <| translate language Translate.PregnancyConcluded ]
                , div [ class "see-more" ] [ text <| translate language Translate.SeeMore ]
                ]

        entries =
            List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activePregnancies
                ++ List.map (\( participantId, data ) -> ( ( participantId, StatusResolved ), data )) completedPregnancies

        daignosisEntries =
            List.map (viewAntenatalEntry language currentDate db) entries
                |> Maybe.Extra.values
                |> List.sortWith sortTuplesByDateDesc
                |> List.map Tuple.second
    in
    div [ class "pane antenatal" ]
        [ viewPaneHeading language <| Translate.PatientRecordFilter FilterAntenatal
        , div [ class "pane-content" ] <|
            entriesHeading
                :: viewEntries language daignosisEntries
        ]


viewAntenatalEntry :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> ( ( IndividualEncounterParticipantId, DiagnosisEntryStatus ), IndividualEncounterParticipant )
    -> Maybe ( NominalDate, Html Msg )
viewAntenatalEntry language currentDate db ( ( participantId, status ), data ) =
    let
        encounters =
            getPrenatalEncountersForParticipant db participantId

        maybeLastEncounterId =
            List.head encounters
                |> Maybe.map Tuple.first
    in
    Maybe.map
        (\lastEncounterId ->
            let
                startDate =
                    Maybe.map (Date.add Days -280 >> formatDDMMYYYY) data.eddDate
                        |> Maybe.withDefault "--/--/----"

                conclusionDate =
                    Maybe.map formatDDMMYYYY data.dateConcluded
                        |> Maybe.withDefault "--/--/----"
            in
            ( Maybe.withDefault currentDate data.eddDate
            , div [ class "entry diagnosis" ]
                [ div [ class "cell assesment" ] [ text startDate ]
                , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
                    [ text <| translate language <| Translate.DiagnosisEntryStatus status ]
                , div [ class "cell date" ] [ text conclusionDate ]
                , div
                    [ class "icon-forward"

                    -- , onClick <|
                    --     setActivePageMsg <|
                    --         UserPage <|
                    --             AntenatalProgressReportPage
                    --
                    --                 lastEncounterId
                    ]
                    []
                ]
            )
        )
        maybeLastEncounterId
