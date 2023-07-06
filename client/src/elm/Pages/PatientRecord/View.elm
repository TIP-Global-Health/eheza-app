module Pages.PatientRecord.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortByDate, sortTuplesByDateDesc)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInYears, generateFullName, isPersonAnAdult)
import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator(..))
import Backend.PrenatalEncounter.Utils exposing (eddToLmpDate)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Components.SendViaWhatsAppDialog.Model
import Components.SendViaWhatsAppDialog.View
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllness.Participant.Utils exposing (isAcuteIllnessActive)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (getPrenatalEncountersForParticipant)
import Pages.Prenatal.Participant.Utils exposing (isPregnancyActive)
import Pages.Report.Model exposing (PaneEntryStatus(..))
import Pages.Report.Utils exposing (diagnosisEntryStatusToString)
import Pages.Report.View exposing (viewAcuteIllnessDiagnosisEntry, viewEntries)
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
        , viewStartEncounterButton
        )
import Pages.WellChild.ProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChild.ProgressReport.View exposing (viewNCDAScorecard, viewPaneHeading, viewProgressReport)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (spinner, thumbnailImage)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Bool -> PatientRecordInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw initiator db model =
    Dict.get id db.people
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\person ->
                let
                    patientType =
                        isPersonAnAdult currentDate person
                            |> Maybe.map
                                (\isAdult ->
                                    if isAdult then
                                        PatientAdult

                                    else
                                        PatientChild
                                )
                            |> Maybe.withDefault PatientUnknown
                in
                case model.viewMode of
                    ViewStartEncounter ->
                        viewStartEncounterPage language currentDate isChw id person patientType initiator db model

                    ViewPatientRecord ->
                        if patientType == PatientChild then
                            viewContentForChild language currentDate zscores id person isChw initiator db model

                        else
                            viewContentForOther language currentDate isChw id person patientType initiator db model
            )
        |> Maybe.withDefault spinner


viewHeader : Language -> Model -> Html Msg
viewHeader language model =
    let
        backAction =
            case model.viewMode of
                ViewPatientRecord ->
                    SetActivePage <| UserPage <| PersonsPage Nothing ParticipantDirectoryOrigin

                ViewStartEncounter ->
                    SetViewMode ViewPatientRecord
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PatientRecord ]
        , span
            [ class "link-back"
            , onClick backAction
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewStartEncounterPage : Language -> NominalDate -> Bool -> PersonId -> Person -> PatientType -> PatientRecordInitiator -> ModelIndexedDb -> Model -> Html Msg
viewStartEncounterPage language currentDate isChw personId person patientType initiator db model =
    let
        participantPageInitiator =
            Backend.IndividualEncounterParticipant.Model.InitiatorPatientRecord initiator personId

        encounterButton encounterType participantPage =
            button
                [ class "ui primary button"
                , onClick <| SetActivePage <| UserPage <| participantPage participantPageInitiator personId
                ]
                [ span [ class "button-label" ] [ text <| translate language <| Translate.IndividualEncounterType encounterType False ]
                , span [ class "icon-back" ] []
                ]

        buttons =
            case patientType of
                PatientAdult ->
                    [ encounterButton AcuteIllnessEncounter AcuteIllnessParticipantPage
                    , encounterButton AntenatalEncounter PrenatalParticipantPage
                        |> showIf (person.gender == Female)
                    ]

                PatientChild ->
                    [ encounterButton AcuteIllnessEncounter AcuteIllnessParticipantPage
                    , encounterButton WellChildEncounter WellChildParticipantPage
                    , encounterButton NutritionEncounter NutritionParticipantPage
                    ]

                PatientUnknown ->
                    [ encounterButton AcuteIllnessEncounter AcuteIllnessParticipantPage ]
    in
    div [ class "page-activity patient-record" ]
        [ viewHeader language model
        , div [ class "ui full segment" ] <|
            p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
                :: buttons
        ]


viewContentForChild : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> Bool -> PatientRecordInitiator -> ModelIndexedDb -> Model -> Html Msg
viewContentForChild language currentDate zscores childId child isChw initiator db model =
    let
        wellChildReportInitiator =
            Pages.WellChild.ProgressReport.Model.InitiatorPatientRecord initiator childId

        bottomActionData =
            Just <|
                { showEndEncounterDialog = False
                , allowEndEncounter = False
                , closeEncounterMsg = NoOp
                , setEndEncounterDialogStateMsg = always NoOp
                , startEncounterMsg = SetViewMode ViewStartEncounter
                }
    in
    Pages.WellChild.ProgressReport.View.viewProgressReport language
        currentDate
        zscores
        isChw
        wellChildReportInitiator
        False
        db
        model.diagnosisMode
        model.sendViaWhatsAppDialog
        model.spvReportTab
        SetActivePage
        SetSPVReportTab
        SetDiagnosisMode
        MsgSendViaWhatsAppDialog
        Nothing
        Nothing
        bottomActionData
        ( childId, child )


viewContentForOther : Language -> NominalDate -> Bool -> PersonId -> Person -> PatientType -> PatientRecordInitiator -> ModelIndexedDb -> Model -> Html Msg
viewContentForOther language currentDate isChw personId person patientType initiator db model =
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

        selectedPane =
            case model.filter of
                FilterAcuteIllness ->
                    viewAcuteIllnessPane language currentDate personId initiator acuteIllnesses db

                FilterAntenatal ->
                    viewAntenatalPane language currentDate personId pregnancies db

                FilterFamilyPlanning ->
                    viewFamilyPlanningPane language currentDate personId (List.map Tuple.first pregnancies) db

                FilterDemographics ->
                    -- Demographics report got dedicated page.
                    emptyNode
    in
    div [ class "page-activity patient-record" ]
        [ viewHeader language model
        , div [ class "ui unstackable items" ]
            [ div [ class "item" ] <|
                viewAdultDetails
                    language
                    currentDate
                    personId
                    person
                    db
            , div [ class "pane progress-reports" ]
                [ div [ class "pane-heading" ]
                    [ text <| translate language Translate.ProgressReports ]
                , viewFilters language person patientType model
                ]
            , selectedPane
            , viewStartEncounterButton language (SetViewMode ViewStartEncounter)
                |> -- Allow staritng encounter only if we can verify
                   -- that patient is an adult.
                   showIf (patientType == PatientAdult)
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
                    li [ onClick <| SetActivePage <| UserPage <| PatientRecordPage (Backend.PatientRecord.Model.InitiatorPatientRecord personId) childId ]
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


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 140
    , height = 140
    }


viewFilters : Language -> Person -> PatientType -> Model -> Html Msg
viewFilters language person patientType model =
    let
        renderButton filter =
            button
                [ classList
                    [ ( "active", filter == model.filter )
                    , ( "primary ui button", True )
                    ]
                , onClick <| SetFilter filter
                ]
                [ translateText language <| Translate.PatientRecordFilter filter ]

        patientRecordFilters =
            case patientType of
                PatientChild ->
                    []

                PatientAdult ->
                    case person.gender of
                        Male ->
                            [ FilterAcuteIllness
                            , FilterDemographics
                            ]

                        Female ->
                            [ FilterAcuteIllness
                            , FilterAntenatal
                            , FilterFamilyPlanning
                            , FilterDemographics
                            ]

                PatientUnknown ->
                    [ FilterAcuteIllness
                    , FilterDemographics
                    ]
    in
    List.map renderButton patientRecordFilters
        |> div [ class "ui segment filters" ]


viewAcuteIllnessPane :
    Language
    -> NominalDate
    -> PersonId
    -> PatientRecordInitiator
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> ModelIndexedDb
    -> Html Msg
viewAcuteIllnessPane language currentDate personId initiator acuteIllnesses db =
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

        daignosisEntries =
            List.map
                (\( data, _ ) ->
                    viewAcuteIllnessDiagnosisEntry
                        language
                        (Backend.AcuteIllnessEncounter.Model.InitiatorPatientRecord initiator personId)
                        db
                        SetActivePage
                        data
                )
                entries
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
            div [ class "heading antenatal" ]
                [ div [ class "date-start" ] [ text <| translate language Translate.PregnancyStart ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                , div [ class "date-end" ] [ text <| translate language Translate.PregnancyConclusion ]
                , div [ class "see-more" ] [ text <| translate language Translate.SeeMore ]
                ]

        entries =
            List.map (\( participantId, data ) -> ( ( participantId, StatusOngoing ), data )) activePregnancies
                ++ List.map (\( participantId, data ) -> ( ( participantId, StatusResolved ), data )) completedPregnancies

        daignosisEntries =
            List.map (viewAntenatalEntry language currentDate personId db) entries
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
    -> PersonId
    -> ModelIndexedDb
    -> ( ( IndividualEncounterParticipantId, PaneEntryStatus ), IndividualEncounterParticipant )
    -> Maybe ( NominalDate, Html Msg )
viewAntenatalEntry language currentDate personId db ( ( participantId, status ), data ) =
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
                    Maybe.map (eddToLmpDate >> formatDDMMYYYY) data.eddDate
                        |> Maybe.withDefault "--/--/----"

                conclusionDate =
                    Maybe.map formatDDMMYYYY data.dateConcluded
                        |> Maybe.withDefault "--/--/----"
            in
            ( Maybe.withDefault currentDate data.eddDate
            , div [ class "entry antenatal" ]
                [ div [ class "cell date-start" ] [ text startDate ]
                , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
                    [ text <| translate language <| Translate.EntryStatusAntenatal status ]
                , div [ class "cell date-end" ] [ text conclusionDate ]
                , div
                    [ class "icon-forward"
                    , onClick <|
                        SetActivePage <|
                            UserPage <|
                                ClinicalProgressReportPage
                                    (Backend.PrenatalEncounter.Model.InitiatorPatientRecord personId)
                                    lastEncounterId
                    ]
                    []
                ]
            )
        )
        maybeLastEncounterId


viewFamilyPlanningPane :
    Language
    -> NominalDate
    -> PersonId
    -> List IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> Html Msg
viewFamilyPlanningPane language currentDate personId prenatalParticipantsIds db =
    let
        entriesHeading =
            div [ class "heading family-planning" ]
                [ div [ class "date" ] [ text <| translate language Translate.EncounterDate ]
                , div [ class "signs" ] [ text <| translate language Translate.SelectedFamilyPlanningMethod ]
                ]

        content =
            groupFamilyPlannings
                ++ prenatalFamilyPlannings
                |> List.sortWith sortTuplesByDateDesc
                |> List.map (viewFamilyPlanningEntry language)

        groupFamilyPlannings =
            Dict.get personId db.motherMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map (.familyPlannings >> Dict.values)
                |> Maybe.withDefault []
                |> List.map prepareEntryData

        prenatalFamilyPlannings =
            List.map
                (\encounterId ->
                    Dict.get encounterId db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map .familyPlanning
                )
                prenatalEncountersIds
                |> Maybe.Extra.values
                |> Maybe.Extra.values
                |> List.map (Tuple.second >> prepareEntryData)

        prepareEntryData familyPlanning =
            ( familyPlanning.dateMeasured, familyPlanning.value )

        prenatalEncountersIds =
            List.map (getPrenatalEncountersForParticipant db >> List.map Tuple.first)
                prenatalParticipantsIds
                |> List.concat
    in
    div [ class "pane family-planning" ]
        [ viewPaneHeading language <| Translate.PatientRecordFilter FilterFamilyPlanning
        , div [ class "pane-content" ] <|
            entriesHeading
                :: content
        ]


viewFamilyPlanningEntry language ( date, signs ) =
    div [ class "entry family-planning" ]
        [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
        , div [ class "signs" ]
            [ EverySet.toList signs
                |> List.map (Translate.FamilyPlanningSignLabel >> translate language)
                |> String.join ", "
                |> text
            ]
        ]