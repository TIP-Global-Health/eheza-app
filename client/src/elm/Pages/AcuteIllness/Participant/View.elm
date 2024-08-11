module Pages.AcuteIllness.Participant.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter, emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualParticipantInitiator(..), emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getAcuteIllnessEncountersForParticipant)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllness.Encounter.Utils exposing (getAcuteIllnessDiagnosisForParticipant)
import Pages.AcuteIllness.Participant.Model exposing (..)
import Pages.AcuteIllness.Participant.Utils exposing (isAcuteIllnessActive, noPursueAcuteIllnessDiagnoses)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.Utils exposing (compareAcuteIllnessEncounters, compareAcuteIllnessEncountersDesc)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.NominalDate exposing (sortEncounterTuples)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> IndividualParticipantInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate selectedHealthCenter personId isChw initiator db model =
    let
        sessions =
            Dict.get personId db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant individual acute-illness" ]
        [ viewHeader language initiator model
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewContent language currentDate selectedHealthCenter personId isChw db model) identity sessions
            ]
        ]


viewHeader : Language -> IndividualParticipantInitiator -> Model -> Html Msg
viewHeader language initiator model =
    let
        ( labelTransId, action ) =
            case model.viewMode of
                ManageIllnesses ->
                    let
                        goBackPage =
                            case initiator of
                                InitiatorParticipantsPage ->
                                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter

                                InitiatorPatientRecord patientRecordInitiator personId ->
                                    PatientRecordPage patientRecordInitiator personId
                    in
                    ( Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter True
                    , SetActivePage <| UserPage goBackPage
                    )

                ManageParticipants ->
                    ( Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter True
                    , SetViewMode ManageIllnesses
                    )

                RecordOutcome ->
                    ( Translate.RecordAcuteIllnessOutcome
                    , SetViewMode ManageParticipants
                    )
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language labelTransId ]
        , span
            [ class "link-back"
            , onClick action
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> ModelIndexedDb -> Model -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html Msg
viewContent language currentDate selectedHealthCenter personId isChw db model sessions =
    let
        activeSessions =
            Dict.toList sessions
                |> List.filter
                    (\( _, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)
                            && isAcuteIllnessActive currentDate session
                    )
                |> List.sortWith sortEncounterTuples
    in
    case model.viewMode of
        ManageIllnesses ->
            viewManageIllnessesContent language currentDate selectedHealthCenter personId isChw db activeSessions

        ManageParticipants ->
            viewManageParticipantsContent language currentDate selectedHealthCenter personId isChw db activeSessions

        RecordOutcome ->
            viewRecordOutcomeContent language currentDate selectedHealthCenter isChw db activeSessions


viewManageIllnessesContent :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html Msg
viewManageIllnessesContent language currentDate selectedHealthCenter personId isChw db activeSessions =
    let
        lastActiveSession =
            List.reverse activeSessions
                |> List.head

        ( createIllnessNavigateToEncounterAction, createIllnessNavigateToEncounterButtonDisabled ) =
            Maybe.map
                (\( sessionId, session ) ->
                    if session.startDate /= currentDate then
                        -- Session was not started today, therefore, we know
                        -- it's first encounter was not started today, which indicates
                        -- subsequent visit option is needed.
                        -- This option will be provided at `active Illnesses` section.
                        ( startIllnessAction, False )

                    else
                        let
                            sessionEncounters =
                                getAcuteIllnessEncountersForParticipant db sessionId
                                    |> List.filter (Tuple.second >> filterByEncounterTypeCondition isChw)

                            mActiveEncounterId =
                                List.filter (Tuple.second >> isDailyEncounterActive currentDate) sessionEncounters
                                    |> List.head
                                    |> Maybe.map Tuple.first

                            encounterWasCompletedToday =
                                sessionEncounters
                                    |> List.filter
                                        (\( _, encounter ) ->
                                            encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                        )
                                    |> List.isEmpty
                                    |> not
                        in
                        ( Maybe.map navigateToEncounterAction mActiveEncounterId
                            |> Maybe.withDefault startIllnessAction
                        , -- We do not allow to create multiple illnesses on the same day.
                          -- Therefore, we disable the button.
                          encounterWasCompletedToday
                        )
                )
                lastActiveSession
                |> Maybe.withDefault ( startIllnessAction, False )

        startIllnessAction =
            startIllnessActionMsg currentDate selectedHealthCenter personId isChw

        createIllnessNavigateToEncounterSection =
            [ viewLabel language "select-visit" <| Translate.IndividualEncounterSelectVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter True
            , viewButton language
                createIllnessNavigateToEncounterAction
                Translate.AcuteIllnessNew
                createIllnessNavigateToEncounterButtonDisabled
            ]

        activeIllnessesExists =
            List.map Tuple.first activeSessions
                |> List.filterMap (getAcuteIllnessDiagnosisForParticipant db >> Maybe.map Tuple.second)
                |> List.filter (\diagnosis -> not <| List.member diagnosis noPursueAcuteIllnessDiagnoses)
                |> List.isEmpty
                |> not

        activeIllnessesSection =
            if activeIllnessesExists then
                [ viewButton language (SetViewMode ManageParticipants) Translate.AcuteIllnessExisting False ]

            else
                []
    in
    createIllnessNavigateToEncounterSection
        ++ activeIllnessesSection
        |> div []


viewManageParticipantsContent :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html Msg
viewManageParticipantsContent language currentDate selectedHealthCenter personId isChw db activeSessions =
    let
        activeIllnesses =
            List.map Tuple.first activeSessions
                |> List.filterMap (viewActiveIllness language currentDate selectedHealthCenter isChw db ManageParticipants)

        lastActiveSession =
            List.reverse activeSessions
                |> List.head

        ( createIllnessNavigateToEncounterAction, createIllnessNavigateToEncounterButtonDisabled ) =
            lastActiveSession
                |> Maybe.map
                    (\( sessionId, session ) ->
                        if session.startDate /= currentDate then
                            -- Session was not started today, therefore, we know
                            -- its first encounter was not started today, which indicates
                            -- subsequent visit option is needed.
                            -- This option will be provided at `active Illnesses` section.
                            ( startIllnessAction, False )

                        else
                            let
                                sessionEncounters =
                                    getAcuteIllnessEncountersForParticipant db sessionId
                                        |> List.filter (Tuple.second >> filterByEncounterTypeCondition isChw)

                                mActiveEncounterId =
                                    List.filter (Tuple.second >> isDailyEncounterActive currentDate) sessionEncounters
                                        |> List.head
                                        |> Maybe.map Tuple.first

                                encounterWasCompletedToday =
                                    sessionEncounters
                                        |> List.filter
                                            (\( _, encounter ) ->
                                                encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                            )
                                        |> List.isEmpty
                                        |> not
                            in
                            ( mActiveEncounterId
                                |> Maybe.map navigateToEncounterAction
                                |> Maybe.withDefault startIllnessAction
                            , -- We do not allow to create multiple encounters for same illness on the same day.
                              -- Therefore, we disable the button.
                              encounterWasCompletedToday
                            )
                    )
                |> Maybe.withDefault ( startIllnessAction, False )

        startIllnessAction =
            startIllnessActionMsg currentDate selectedHealthCenter personId isChw

        createIllnessNavigateToEncounterButton =
            viewButton language
                createIllnessNavigateToEncounterAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)
                createIllnessNavigateToEncounterButtonDisabled
    in
    if List.isEmpty activeIllnesses then
        div []
            [ viewLabel language "select-visit" <| Translate.IndividualEncounterSelectVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter True
            , createIllnessNavigateToEncounterButton
            ]

    else
        let
            recordIllnessOutcomeButton =
                viewButton language (SetViewMode RecordOutcome) Translate.RecordAcuteIllnessOutcome False
        in
        div []
            [ viewLabel language "select-illness" Translate.SelectExistingAcuteIllness
            , div [ class "active-illnesses" ] activeIllnesses
            , viewLabel language "start-new-encounter" Translate.StrartNewAcuteIllnessHelper
            , createIllnessNavigateToEncounterButton
            , div [ class "separator" ] []
            , viewLabel language "" Translate.CloseAcuteIllnessLabel
            , recordIllnessOutcomeButton
            ]


startIllnessActionMsg : NominalDate -> HealthCenterId -> PersonId -> Bool -> Msg
startIllnessActionMsg currentDate selectedHealthCenter personId isChw =
    let
        encounterType =
            -- We know it's going to be first encounter, therefore,
            -- it's clear it can't be AcuteIllnessEncounterNurseSubsequent.
            if isChw then
                AcuteIllnessEncounterCHW

            else
                AcuteIllnessEncounterNurse
    in
    emptyIndividualEncounterParticipant currentDate personId Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter selectedHealthCenter
        |> Backend.Model.PostIndividualEncounterParticipant (Backend.IndividualEncounterParticipant.Model.AcuteIllnessData encounterType)
        |> MsgBackend


viewRecordOutcomeContent :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Bool
    -> ModelIndexedDb
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html Msg
viewRecordOutcomeContent language currentDate selectedHealthCenter isChw db activeSessions =
    let
        activeIllnesses =
            List.map Tuple.first activeSessions
                |> List.filterMap (viewActiveIllness language currentDate selectedHealthCenter isChw db RecordOutcome)
    in
    div []
        [ viewLabel language "select-illness" Translate.SelectExistingAcuteIllnessToRecordOutcome
        , div [ class "active-illnesses" ] activeIllnesses
        ]


viewActiveIllness :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Bool
    -> ModelIndexedDb
    -> AcuteIllnessParticipantViewMode
    -> IndividualEncounterParticipantId
    -> Maybe (Html Msg)
viewActiveIllness language currentDate selectedHealthCenter isChw db viewMode sessionId =
    let
        sessionEncounters =
            getAcuteIllnessEncountersForParticipant db sessionId
                |> List.sortWith (\( _, e1 ) ( _, e2 ) -> compareAcuteIllnessEncountersDesc e1 e2)

        mDiagnosis =
            List.map Tuple.second sessionEncounters
                |> List.filter (\encounter -> encounter.diagnosis /= NoAcuteIllnessDiagnosis)
                |> List.head
                |> Maybe.map .diagnosis
    in
    Maybe.andThen
        (\diagnosis ->
            if List.member diagnosis noPursueAcuteIllnessDiagnoses then
                -- Do not show illness if diagnosis does not
                -- require subsequent encounters.
                Nothing

            else
                case viewMode of
                    -- No need to view illnesses for this view mode.
                    ManageIllnesses ->
                        Nothing

                    ManageParticipants ->
                        viewActiveIllnessForManagement language currentDate selectedHealthCenter isChw sessionId sessionEncounters diagnosis

                    RecordOutcome ->
                        viewActiveIllnessForOutcome language currentDate isChw sessionId sessionEncounters diagnosis
        )
        mDiagnosis


viewActiveIllnessForManagement :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Bool
    -> IndividualEncounterParticipantId
    -> List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
    -> AcuteIllnessDiagnosis
    -> Maybe (Html Msg)
viewActiveIllnessForManagement language currentDate selectedHealthCenter isChw sessionId encounters diagnosis =
    let
        -- Variable encounters  holds data for all encounters of the illness,
        -- performed by both Nurse and CHW.
        ( activeEncounters, completedEncounters ) =
            List.partition (Tuple.second >> isDailyEncounterActive currentDate)
                encounters

        mActiveEncounter =
            List.filter
                (\( _, encounter ) ->
                    -- To determine active encounter we filter to get only Nurse
                    -- or CHW encounters, as nurse should not be able to enter
                    -- encounter started by CHW, and vice versa.
                    filterByEncounterTypeCondition isChw encounter
                )
                activeEncounters
                |> List.head

        mActiveEncounterId =
            Maybe.map Tuple.first mActiveEncounter

        encounterSequenceNumberForToday =
            List.filter (Tuple.second >> .startDate >> (==) currentDate) encounters
                |> List.sortBy (Tuple.second >> .sequenceNumber)
                |> List.reverse
                |> List.head
                |> Maybe.map (Tuple.second >> .sequenceNumber >> (+) 1)
                |> Maybe.withDefault 1

        action =
            Maybe.map navigateToEncounterAction mActiveEncounterId
                |> Maybe.withDefault
                    (emptyAcuteIllnessEncounter sessionId currentDate encounterSequenceNumberForToday encounterType (Just selectedHealthCenter)
                        |> Backend.Model.PostAcuteIllnessEncounter
                        |> MsgBackend
                    )

        encounterType =
            if isChw then
                AcuteIllnessEncounterCHW

            else
                let
                    nurseEncounterPerformed =
                        List.filter
                            (Tuple.second
                                >> .encounterType
                                >> (==) AcuteIllnessEncounterNurse
                            )
                            encounters
                            |> List.isEmpty
                            |> not
                in
                if nurseEncounterPerformed then
                    AcuteIllnessEncounterNurseSubsequent

                else
                    AcuteIllnessEncounterNurse

        encounterLabelTransId =
            if List.length encounters == 1 && isJust mActiveEncounterId then
                Translate.IndividualEncounterFirstVisit
                    Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter

            else
                Translate.IndividualEncounterSubsequentVisit
                    Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter

        mReferringEncounter =
            case mActiveEncounter of
                Just ( _, activeEncounter ) ->
                    -- When we have active encounter, we search for the one
                    -- that was completed just before the active one.
                    List.filter
                        (\( _, encounter ) ->
                            compareAcuteIllnessEncounters encounter activeEncounter == LT
                        )
                        completedEncounters
                        |> List.head

                Nothing ->
                    -- When we don't have active encounter, we take
                    -- last encounter that was completed.
                    List.head completedEncounters

        referringLabel =
            Maybe.map
                (\( _, encounter ) ->
                    div [] [ text <| translate language <| Translate.SubsequentEncounterReferral encounter.encounterType ]
                )
                mReferringEncounter
                |> Maybe.withDefault emptyNode
    in
    Just <|
        div
            [ class "ui primary button active-illness"
            , onClick action
            ]
            [ div [ class "button-label" ]
                [ div [ class "encounter-label" ]
                    [ div [] [ text <| translate language encounterLabelTransId ]
                    , referringLabel
                    ]
                , div [] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
                ]
            , div [ class "icon-back" ] []
            ]


viewActiveIllnessForOutcome :
    Language
    -> NominalDate
    -> Bool
    -> IndividualEncounterParticipantId
    -> List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
    -> AcuteIllnessDiagnosis
    -> Maybe (Html Msg)
viewActiveIllnessForOutcome language currentDate isChw sessionId encounters diagnosis =
    Just <|
        viewButton language (navigateToRecordOutcomePage sessionId) (Translate.AcuteIllnessDiagnosis diagnosis) False


filterByEncounterTypeCondition : Bool -> AcuteIllnessEncounter -> Bool
filterByEncounterTypeCondition isChw encounter =
    if isChw then
        encounter.encounterType == AcuteIllnessEncounterCHW

    else
        encounter.encounterType /= AcuteIllnessEncounterCHW


viewLabel : Language -> String -> TranslationId -> Html Msg
viewLabel language cssClass transId =
    p [ class cssClass ] [ text <| translate language transId ]


viewButton : Language -> Msg -> TranslationId -> Bool -> Html Msg
viewButton language action lablelTransId disabled =
    div
        [ class "ui primary button"
        , classList [ ( "disabled", disabled ) ]
        , onClick action
        ]
        [ div [ class "button-label" ]
            [ text <| translate language lablelTransId ]
        , div [ class "icon-back" ] []
        ]


navigateToEncounterAction : AcuteIllnessEncounterId -> Msg
navigateToEncounterAction id =
    Pages.Page.AcuteIllnessEncounterPage id
        |> UserPage
        |> SetActivePage


navigateToRecordOutcomePage : IndividualEncounterParticipantId -> Msg
navigateToRecordOutcomePage id =
    Pages.Page.AcuteIllnessOutcomePage id
        |> UserPage
        |> SetActivePage
