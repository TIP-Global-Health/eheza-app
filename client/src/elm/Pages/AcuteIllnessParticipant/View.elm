module Pages.AcuteIllnessParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter, emptyAcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (emptyIndividualEncounterParticipant, isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessEncounter.Utils exposing (getAcuteIllnessDiagnosisForParticipant)
import Pages.AcuteIllnessParticipant.Model exposing (..)
import Pages.AcuteIllnessParticipant.Utils exposing (isAcuteIllnessActive)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate selectedHealthCenter id db model =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant acute-illness" ]
        [ viewHeader language model
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewContent language currentDate selectedHealthCenter id db model) identity sessions
            ]
        ]


viewHeader : Language -> Model -> Html Msg
viewHeader language model =
    let
        ( labelTransId, action ) =
            case model.viewMode of
                ManageIllnesses ->
                    ( Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                    , SetActivePage <|
                        UserPage <|
                            IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                    )

                ManageParticipants ->
                    ( Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
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
        , a
            [ class "link-back"
            , onClick action
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Model -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html Msg
viewContent language currentDate selectedHealthCenter id db model sessions =
    let
        activeSessions =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)
                            && isAcuteIllnessActive currentDate session
                    )
                |> List.sortWith (\( _, s1 ) ( _, s2 ) -> Gizra.NominalDate.compare s1.startDate s2.startDate)
    in
    case model.viewMode of
        ManageIllnesses ->
            viewManageIllnessesContent language currentDate selectedHealthCenter id db activeSessions

        ManageParticipants ->
            viewManageParticipantsContent language currentDate selectedHealthCenter id db activeSessions

        RecordOutcome ->
            viewRecordOutcomeContent language currentDate selectedHealthCenter id db activeSessions


viewManageIllnessesContent :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html Msg
viewManageIllnessesContent language currentDate selectedHealthCenter id db activeSessions =
    let
        lastActiveSession =
            List.reverse activeSessions
                |> List.head

        ( createIllnessNavigateToEncounterAction, createIllnessNavigateToEncounterButtonDisabled ) =
            lastActiveSession
                |> Maybe.map
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
                                    Dict.get sessionId db.acuteIllnessEncountersByParticipant
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.toMaybe
                                        |> Maybe.map Dict.toList
                                        |> Maybe.withDefault []

                                maybeActiveEncounterId =
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
                            ( maybeActiveEncounterId
                                |> Maybe.map navigateToEncounterAction
                                |> Maybe.withDefault startIllnessAction
                            , -- We do not allow to create multiple encounters for same illness on the same day.
                              -- Therefore, we disable the button.
                              encounterWasCompletedToday
                            )
                    )
                |> Maybe.withDefault ( startIllnessAction, False )

        startIllnessAction =
            emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter selectedHealthCenter
                |> Backend.Model.PostIndividualSession
                |> MsgBackend

        createIllnessNavigateToEncounterSection =
            [ viewLabel language "select-visit" <| Translate.IndividualEncounterSelectVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            , viewButton language
                createIllnessNavigateToEncounterAction
                Translate.AcuteIllnessNew
                createIllnessNavigateToEncounterButtonDisabled
            ]

        activeIllnessesExists =
            List.map Tuple.first activeSessions
                |> List.filterMap (getAcuteIllnessDiagnosisForParticipant db)
                |> List.filter ((/=) NoAcuteIllnessDiagnosis)
                |> List.isEmpty
                |> not

        activeIllnessesSection =
            if activeIllnessesExists then
                [ viewButton language (SetViewMode ManageParticipants) Translate.AcuteIllnessExisting False
                ]

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
    -> ModelIndexedDb
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html Msg
viewManageParticipantsContent language currentDate selectedHealthCenter id db activeSessions =
    let
        activeIllnesses =
            List.map Tuple.first activeSessions
                |> List.filterMap (viewActiveIllness language currentDate selectedHealthCenter db ManageParticipants)

        lastActiveSession =
            List.reverse activeSessions
                |> List.head

        ( createIllnessNavigateToEncounterAction, createIllnessNavigateToEncounterButtonDisabled ) =
            lastActiveSession
                |> Maybe.map
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
                                    Dict.get sessionId db.acuteIllnessEncountersByParticipant
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.toMaybe
                                        |> Maybe.map Dict.toList
                                        |> Maybe.withDefault []

                                maybeActiveEncounterId =
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
                            ( maybeActiveEncounterId
                                |> Maybe.map navigateToEncounterAction
                                |> Maybe.withDefault startIllnessAction
                            , -- We do not allow to create multiple encounters for same illness on the same day.
                              -- Therefore, we disable the button.
                              encounterWasCompletedToday
                            )
                    )
                |> Maybe.withDefault ( startIllnessAction, False )

        startIllnessAction =
            emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter selectedHealthCenter
                |> Backend.Model.PostIndividualSession
                |> MsgBackend

        createIllnessNavigateToEncounterButton =
            viewButton language
                createIllnessNavigateToEncounterAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)
                createIllnessNavigateToEncounterButtonDisabled

        recordIllnessOutcomeButton =
            viewButton language (SetViewMode RecordOutcome) Translate.RecordAcuteIllnessOutcome False
    in
    if List.isEmpty activeIllnesses then
        div []
            [ viewLabel language "select-visit" <| Translate.IndividualEncounterSelectVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            , createIllnessNavigateToEncounterButton
            ]

    else
        div []
            [ viewLabel language "select-illness" Translate.SelectExistingAcuteIllness
            , div [ class "active-illnesses" ] activeIllnesses
            , viewLabel language "start-new-encounter" Translate.StrartNewAcuteIllnessHelper
            , createIllnessNavigateToEncounterButton
            , div [ class "separator" ] []
            , viewLabel language "" Translate.CloseAcuteIllnessLabel
            , recordIllnessOutcomeButton
            ]


viewRecordOutcomeContent :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> Html Msg
viewRecordOutcomeContent language currentDate selectedHealthCenter id db activeSessions =
    let
        activeIllnesses =
            List.map Tuple.first activeSessions
                |> List.filterMap (viewActiveIllness language currentDate selectedHealthCenter db RecordOutcome)
    in
    div []
        [ viewLabel language "select-illness" Translate.SelectExistingAcuteIllnessToRecordOutcome
        , div [ class "active-illnesses" ] activeIllnesses
        ]


viewActiveIllness :
    Language
    -> NominalDate
    -> HealthCenterId
    -> ModelIndexedDb
    -> AcuteIllnessParticipantViewMode
    -> IndividualEncounterParticipantId
    -> Maybe (Html Msg)
viewActiveIllness language currentDate selectedHealthCenter db viewMode sessionId =
    let
        sessionEncounters =
            Dict.get sessionId db.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map Dict.toList

        mDiagnosis =
            sessionEncounters
                |> Maybe.andThen
                    (List.map Tuple.second
                        >> List.sortWith (\e1 e2 -> Gizra.NominalDate.compare e1.startDate e2.startDate)
                        >> List.head
                        >> Maybe.map .diagnosis
                    )
    in
    case mDiagnosis of
        Nothing ->
            Nothing

        Just NoAcuteIllnessDiagnosis ->
            Nothing

        Just diagnosis ->
            sessionEncounters
                |> Maybe.andThen
                    (\encounters ->
                        case viewMode of
                            -- No need to view illnesses for this view mode.
                            ManageIllnesses ->
                                Nothing

                            ManageParticipants ->
                                viewActiveIllnessForManagement language currentDate selectedHealthCenter sessionId encounters diagnosis

                            RecordOutcome ->
                                viewActiveIllnessForOutcome language currentDate sessionId encounters diagnosis
                    )


viewActiveIllnessForManagement :
    Language
    -> NominalDate
    -> HealthCenterId
    -> IndividualEncounterParticipantId
    -> List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
    -> AcuteIllnessDiagnosis
    -> Maybe (Html Msg)
viewActiveIllnessForManagement language currentDate selectedHealthCenter sessionId encounters diagnosis =
    let
        maybeActiveEncounterId =
            List.filter (Tuple.second >> isDailyEncounterActive currentDate) encounters
                |> List.head
                |> Maybe.map Tuple.first

        encounterWasCompletedToday =
            encounters
                |> List.filter
                    (\( _, encounter ) ->
                        encounter.startDate == currentDate && encounter.endDate == Just currentDate
                    )
                |> List.isEmpty
                |> not

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (emptyAcuteIllnessEncounter sessionId currentDate (Just selectedHealthCenter)
                        |> Backend.Model.PostAcuteIllnessEncounter
                        |> MsgBackend
                    )

        encounterLabel =
            if List.length encounters == 1 && isJust maybeActiveEncounterId then
                Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter

            else
                Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
    in
    Just <|
        div
            [ classList
                [ ( "ui primary button active-illness", True )
                , ( "disabled", encounterWasCompletedToday )
                ]
            , onClick action
            ]
            [ div [ class "button-label" ]
                [ div [ class "encounter-label" ]
                    [ text <| translate language encounterLabel ]
                , div [] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
                ]
            , div [ class "icon-back" ] []
            ]


viewActiveIllnessForOutcome :
    Language
    -> NominalDate
    -> IndividualEncounterParticipantId
    -> List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
    -> AcuteIllnessDiagnosis
    -> Maybe (Html Msg)
viewActiveIllnessForOutcome language currentDate sessionId encounters diagnosis =
    Just <|
        viewButton language (navigateToRecordOutcomePage sessionId) (Translate.AcuteIllnessDiagnosis diagnosis) False


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
