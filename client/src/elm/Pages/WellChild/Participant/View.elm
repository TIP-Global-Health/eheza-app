module Pages.WellChild.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualParticipantInitiator(..), emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (resolveMainParticipant, resolveTodaysEncounterState)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForPerson)
import Backend.Person.Utils exposing (isNewborn)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..))
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> IndividualParticipantInitiator -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw initiator db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant individual well-child" ]
        [ viewHeader language isChw initiator
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id isChw db) identity sessions
            ]
        ]


viewHeader : Language -> Bool -> IndividualParticipantInitiator -> Html App.Model.Msg
viewHeader language isChw initiator =
    let
        goBackPage =
            case initiator of
                InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.WellChildEncounter

                InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , span
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id isChw db sessions =
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , viewWellChildAction language currentDate selectedHealthCenter id isChw db sessions
        ]


viewWellChildAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewWellChildAction language currentDate selectedHealthCenter id isChw db sessions =
    let
        -- Person Well Child participant.
        maybeSessionId =
            resolveMainParticipant Backend.IndividualEncounterParticipant.Model.WellChildEncounter sessions

        ( pediatricCareEncounters, chwPediatricCareEncounters ) =
            getWellChildEncountersForPerson db id
                |> List.partition (Tuple.second >> .encounterType >> (==) PediatricCare)

        -- We will not allow create new / edit existing action, if an encounter
        -- of the same kind (nurse or CHW) was completed today.
        ( maybeActiveEncounterId, disableAction, newEncounterType ) =
            if isChw then
                let
                    newborn =
                        Dict.get id db.people
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.andThen (isNewborn currentDate)
                            |> Maybe.withDefault False

                    newbornEncounterExists =
                        List.any (Tuple.second >> .encounterType >> (==) NewbornExam) chwPediatricCareEncounters

                    ( activeEncounterId, encounterWasCompletedToday ) =
                        resolveTodaysEncounterState currentDate chwPediatricCareEncounters
                in
                ( activeEncounterId
                , encounterWasCompletedToday
                , if not newbornEncounterExists && newborn then
                    -- Child is below 2 months old and did not
                    -- have Newborn Exam before.
                    NewbornExam

                  else
                    PediatricCareChw
                )

            else
                let
                    ( activeEncounterId, encounterWasCompletedToday ) =
                        resolveTodaysEncounterState currentDate pediatricCareEncounters
                in
                ( activeEncounterId, encounterWasCompletedToday, PediatricCare )

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If participant exists, create new encounter for it.
                            (\sessionId ->
                                [ Backend.WellChildEncounter.Model.emptyWellChildEncounter sessionId currentDate newEncounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostWellChildEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If participant does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.WellChildEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualEncounterParticipant (Backend.IndividualEncounterParticipant.Model.WellChildData newEncounterType)
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.WellChildEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div
        (classList
            [ ( "ui primary button", True )
            , ( "disabled", disableAction )
            ]
            :: action
        )
        [ div [ class "button-label" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , div [ class "icon-back" ] []
        ]
