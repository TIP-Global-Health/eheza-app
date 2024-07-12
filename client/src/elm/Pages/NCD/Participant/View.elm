module Pages.NCD.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , IndividualParticipantInitiator(..)
        , emptyIndividualEncounterParticipant
        )
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model
import Backend.NutritionEncounter.Utils exposing (getNCDEncountersForParticipant)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> IndividualParticipantInitiator -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id initiator db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant individual ncd" ]
        [ viewHeader language initiator
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id db) identity sessions
            ]
        ]


viewHeader : Language -> IndividualParticipantInitiator -> Html App.Model.Msg
viewHeader language initiator =
    let
        goBackPage =
            case initiator of
                InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.NCDEncounter

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
                        Backend.IndividualEncounterParticipant.Model.NCDEncounter
                        False
            ]
        , span
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id db sessions =
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.NCDEncounter
                        False
            ]
        , viewNCDAction language currentDate selectedHealthCenter id db sessions
        ]


viewNCDAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewNCDAction language currentDate selectedHealthCenter id db sessions =
    let
        -- Person ncd session.
        maybeSessionId =
            Dict.toList sessions
                |> List.filter
                    (\( _, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.NCDEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            Maybe.map
                (getNCDEncountersForParticipant db
                    >> (\list ->
                            ( List.filter (Tuple.second >> isDailyEncounterActive currentDate) list
                                |> List.head
                                |> Maybe.map Tuple.first
                            , List.filter
                                (\( _, encounter ) ->
                                    encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                )
                                list
                                |> List.isEmpty
                                |> not
                            )
                       )
                )
                maybeSessionId
                |> Maybe.withDefault ( Nothing, False )

        action =
            Maybe.map navigateToEncounterAction maybeActiveEncounterId
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If ncd session exists, create new encounter for it.
                            (\sessionId ->
                                [ Backend.NCDEncounter.Model.emptyNCDEncounter sessionId currentDate (Just selectedHealthCenter)
                                    |> Backend.Model.PostNCDEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If ncd session does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.NCDEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.NCDEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div
        (classList
            [ ( "ui primary button", True )
            , ( "disabled", encounterWasCompletedToday )
            ]
            :: action
        )
        [ div [ class "button-label" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.NCDEncounter
                        False
            ]
        , div [ class "icon-back" ] []
        ]
