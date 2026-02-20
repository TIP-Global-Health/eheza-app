module Pages.FamilyNutrition.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model
    exposing
        ( FamilyEncounterParticipant
        , FamilyEncounterType(..)
        , FamilyParticipantInitiator(..)
        , emptyFamilyEncounterParticipant
        )
import Backend.FamilyEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.FamilyNutritionEncounter.Model
import Backend.FamilyNutritionEncounter.Utils exposing (getFamilyNutritionEncountersForParticipant)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> FamilyParticipantInitiator -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw initiator db =
    let
        sessions =
            Dict.get id db.familyParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant family nutrition" ]
        [ viewHeader language id initiator
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id isChw db) identity sessions
            ]
        ]


viewHeader : Language -> PersonId -> FamilyParticipantInitiator -> Html App.Model.Msg
viewHeader language id initiator =
    let
        goBackPage =
            case initiator of
                InitiatorParticipantsPage ->
                    PersonPage id (FamilyEncounterOrigin NutritionEncounter)

                InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.FamilyEncounterLabel
                        Backend.FamilyEncounterParticipant.Model.NutritionEncounter
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
    -> Bool
    -> ModelIndexedDb
    -> Dict FamilyEncounterParticipantId FamilyEncounterParticipant
    -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id isChw db sessions =
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.FamilyEncounterSelectVisit
                        Backend.FamilyEncounterParticipant.Model.NutritionEncounter
            ]
        , viewFamilyNutritionAction language currentDate selectedHealthCenter id isChw db sessions
        ]


viewFamilyNutritionAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict FamilyEncounterParticipantId FamilyEncounterParticipant
    -> Html App.Model.Msg
viewFamilyNutritionAction language currentDate selectedHealthCenter id isChw db sessions =
    let
        -- Person nutrition session.
        maybeSessionId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( _, session ) ->
                        session.encounterType == Backend.FamilyEncounterParticipant.Model.NutritionEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            Maybe.map (getFamilyNutritionEncountersForParticipant db) maybeSessionId
                |> Maybe.map
                    (\list ->
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
                |> Maybe.withDefault ( Nothing, False )

        action =
            Maybe.map navigateToEncounterAction maybeActiveEncounterId
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If family nutrition session exists, create new encounter for it.
                            (\sessionId ->
                                [ Backend.FamilyNutritionEncounter.Model.emptyFamilyNutritionEncounter sessionId
                                    currentDate
                                    (Just selectedHealthCenter)
                                    |> Backend.Model.PostFamilyNutritionEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If nutrition session does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyFamilyEncounterParticipant currentDate
                                id
                                Backend.FamilyEncounterParticipant.Model.NutritionEncounter
                                selectedHealthCenter
                                |> Backend.Model.PostFamilyEncounterParticipant
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.FamilyNutritionEncounterPage id_
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
                    Translate.FamilyEncounterLabel
                        Backend.FamilyEncounterParticipant.Model.NutritionEncounter
            ]
        , div [ class "icon-back" ] []
        ]
