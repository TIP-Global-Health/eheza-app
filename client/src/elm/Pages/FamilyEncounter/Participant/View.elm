module Pages.FamilyEncounter.Participant.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Translate exposing (Language)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> IndividualParticipantInitiator -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw initiator db =
    -- TODO: Implement participant search and selection in #1664
    div
        [ class "wrap wrap-alt-2 page-participant individual family" ]
        [ viewHeader language isChw initiator
        , div
            [ class "ui full segment" ]
            [ h2 [] [ text "Family Encounter Participant - TODO: Implement in #1664" ]
            , p [] [ text "This page will allow searching and selecting participants for family encounters." ]
            ]
        ]


viewHeader : Language -> Bool -> IndividualParticipantInitiator -> Html App.Model.Msg
viewHeader language isChw initiator =
    let
        goBackPage =
            case initiator of
                Backend.IndividualEncounterParticipant.Model.InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.FamilyEncounter

                Backend.IndividualEncounterParticipant.Model.InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text "Family Encounter Participant" ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]
