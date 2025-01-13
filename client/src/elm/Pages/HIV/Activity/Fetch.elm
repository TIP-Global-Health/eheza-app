module Pages.HIV.Activity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HIVActivity.Model exposing (HIVActivity(..))
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getNCDEncountersForParticipant, getPrenatalEncountersForParticipant)
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Pages.HIV.Encounter.Fetch
import RemoteData


fetch : HIVEncounterId -> HIVActivity -> ModelIndexedDb -> List MsgIndexedDb
fetch id activity db =
    Pages.HIV.Encounter.Fetch.fetch id db
        ++ (if activity == Diagnostics then
                fetchForDiagnostics id db

            else
                []
           )


{-| For Diagnostics activity, we need to know if patient has ever tested positive to HIV.
For this, we need to fetch all measurements of all NCD and Prenatal encounters.
-}
fetchForDiagnostics : HIVEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetchForDiagnostics id db =
    let
        participantId =
            Dict.get id db.hivEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant
    in
    Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\participant ->
                let
                    personId =
                        participant.person

                    ncdParticipantsIds =
                        resolveIndividualParticipantsForPerson personId NCDEncounter db

                    prenatalParticipantsIds =
                        resolveIndividualParticipantsForPerson personId AntenatalEncounter db

                    ncdEncountersIds =
                        List.concatMap (getNCDEncountersForParticipant db >> List.map Tuple.first) ncdParticipantsIds

                    prenatalEncountersIds =
                        List.concatMap (getPrenatalEncountersForParticipant db >> List.map Tuple.first) prenatalParticipantsIds

                    fetchNCDMeasurementsMsgs =
                        List.map Backend.Model.FetchNCDMeasurements ncdEncountersIds

                    fetchPrenatalMeasurementsMsgs =
                        List.map Backend.Model.FetchPrenatalMeasurements prenatalEncountersIds
                in
                [ Backend.Model.FetchIndividualEncounterParticipantsForPerson personId
                , Backend.Model.FetchNCDEncountersForParticipants ncdParticipantsIds
                , Backend.Model.FetchPrenatalEncountersForParticipants prenatalParticipantsIds
                ]
                    ++ fetchNCDMeasurementsMsgs
                    ++ fetchPrenatalMeasurementsMsgs
            )
        |> Maybe.withDefault []
