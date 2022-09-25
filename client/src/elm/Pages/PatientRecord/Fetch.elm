module Pages.PatientRecord.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Person.Utils exposing (isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.AcuteIllness.Participant.Fetch
import RemoteData exposing (RemoteData)


fetch : NominalDate -> PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate personId db =
    let
        msgsByAge =
            Dict.get personId db.people
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (\person ->
                        if isPersonAnAdult currentDate person == Just False then
                            Backend.NutritionEncounter.Fetch.fetch personId db

                        else
                            fetchForAdult personId db
                    )
                |> Maybe.withDefault []
    in
    [ FetchPerson personId
    , FetchRelationshipsForPerson personId
    ]
        ++ Pages.AcuteIllness.Participant.Fetch.fetch personId db
        ++ msgsByAge


fetchForAdult : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForAdult personId db =
    let
        prenatalParticipantsIds =
            resolveIndividualParticipantsForPerson personId AntenatalEncounter db

        prenatalEncountersIds =
            List.map
                (\participantId ->
                    Dict.get participantId db.prenatalEncountersByParticipant
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map Dict.keys
                )
                prenatalParticipantsIds
                |> Maybe.Extra.values
                |> List.concat

        fetchPrenatalEncountersMsgs =
            List.map FetchPrenatalEncountersForParticipant prenatalParticipantsIds

        fetchPrenatalMeasurementsMsgs =
            List.map FetchPrenatalMeasurements prenatalEncountersIds

        fetchChildrenMsgs =
            Dict.get personId db.relationshipsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.values
                        >> List.filter (.relatedBy >> (==) MyChild)
                        >> List.map (.relatedTo >> FetchPerson)
                    )
                |> Maybe.withDefault []
    in
    [ FetchIndividualEncounterParticipantsForPerson personId
    , FetchMotherMeasurements personId
    ]
        ++ fetchPrenatalEncountersMsgs
        ++ fetchPrenatalMeasurementsMsgs
        ++ fetchChildrenMsgs
