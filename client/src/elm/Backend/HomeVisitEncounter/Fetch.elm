module Backend.HomeVisitEncounter.Fetch exposing (fetchForChild)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Utils exposing (resolveHomeVisitParticipantForChild)
import Backend.IndividualEncounterType.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


fetchForChild : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForChild id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id HomeVisitEncounter db

        encountersIds =
            participantId
                |> Maybe.map
                    (\participantId_ ->
                        Dict.get participantId_ db.homeVisitEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map Dict.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            encountersIds
                |> List.map FetchHomeVisitMeasurements
    in
    List.filterMap identity
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchHomeVisitEncountersForParticipant participantId
        , Just <| FetchPerson id

        -- We need this, so we can resolve the homeVisit participant for child.
        , Just <| FetchIndividualEncounterParticipantsForPerson id
        ]
        ++ fetchMeasurements
