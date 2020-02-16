module Pages.DemographicsReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import RemoteData exposing (RemoteData(..))


fetch : PrenatalEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            participantId
                |> Maybe.andThen (\id -> Dict.get id db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        children =
            personId
                |> Maybe.map
                    (\motherId ->
                        Dict.get motherId db.relationshipsByPerson
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (Dict.values
                                    >> List.filter (\person -> person.relatedBy == MyChild)
                                    >> List.map (.relatedTo >> FetchPerson >> Just)
                                )
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []
    in
    List.filterMap identity <|
        ([ Just <| FetchHealthCenters
         , Just <| FetchPrenatalEncounter id
         , Maybe.map FetchIndividualEncounterParticipant participantId
         , Maybe.map FetchPerson personId
         , Maybe.map FetchRelationshipsForPerson personId
         ]
            ++ children
        )
