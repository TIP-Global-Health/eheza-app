module Pages.Person.Fetch exposing (fetch, fetchForCreateForm)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import EverySet
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        familyMembers =
            EveryDict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (EveryDictList.values >> List.map (.relatedTo >> FetchPerson))
                |> RemoteData.withDefault []

        addParticipants participant accum =
            -- We add them both, and then as a final step remove the id itself,
            -- since we'll fetch that anyway.
            accum
                |> EverySet.insert participant.child
                |> EverySet.insert participant.adult
                >> EverySet.remove id

        -- We also need to fetch the person data for the other half of
        -- participant pairings even if not a family member.
        participantMembers =
            EveryDict.get id db.participantsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (EveryDict.values
                        >> List.foldl addParticipants EverySet.empty
                        >> EverySet.toList
                        >> List.map FetchPerson
                    )
                |> RemoteData.withDefault []
    in
    familyMembers
        ++ participantMembers
        ++ [ FetchPerson id
           , FetchRelationshipsForPerson id
           , FetchParticipantsForPerson id
           , FetchClinics
           ]


fetchForCreateForm : Maybe PersonId -> List MsgIndexedDb
fetchForCreateForm related =
    FetchHealthCenters
        :: List.filterMap identity
            [ Maybe.map FetchPerson related
            ]
