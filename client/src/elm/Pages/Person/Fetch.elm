module Pages.Person.Fetch exposing (fetch, fetchForCreateOrEdit)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EverySet
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        familyMembers =
            Dict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (Dict.values >> List.map (.relatedTo >> FetchPerson))
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
            Dict.get id db.participantsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (Dict.values
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


fetchForCreateOrEdit : Maybe PersonId -> List MsgIndexedDb
fetchForCreateOrEdit related =
    FetchHealthCenters
        :: List.filterMap identity
            [ Maybe.map FetchPerson related
            ]
