module Pages.Relationship.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id1 id2 db =
    let
        familyMembers1 =
            EveryDict.get id1 db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (EveryDictList.values >> List.map (.relatedTo >> FetchPerson))
                |> RemoteData.withDefault []

        familyMembers2 =
            EveryDict.get id2 db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (EveryDictList.values >> List.map (.relatedTo >> FetchPerson))
                |> RemoteData.withDefault []

        relationships =
            [ FetchRelationshipsForPerson id1
            , FetchRelationshipsForPerson id2
            ]

        people =
            [ FetchPerson id1
            , FetchPerson id2
            ]
    in
    List.concat
        [ familyMembers1
        , familyMembers2
        , relationships
        , people
        ]
