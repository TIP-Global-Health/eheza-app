module Pages.Person.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        familyMembers =
            EveryDict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (EveryDictList.values
                        >> List.map
                            (\relationship ->
                                [ FetchPerson relationship.person
                                , FetchPerson relationship.relatedTo
                                ]
                            )
                    )
                |> RemoteData.withDefault []
                |> List.concat
    in
    familyMembers
        ++ [ FetchPerson id
           , FetchRelationshipsForPerson id
           ]
