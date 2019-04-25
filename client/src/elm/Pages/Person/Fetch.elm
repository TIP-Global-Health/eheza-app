module Pages.Person.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> Maybe PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id relationId db =
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

        relation =
            relationId
                |> Maybe.map FetchPerson
                |> Maybe.Extra.toList
    in
    familyMembers
        ++ relation
        ++ [ FetchPerson id
           , FetchRelationshipsForPerson id
           ]
