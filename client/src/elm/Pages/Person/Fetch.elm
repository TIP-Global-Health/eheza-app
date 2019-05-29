module Pages.Person.Fetch exposing (fetch, fetchForCreateForm)

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
                |> RemoteData.map (EveryDictList.values >> List.map (.relatedTo >> FetchPerson))
                |> RemoteData.withDefault []
    in
    familyMembers
        ++ [ FetchPerson id
           , FetchRelationshipsForPerson id
           ]


fetchForCreateForm : Maybe PersonId -> List MsgIndexedDb
fetchForCreateForm related =
    FetchHealthCenters
        :: List.filterMap identity
            [ Maybe.map FetchPerson related
            ]
