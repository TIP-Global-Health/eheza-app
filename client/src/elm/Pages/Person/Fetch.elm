module Pages.Person.Fetch exposing (fetch, fetchForCreateForm)

import AllDict
import AllDictList
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        familyMembers =
            AllDict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (AllDictList.values >> List.map (.relatedTo >> FetchPerson))
                |> RemoteData.withDefault []
    in
    familyMembers
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
