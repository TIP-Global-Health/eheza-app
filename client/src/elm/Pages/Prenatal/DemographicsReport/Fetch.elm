module Pages.Prenatal.DemographicsReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch personId db =
    let
        fetchChildrenMsgs =
            Dict.get personId db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (Dict.values
                        >> List.filter (.relatedBy >> (==) MyChild)
                        >> List.map (.relatedTo >> FetchPerson)
                    )
                |> RemoteData.withDefault []
    in
    [ FetchHealthCenters
    , FetchPerson personId
    , FetchRelationshipsForPerson personId
    ]
        ++ fetchChildrenMsgs
