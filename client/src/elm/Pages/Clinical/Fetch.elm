module Pages.Clinical.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Village.Utils exposing (getVillageClinicId)
import RemoteData exposing (RemoteData(..))


fetch : Maybe VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch maybeVillageId db =
    maybeVillageId
        |> Maybe.map
            (\villageId ->
                List.filterMap identity
                    [ Just FetchClinics
                    , Maybe.map FetchSessionsByClinic (getVillageClinicId villageId db)
                    ]
                    |> List.append fetchForAll
            )
        |> Maybe.withDefault fetchForAll


fetchForAll : List MsgIndexedDb
fetchForAll =
    [ FetchSyncData
    , FetchHealthCenters
    , FetchVillages
    ]
