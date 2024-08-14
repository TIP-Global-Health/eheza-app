module Pages.Clinical.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Village.Utils exposing (getVillageClinicId)
import Maybe.Extra


fetch : Maybe VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch maybeVillageId db =
    maybeVillageId
        |> Maybe.map
            (\villageId ->
                Maybe.Extra.values
                    [ Just FetchClinics
                    , Maybe.map FetchSessionsByClinic (getVillageClinicId villageId db)
                    ]
                    |> List.append fetchForAll
            )
        |> Maybe.withDefault fetchForAll


fetchForAll : List MsgIndexedDb
fetchForAll =
    [ FetchHealthCenters
    , FetchVillages
    ]
