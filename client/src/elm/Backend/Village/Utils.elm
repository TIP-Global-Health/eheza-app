module Backend.Village.Utils exposing (getVillageById, getVillageClinicId, getVillageHealthCenterId)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Village.Model exposing (..)
import RemoteData exposing (RemoteData(..))


getVillageClinicId : VillageId -> ModelIndexedDb -> Maybe ClinicId
getVillageClinicId villageId db =
    db.clinics
        |> RemoteData.toMaybe
        |> Maybe.map
            (Dict.toList
                >> List.filterMap
                    (\( clinicId, clinic ) ->
                        if clinic.villageId == Just villageId then
                            Just clinicId

                        else
                            Nothing
                    )
            )
        |> Maybe.andThen List.head


getVillageById : ModelIndexedDb -> VillageId -> Maybe Village
getVillageById db villageId =
    db.villages
        |> RemoteData.toMaybe
        |> Maybe.andThen (Dict.get villageId)


getVillageHealthCenterId : VillageId -> ModelIndexedDb -> Maybe HealthCenterId
getVillageHealthCenterId villageId db =
    getVillageById db villageId
        |> Maybe.map .healthCenterId
