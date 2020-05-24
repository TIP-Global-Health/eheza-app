module Backend.Village.Utils exposing (getVillageById, getVillageClinicId, getVillageHealthCenterId, personLivesInVillage)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
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


personLivesInVillage : Person -> ModelIndexedDb -> VillageId -> Bool
personLivesInVillage person db villageId =
    getVillageById db villageId
        |> Maybe.map
            (\village ->
                (Just village.province == person.province)
                    && (Just village.district == person.district)
                    && (Just village.sector == person.sector)
                    && (Just village.cell == person.cell)
                    && (Just village.village == person.village)
            )
        |> Maybe.withDefault False
