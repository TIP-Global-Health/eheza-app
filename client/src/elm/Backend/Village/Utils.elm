module Backend.Village.Utils exposing (..)

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
    let
        valuesMatch villageValue personValue =
            case personValue of
                Just value ->
                    String.toLower value == String.toLower villageValue

                Nothing ->
                    False
    in
    getVillageById db villageId
        |> Maybe.map
            (\village ->
                valuesMatch village.province person.province
                    && valuesMatch village.district person.district
                    && valuesMatch village.sector person.sector
                    && valuesMatch village.cell person.cell
                    && valuesMatch village.village person.village
            )
        |> Maybe.withDefault False


getVillageIdByGeoFields : ModelIndexedDb -> String -> String -> String -> String -> String -> Maybe VillageId
getVillageIdByGeoFields db province district sector cell village_ =
    RemoteData.toMaybe db.villages
        |> Maybe.andThen
            (Dict.toList
                >> List.filter
                    (\( _, village ) ->
                        (String.toLower village.province == String.toLower province)
                            && (String.toLower village.district == String.toLower district)
                            && (String.toLower village.sector == String.toLower sector)
                            && (String.toLower village.cell == String.toLower cell)
                            && (String.toLower village.village == String.toLower village_)
                    )
                >> List.head
                >> Maybe.map Tuple.first
            )
