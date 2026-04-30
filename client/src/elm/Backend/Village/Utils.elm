module Backend.Village.Utils exposing (getVillageById, getVillageClinicId, getVillageHealthCenterId, getVillageIdByGeoFields, personLivesInVillage, resolveVillageResidents)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Village.Model exposing (Village)
import RemoteData


getVillageClinicId : VillageId -> ModelIndexedDb -> Maybe ClinicId
getVillageClinicId villageId db =
    RemoteData.toMaybe db.clinics
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
    RemoteData.toMaybe db.villages
        |> Maybe.andThen (Dict.get villageId)


getVillageHealthCenterId : VillageId -> ModelIndexedDb -> Maybe HealthCenterId
getVillageHealthCenterId villageId db =
    getVillageById db villageId
        |> Maybe.map .healthCenterId


personLivesInVillage : Person -> ModelIndexedDb -> VillageId -> Bool
personLivesInVillage person db villageId =
    getVillageById db villageId
        |> Maybe.map (isVillageResident person)
        |> Maybe.withDefault False


isVillageResident : Person -> Village -> Bool
isVillageResident person village =
    let
        valuesMatch villageValue personValue =
            case personValue of
                Just value ->
                    String.toLower value == String.toLower villageValue

                Nothing ->
                    False
    in
    valuesMatch village.province person.province
        && valuesMatch village.district person.district
        && valuesMatch village.sector person.sector
        && valuesMatch village.cell person.cell
        && valuesMatch village.village person.village


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


{-| Need to `FetchPeopleInVillage villageId` at Fetch, for this to work.
-}
resolveVillageResidents : VillageId -> ModelIndexedDb -> List PersonId
resolveVillageResidents villageId db =
    Dict.get villageId db.peopleInVillage
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.keys
        |> Maybe.withDefault []
