module Components.PatientsSearchForm.Utils exposing (getSearchResults, getSearchValue)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Components.PatientsSearchForm.Model exposing (Model, PatientsSearchFormMode(..))
import RemoteData exposing (RemoteData(..), WebData)


getSearchResults : ModelIndexedDb -> Model -> WebData (Dict PersonId Person)
getSearchResults db model =
    let
        searchValue =
            getSearchValue model

        resultsDictFunc =
            case model.mode of
                ModeSearchByName ->
                    .personSearchesByName

                ModeSearchByNationalId ->
                    .personSearchesByNationalId
    in
    resultsDictFunc db
        |> Dict.get searchValue
        |> Maybe.withDefault NotAsked


getSearchValue : Model -> String
getSearchValue =
    .search >> Maybe.withDefault ""
