module Pages.People.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Components.PatientsSearchForm.Fetch
import Maybe.Extra
import Pages.People.Model exposing (..)


fetch : Maybe PersonId -> Model -> List MsgIndexedDb
fetch relation model =
    let
        fetchRelation =
            Maybe.map FetchPerson relation
                |> Maybe.Extra.toList
    in
    Components.PatientsSearchForm.Fetch.fetch model
        ++ fetchRelation
        ++ [ FetchHealthCenters, FetchVillages ]
