module Pages.People.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator)
import Components.PatientsSearchForm.Fetch
import Maybe.Extra
import Pages.People.Model exposing (Model)
import Pages.Person.Fetch exposing (fetchSessionForInitiator)


fetch : Maybe PersonId -> Initiator -> Model -> List MsgIndexedDb
fetch relation initiator model =
    let
        fetchRelation =
            Maybe.map FetchPerson relation
                |> Maybe.Extra.toList
    in
    Components.PatientsSearchForm.Fetch.fetch model
        ++ fetchRelation
        ++ [ FetchHealthCenters, FetchVillages ]
        ++ fetchSessionForInitiator initiator
