module Pages.People.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator)
import Components.PatientsSearchForm.Fetch
import Pages.People.Model exposing (Model)
import Pages.Person.Fetch exposing (fetchSessionForInitiator)


fetch : Maybe PersonId -> Initiator -> Model -> List MsgIndexedDb
fetch relation initiator model =
    let
        -- The relationships let the results leave out people already related.
        fetchRelation =
            Maybe.map (\personId -> [ FetchPerson personId, FetchRelationshipsForPerson personId ]) relation
                |> Maybe.withDefault []
    in
    Components.PatientsSearchForm.Fetch.fetch model
        ++ fetchRelation
        ++ [ FetchHealthCenters, FetchVillages ]
        ++ fetchSessionForInitiator initiator
