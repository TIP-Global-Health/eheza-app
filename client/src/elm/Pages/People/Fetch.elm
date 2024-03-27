module Pages.People.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator)
import Maybe.Extra
import Pages.People.Model exposing (..)


fetch : Maybe PersonId -> Initiator -> Model -> List MsgIndexedDb
fetch relation initiator model =
    let
        trimmed =
            Maybe.withDefault "" model.search
                |> String.trim

        fetchPeople =
            if String.isEmpty trimmed then
                []

            else
                [ FetchPeopleByName trimmed ]

        fetchRelation =
            Maybe.map FetchPerson relation
                |> Maybe.Extra.toList
    in
    fetchPeople ++ fetchRelation ++ [ FetchHealthCenters, FetchVillages ]
