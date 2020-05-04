module Pages.People.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import Pages.People.Model exposing (..)


fetch : Maybe PersonId -> Model -> List MsgIndexedDb
fetch relation model =
    let
        trimmed =
            model.search
                |> Maybe.withDefault ""
                |> String.trim

        fetchPeople =
            if String.isEmpty trimmed then
                []

            else
                [ FetchPeopleByName trimmed
                ]

        fetchRelation =
            relation
                |> Maybe.map FetchPerson
                |> Maybe.Extra.toList
    in
    fetchPeople ++ fetchRelation ++ [ FetchVillages ]
