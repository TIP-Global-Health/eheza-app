module Pages.People.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra


fetch : Maybe String -> Maybe PersonId -> List MsgIndexedDb
fetch search relation =
    let
        trimmed =
            search
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
    fetchPeople ++ fetchRelation
