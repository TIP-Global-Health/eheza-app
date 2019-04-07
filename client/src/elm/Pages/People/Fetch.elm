module Pages.People.Fetch exposing (fetch)

import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))


fetch : Maybe String -> List MsgIndexedDb
fetch search =
    let
        trimmed =
            search
                |> Maybe.withDefault ""
                |> String.trim
    in
    if String.isEmpty trimmed then
        []

    else
        [ FetchPeopleByName trimmed
        ]
