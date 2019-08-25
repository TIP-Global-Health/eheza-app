module Pages.PrenatalParticipants.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import Pages.PrenatalParticipants.Model exposing (..)


fetch : Model -> List MsgIndexedDb
fetch model =
    let
        trimmed =
            model.search
                |> Maybe.withDefault ""
                |> String.trim
    in
    if String.isEmpty trimmed then
        []

    else
        [ FetchPeopleByName trimmed
        ]
