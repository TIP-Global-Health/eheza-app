module Pages.IndividualEncounterParticipants.Fetch exposing (fetch)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.IndividualEncounterParticipants.Model exposing (..)


fetch : IndividualEncounterType -> Model -> List MsgIndexedDb
fetch encounterType model =
    let
        trimmed =
            model.search
                |> Maybe.withDefault ""
                |> String.trim
    in
    if String.isEmpty trimmed then
        []

    else
        [ FetchPeopleByName trimmed ]
