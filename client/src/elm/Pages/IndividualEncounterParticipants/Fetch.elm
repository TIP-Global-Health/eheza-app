module Pages.IndividualEncounterParticipants.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import Pages.IndividualEncounterParticipants.Model exposing (..)


fetch : IndividualEncounterType -> Model -> List MsgIndexedDb
fetch encounterType model =
    let
        trimmed =
            model.search
                |> Maybe.withDefault ""
                |> String.trim
    in
    [ FetchSyncData, FetchHealthCenters, FetchVillages ]
        ++ (if String.isEmpty trimmed then
                []

            else
                [ FetchPeopleByName trimmed ]
           )
