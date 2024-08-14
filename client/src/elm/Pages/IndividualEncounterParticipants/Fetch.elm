module Pages.IndividualEncounterParticipants.Fetch exposing (fetch)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Model exposing (MsgIndexedDb(..))
import Pages.IndividualEncounterParticipants.Model exposing (..)


fetch : IndividualEncounterType -> Model -> List MsgIndexedDb
fetch encounterType model =
    let
        trimmed =
            Maybe.withDefault "" model.search
                |> String.trim
    in
    [ FetchHealthCenters, FetchVillages ]
        ++ (if String.isEmpty trimmed then
                []

            else
                [ FetchPeopleByName trimmed ]
           )
