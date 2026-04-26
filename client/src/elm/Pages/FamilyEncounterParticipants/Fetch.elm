module Pages.FamilyEncounterParticipants.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))
import Components.PatientsSearchForm.Fetch
import Pages.FamilyEncounterParticipants.Model exposing (Model)


fetch : Model -> List MsgIndexedDb
fetch model =
    [ FetchHealthCenters, FetchVillages ]
        ++ Components.PatientsSearchForm.Fetch.fetch model
