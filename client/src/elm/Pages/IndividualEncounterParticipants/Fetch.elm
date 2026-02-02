module Pages.IndividualEncounterParticipants.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))
import Components.PatientsSearchForm.Fetch
import Pages.IndividualEncounterParticipants.Model exposing (Model)


fetch : Model -> List MsgIndexedDb
fetch model =
    [ FetchHealthCenters, FetchVillages ]
        ++ Components.PatientsSearchForm.Fetch.fetch model
