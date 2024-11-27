module Pages.IndividualEncounterParticipants.Fetch exposing (fetch)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Model exposing (MsgIndexedDb(..))
import Components.PatientsSearchForm.Fetch
import Pages.IndividualEncounterParticipants.Model exposing (..)


fetch : IndividualEncounterType -> Model -> List MsgIndexedDb
fetch encounterType model =
    [ FetchHealthCenters, FetchVillages ]
        ++ Components.PatientsSearchForm.Fetch.fetch model
