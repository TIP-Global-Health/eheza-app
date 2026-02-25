module Pages.FamilyEncounterParticipants.Fetch exposing (fetch)

import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterType)
import Backend.Model exposing (MsgIndexedDb(..))
import Components.PatientsSearchForm.Fetch
import Pages.FamilyEncounterParticipants.Model exposing (..)


fetch : FamilyEncounterType -> Model -> List MsgIndexedDb
fetch encounterType model =
    [ FetchHealthCenters, FetchVillages ]
        ++ Components.PatientsSearchForm.Fetch.fetch model
