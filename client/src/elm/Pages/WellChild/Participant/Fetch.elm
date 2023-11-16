module Pages.WellChild.Participant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        -- We need the whole Well Child data for person at this
        -- early stage, to be able to determine if to allow
        -- an encounter (based on immunisation and medication data).
        ++ Backend.NutritionEncounter.Fetch.fetch id db
