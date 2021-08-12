module Pages.WellChildParticipant.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        individualParticipants =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        -- We need the whole Well Child data for person at this
        -- early stage, to be able to determine if to allow
        -- an encounter (based on immunisation and medication data).
        ++ Backend.NutritionEncounter.Fetch.fetch id db
