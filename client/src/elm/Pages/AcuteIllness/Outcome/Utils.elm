module Pages.AcuteIllness.Outcome.Utils exposing (generateOutcomeData)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getAcuteIllnessEncountersForParticipant)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.AcuteIllness.Encounter.Utils exposing (generateAssembledData, getAcuteIllnessDiagnosisForParticipant)
import RemoteData exposing (RemoteData(..), WebData)
import SyncManager.Model exposing (SiteFeature)


{-| The data the outcome page shows for an illness. Its diagnosis is the one
saved at the latest visit that has one, as on the participant page's outcome
button.
-}
generateOutcomeData : NominalDate -> EverySet SiteFeature -> IndividualEncounterParticipantId -> Bool -> ModelIndexedDb -> WebData AssembledData
generateOutcomeData currentDate features id isChw db =
    getAcuteIllnessEncountersForParticipant db id
        |> List.head
        |> Maybe.map
            (\( encounterId, _ ) ->
                generateAssembledData currentDate features encounterId isChw db
            )
        |> Maybe.withDefault NotAsked
        |> RemoteData.map (\data -> { data | diagnosis = getAcuteIllnessDiagnosisForParticipant db id })
