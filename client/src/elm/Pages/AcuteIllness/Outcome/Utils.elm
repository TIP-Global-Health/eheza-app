module Pages.AcuteIllness.Outcome.Utils exposing (generateOutcomeData)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.AcuteIllness.Encounter.Utils exposing (generateAssembledData)
import Pages.Report.Utils exposing (getAcuteIllnessDiagnosisForEncounters, getAcuteIllnessEncountersForParticipant)
import RemoteData exposing (RemoteData(..), WebData)
import SyncManager.Model exposing (SiteFeature)


{-| The data the outcome page shows for an illness, assembled on its latest
visit. Its diagnosis is the one saved at the latest visit that has one, as on
the participant page's outcome button.
-}
generateOutcomeData : NominalDate -> EverySet SiteFeature -> IndividualEncounterParticipantId -> Bool -> ModelIndexedDb -> WebData AssembledData
generateOutcomeData currentDate features id isChw db =
    let
        -- Latest first.
        encounters =
            getAcuteIllnessEncountersForParticipant db id
    in
    List.head encounters
        |> Maybe.map
            (\( encounterId, _ ) ->
                generateAssembledData currentDate features encounterId isChw db
                    |> RemoteData.map (\data -> { data | diagnosis = getAcuteIllnessDiagnosisForEncounters encounters })
            )
        |> Maybe.withDefault NotAsked
