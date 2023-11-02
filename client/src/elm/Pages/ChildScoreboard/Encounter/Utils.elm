module Pages.ChildScoreboard.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.ChildScoreboard.Utils exposing (generatePreviousMeasurements, generateVaccinationProgressDicts)
import RemoteData exposing (RemoteData(..), WebData)
import SyncManager.Model exposing (Site)


generateAssembledData : Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData site id db =
    let
        encounter =
            Dict.get id db.childScoreboardEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.childScoreboardMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements (Just id) encounter_.participant db)
                |> Maybe.withDefault []

        assembledWithEmptyVaccinationDicts =
            RemoteData.map AssembledData (Success id)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success previousMeasurementsWithDates)
                |> RemoteData.andMap (Success Dict.empty)
                |> RemoteData.andMap (Success Dict.empty)
    in
    RemoteData.map
        (\assembled ->
            let
                ( vaccinationHistory, vaccinationProgress ) =
                    generateVaccinationProgressDicts site assembled db
            in
            { assembled | vaccinationHistory = vaccinationHistory, vaccinationProgress = vaccinationProgress }
        )
        assembledWithEmptyVaccinationDicts
