module Pages.HIV.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getHIVEncountersForParticipant)
import Pages.HIV.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.NominalDate exposing (sortByStartDateDesc)


generateAssembledData : HIVEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.hivEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.hivMeasurements
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

        previousEncountersData =
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousEncountersData (Just id) encounter_.participant db)
                |> Maybe.withDefault []

        initialEncounter =
            List.isEmpty previousEncountersData
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousEncountersData)
        |> RemoteData.andMap (Success initialEncounter)


generatePreviousEncountersData : Maybe HIVEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List EncounterData
generatePreviousEncountersData currentEncounterId participantId db =
    getHIVEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- If the ID of current encounter was provided,
                -- we do not want to get its data.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId db.hivMeasurements of
                        Just (Success measurements) ->
                            Just
                                { id = encounterId
                                , startDate = encounter.startDate
                                , measurements = measurements
                                }

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortByStartDateDesc
