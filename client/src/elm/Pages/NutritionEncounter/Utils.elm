module Pages.NutritionEncounter.Utils exposing (generateAssembledData, generatePreviousMeasurements, generatePreviousValuesForChild, resolveNutritionParticipantForChild)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime)
import Maybe.Extra exposing (isJust, unwrap)
import NutritionActivity.Model exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


generatePreviousMeasurements : NutritionEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> WebData (List ( NominalDate, NutritionMeasurements ))
generatePreviousMeasurements currentEncounterId participantId db =
    Dict.get participantId db.nutritionEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.map
            (Dict.toList
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter.
                        if encounterId == currentEncounterId then
                            Nothing

                        else
                            case Dict.get encounterId db.nutritionMeasurements of
                                Just (Success data) ->
                                    Just ( encounter.startDate, data )

                                _ ->
                                    Nothing
                    )
                -- Most recent date to least recent date.
                >> List.sortWith
                    (\( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date2 date1)
            )


generateAssembledData : NutritionEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.nutritionEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.nutritionMeasurements
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
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> RemoteData.withDefault []

        previousMeasurements =
            List.map Tuple.second previousMeasurementsWithDates
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


resolveNutritionParticipantForChild : PersonId -> ModelIndexedDb -> Maybe IndividualEncounterParticipantId
resolveNutritionParticipantForChild id db =
    Dict.get id db.individualParticipantsByPerson
        |> Maybe.withDefault NotAsked
        |> RemoteData.toMaybe
        |> Maybe.andThen
            (Dict.toList
                >> List.filter
                    (\( _, participant ) ->
                        participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                    )
                >> List.head
                >> Maybe.map Tuple.first
            )


generatePreviousValuesForChild : PersonId -> ModelIndexedDb -> PreviousMeasurementsValue
generatePreviousValuesForChild childId db =
    resolveNutritionParticipantForChild childId db
        |> Maybe.map
            (\participantId ->
                let
                    measurementsWithDates =
                        Dict.get participantId db.nutritionEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (Dict.toList
                                    >> List.filterMap
                                        (\( encounterId, encounter ) ->
                                            case Dict.get encounterId db.nutritionMeasurements of
                                                Just (Success data) ->
                                                    Just ( encounter.startDate, data )

                                                _ ->
                                                    Nothing
                                        )
                                    -- Most recent date to least recent date.
                                    >> List.sortWith
                                        (\( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date2 date1)
                                )
                            |> RemoteData.withDefault []

                    previuosHeight =
                        measurementsWithDates
                            |> List.filterMap
                                (\( date, measurements ) ->
                                    measurements.height
                                        |> Maybe.map (\measurement -> ( date, Tuple.second measurement |> .value ))
                                )
                            |> List.head

                    previousMuac =
                        measurementsWithDates
                            |> List.filterMap
                                (\( date, measurements ) ->
                                    measurements.muac
                                        |> Maybe.map (\measurement -> ( date, Tuple.second measurement |> .value ))
                                )
                            |> List.head

                    previousWeight =
                        measurementsWithDates
                            |> List.filterMap
                                (\( date, measurements ) ->
                                    measurements.weight
                                        |> Maybe.map (\measurement -> ( date, Tuple.second measurement |> .value ))
                                )
                            |> List.head
                in
                PreviousMeasurementsValue previuosHeight previousMuac previousWeight
            )
        |> Maybe.withDefault (PreviousMeasurementsValue Nothing Nothing Nothing)
