module Backend.NutritionEncounter.Utils exposing (generatePreviousMeasurementsForChild, generatePreviousValuesForChild, resolveNutritionParticipantForChild)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import NutritionActivity.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)


generatePreviousMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
generatePreviousMeasurementsForChild childId db =
    resolveNutritionParticipantForChild childId db
        |> Maybe.map
            (\participantId ->
                Dict.get participantId db.nutritionEncountersByParticipant
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.toList
                            >> List.filterMap
                                (\( encounterId, encounter ) ->
                                    case Dict.get encounterId db.nutritionMeasurements of
                                        Just (Success data) ->
                                            Just ( encounter.startDate, ( encounterId, data ) )

                                        _ ->
                                            Nothing
                                )
                            -- Most recent date to least recent date.
                            >> List.sortWith
                                (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))
                        )
                    |> RemoteData.withDefault []
            )
        |> Maybe.withDefault []


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
