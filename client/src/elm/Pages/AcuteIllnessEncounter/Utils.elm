module Pages.AcuteIllnessEncounter.Utils exposing (exposureTasksCompleted, generateAssembledData, resolveExposureTasks, suspectedCovid19Case)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , AcuteIllnessVitalsValue
        , ExposureSign(..)
        , HCContactSign(..)
        , HCContactValue
        , HCRecomendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaTestingSign(..)
        , ReasonForNotIsolating(..)
        , ResponsePeriod(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllnessActivity.Model exposing (ExposureTask(..))
import Pages.AcuteIllnessEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)


suspectedCovid19Case : AcuteIllnessMeasurements -> Bool
suspectedCovid19Case measurements =
    let
        calculateSymptoms measurement_ default =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> .value

                            setSize =
                                Dict.size set
                        in
                        case setSize of
                            1 ->
                                if Dict.member default set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        calculateSigns measurement_ default =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> .value

                            setSize =
                                EverySet.size set
                        in
                        case setSize of
                            1 ->
                                if EverySet.member default set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        totalSymptoms =
            calculateSymptoms measurements.symptomsGeneral NoSymptomsGeneral
                + calculateSymptoms measurements.symptomsRespiratory NoSymptomsRespiratory
                + calculateSymptoms measurements.symptomsGI NoSymptomsGI

        totalSigns =
            calculateSigns measurements.travelHistory NoTravelHistorySigns
                + calculateSigns measurements.exposure NoExposureSigns

        fever =
            measurements.vitals
                |> Maybe.map
                    (\measurement ->
                        let
                            bodyTemperature =
                                Tuple.second measurement |> .value |> .bodyTemperature
                        in
                        bodyTemperature > 37.5
                    )
                |> Maybe.withDefault False
    in
    totalSigns > 0 && (fever || totalSymptoms > 1)


generateAssembledData : AcuteIllnessEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.acuteIllnessEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.acuteIllnessMeasurements
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


generatePreviousMeasurements : AcuteIllnessEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> WebData (List ( NominalDate, AcuteIllnessMeasurements ))
generatePreviousMeasurements currentEncounterId participantId db =
    Dict.get participantId db.acuteIllnessEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.map
            (Dict.toList
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter.
                        if encounterId == currentEncounterId then
                            Nothing

                        else
                            case Dict.get encounterId db.acuteIllnessMeasurements of
                                Just (Success data) ->
                                    Just ( encounter.startDate, data )

                                _ ->
                                    Nothing
                    )
                >> List.sortWith
                    (\( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date1 date2)
            )


resolveExposureTasks : AcuteIllnessMeasurements -> Bool -> List ExposureTask
resolveExposureTasks measurements isSuspected =
    let
        expectTask task =
            if isSuspected then
                case task of
                    ExposureTravel ->
                        isJust measurements.travelHistory

                    ExposureExposure ->
                        isJust measurements.exposure

                    ExposureIsolation ->
                        True

                    ExposureContactHC ->
                        True

            else
                case task of
                    ExposureTravel ->
                        True

                    ExposureExposure ->
                        True

                    ExposureIsolation ->
                        False

                    ExposureContactHC ->
                        False
    in
    [ ExposureTravel, ExposureExposure, ExposureIsolation, ExposureContactHC ]
        |> List.filter expectTask


exposureTasksCompleted : AcuteIllnessMeasurements -> Bool -> Bool
exposureTasksCompleted measurements isSuspected =
    resolveExposureTasks measurements isSuspected
        |> List.all
            (\task ->
                case task of
                    ExposureTravel ->
                        isJust measurements.travelHistory

                    ExposureExposure ->
                        isJust measurements.exposure

                    ExposureIsolation ->
                        isJust measurements.isolation

                    ExposureContactHC ->
                        isJust measurements.hcContact
            )
