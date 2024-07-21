module Pages.Tuberculosis.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (diabetesBySugarCount, diabetesByUrineGlucose, getCurrentReasonForNonReferral, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getTuberculosisEncountersForParticipant)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity)
import Backend.TuberculosisActivity.Utils exposing (allActivities)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Pages.Tuberculosis.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.Tuberculosis.Encounter.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , maybeToBoolTask
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectCustomInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewInstructionsLabel
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.NominalDate exposing (sortByStartDateDesc)


generateAssembledData : TuberculosisEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.tuberculosisEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.tuberculosisMeasurements
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


generatePreviousEncountersData : Maybe TuberculosisEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List EncounterData
generatePreviousEncountersData currentEncounterId participantId db =
    getTuberculosisEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- If the ID of current encounter was provided,
                -- we do not want to get its data.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId db.tuberculosisMeasurements of
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


partitionActivities : NominalDate -> AssembledData -> ( List TuberculosisActivity, List TuberculosisActivity )
partitionActivities currentDate assembled =
    List.filter (expectActivity currentDate assembled) allActivities
        |> List.partition (activityCompleted currentDate assembled)
