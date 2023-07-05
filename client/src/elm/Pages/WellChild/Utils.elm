module Pages.WellChild.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementDateMeasuredFunc, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant, sortTuplesByDateDesc)
import Backend.WellChildActivity.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.WellChild.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


generatePreviousMeasurements : Maybe WellChildEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
generatePreviousMeasurements currentEncounterId participantId db =
    getWellChildEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- We do not want to get data of current encounter.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId db.wellChildMeasurements of
                        Just (Success data) ->
                            Just ( encounter.startDate, ( encounterId, data ) )

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortTuplesByDateDesc


generateGroupNutritionAssessmentEntries : ChildMeasurementList -> List ( NominalDate, List NutritionAssessment )
generateGroupNutritionAssessmentEntries measurementList =
    let
        assessmentsFromNutrition =
            Dict.values measurementList.nutritions
                |> List.filterMap
                    (\nutrition -> filterNutritionAssessmentsFromNutritionValue nutrition.dateMeasured nutrition.value)

        assessmentsFromFollowUp =
            Dict.values measurementList.followUp
                |> List.filterMap
                    (\followUp -> filterNutritionAssessmentsFromFollowUpValue followUp.dateMeasured followUp.value)
    in
    mergeNutritionAssessmentEntries
        assessmentsFromNutrition
        assessmentsFromFollowUp


generateIndividualNutritionAssessmentEntries :
    List
        { c
            | nutrition :
                Maybe
                    ( id1
                    , { v1
                        | dateMeasured : NominalDate
                        , value : NutritionValue
                      }
                    )
            , followUp :
                Maybe
                    ( id2
                    , { v2
                        | dateMeasured : NominalDate
                        , value : FollowUpValue
                      }
                    )
        }
    -> List ( NominalDate, List NutritionAssessment )
generateIndividualNutritionAssessmentEntries measurementList =
    let
        assessmentsFromNutrition =
            List.map
                (\measurements ->
                    Maybe.map2 filterNutritionAssessmentsFromNutritionValue
                        (getMeasurementDateMeasuredFunc measurements.nutrition)
                        (getMeasurementValueFunc measurements.nutrition)
                        |> Maybe.Extra.join
                )
                measurementList
                |> Maybe.Extra.values

        assessmentsFromFollowUp =
            List.map
                (\measurements ->
                    Maybe.map2 filterNutritionAssessmentsFromFollowUpValue
                        (getMeasurementDateMeasuredFunc measurements.followUp)
                        (getMeasurementValueFunc measurements.followUp)
                        |> Maybe.Extra.join
                )
                measurementList
                |> Maybe.Extra.values
    in
    mergeNutritionAssessmentEntries
        assessmentsFromNutrition
        assessmentsFromFollowUp


filterNutritionAssessmentsFromNutritionValue : NominalDate -> NutritionValue -> Maybe ( NominalDate, List NutritionAssessment )
filterNutritionAssessmentsFromNutritionValue dateMeasured value =
    let
        assesments =
            EverySet.toList value.assesment
                |> List.filterMap
                    (\assesment ->
                        case assesment of
                            NoNutritionAssessment ->
                                Nothing

                            AssesmentMalnutritionSigns _ ->
                                Just <| AssesmentMalnutritionSigns (EverySet.toList value.signs)

                            _ ->
                                Just assesment
                    )
    in
    if List.isEmpty assesments then
        Nothing

    else
        Just ( dateMeasured, assesments )


filterNutritionAssessmentsFromFollowUpValue : NominalDate -> FollowUpValue -> Maybe ( NominalDate, List NutritionAssessment )
filterNutritionAssessmentsFromFollowUpValue dateMeasured value =
    let
        assesments =
            EverySet.toList value.assesment
                |> List.filterMap
                    (\assesment ->
                        case assesment of
                            NoNutritionAssessment ->
                                Nothing

                            _ ->
                                Just assesment
                    )
    in
    if List.isEmpty assesments then
        Nothing

    else
        Just ( dateMeasured, assesments )


mergeNutritionAssessmentEntries :
    List ( NominalDate, List NutritionAssessment )
    -> List ( NominalDate, List NutritionAssessment )
    -> List ( NominalDate, List NutritionAssessment )
mergeNutritionAssessmentEntries list1 list2 =
    Dict.merge
        (\date assessments -> Dict.insert date assessments)
        (\date assessments1 assessments2 ->
            Dict.insert date
                ((assessments1 ++ assessments2)
                    |> EverySet.fromList
                    |> EverySet.toList
                )
        )
        (\date assessments -> Dict.insert date assessments)
        (Dict.fromList list1)
        (Dict.fromList list2)
        Dict.empty
        |> Dict.toList
