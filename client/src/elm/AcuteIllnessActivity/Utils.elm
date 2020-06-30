module AcuteIllnessActivity.Utils exposing (activityCompleted, decodeActivityFromString, encodeActivityAsString, expectActivity, getActivityIcon, getAllActivities)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import AcuteIllnessActivity.Model exposing (..)
import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements, SymptomsGeneralSign(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllnessEncounter.Utils exposing (exposureTasksCompleted)
import Translate exposing (Language, translate)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
encodeActivityAsString : AcuteIllnessActivity -> String
encodeActivityAsString activity =
    case activity of
        AcuteIllnessSymptoms ->
            "symptoms"

        AcuteIllnessPhysicalExam ->
            "physical-exam"

        AcuteIllnessPriorTreatment ->
            "prior-treatment"

        AcuteIllnessLaboratory ->
            "laboratory"

        AcuteIllnessExposure ->
            "exposure"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe AcuteIllnessActivity
decodeActivityFromString s =
    case s of
        "symptoms" ->
            Just AcuteIllnessSymptoms

        "physical-exam" ->
            Just AcuteIllnessPhysicalExam

        "prior-treatment" ->
            Just AcuteIllnessPriorTreatment

        "laboratory" ->
            Just AcuteIllnessLaboratory

        "exposure" ->
            Just AcuteIllnessExposure

        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivity : AcuteIllnessActivity
defaultActivity =
    AcuteIllnessSymptoms


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : AcuteIllnessActivity -> String
getActivityIcon activity =
    encodeActivityAsString activity


getAllActivities : List AcuteIllnessActivity
getAllActivities =
    [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPriorTreatment, AcuteIllnessPhysicalExam, AcuteIllnessLaboratory ]


expectActivity : NominalDate -> AcuteIllnessMeasurements -> Bool -> AcuteIllnessActivity -> Bool
expectActivity currentDate measurements covid19Suspected activity =
    case activity of
        AcuteIllnessLaboratory ->
            let
                feverToday =
                    measurements.vitals
                        |> Maybe.map
                            (\measurement ->
                                let
                                    bodyTemperature =
                                        Tuple.second measurement
                                            |> .value
                                            |> .bodyTemperature
                                in
                                bodyTemperature > 37.5
                            )
                        |> Maybe.withDefault False

                symptomaticFever =
                    measurements.symptomsGeneral
                        |> Maybe.map
                            (\measurement ->
                                let
                                    feverPeriod =
                                        Tuple.second measurement
                                            |> .value
                                            |> Dict.get SymptomGeneralFever
                                            |> Maybe.withDefault 0
                                in
                                feverPeriod > 0
                            )
                        |> Maybe.withDefault False
            in
            not covid19Suspected
                && List.all (activityCompleted measurements covid19Suspected) [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPhysicalExam ]
                && (feverToday || symptomaticFever)

        _ ->
            True


activityCompleted : AcuteIllnessMeasurements -> Bool -> AcuteIllnessActivity -> Bool
activityCompleted measurements isSuspected activity =
    case activity of
        AcuteIllnessSymptoms ->
            isJust measurements.symptomsGeneral
                && isJust measurements.symptomsRespiratory
                && isJust measurements.symptomsGI

        AcuteIllnessPhysicalExam ->
            isJust measurements.vitals

        AcuteIllnessPriorTreatment ->
            isJust measurements.treatmentReview

        AcuteIllnessLaboratory ->
            isJust measurements.malariaTesting

        AcuteIllnessExposure ->
            exposureTasksCompleted measurements isSuspected
