module Backend.Completion.Utils exposing (..)

import Backend.Completion.Model exposing (..)


nutritionChildActivityFromMapping : String -> Maybe NutritionChildActivity
nutritionChildActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just NutritionHeight

        "b" ->
            Just NutritionNutrition

        "c" ->
            Just NutritionPhoto

        "d" ->
            Just NutritionWeight

        "e" ->
            Just NutritionMUAC

        "f" ->
            Just NutritionContributingFactors

        "g" ->
            Just NutritionFollowUp

        "h" ->
            Just NutritionHealthEducation

        "i" ->
            Just NutritionSendToHC

        "j" ->
            Just NutritionNCDA

        "k" ->
            Just NutritionChildFbf

        _ ->
            Nothing


nutritionMotherActivityFromMapping : String -> Maybe NutritionMotherActivity
nutritionMotherActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just NutritionFamilyPlanning

        "b" ->
            Just NutritionMotherFbf

        "c" ->
            Just NutritionLactation

        _ ->
            Nothing


acuteIllnessActivityFromMapping : String -> Maybe AcuteIllnessActivity
acuteIllnessActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just AcuteIllnessAcuteFindings

        "b" ->
            Just AcuteIllnessContactsTracing

        "c" ->
            Just AcuteIllnessCoreExam

        "d" ->
            Just AcuteIllnessDangerSigns

        "e" ->
            Just AcuteIllnessFollowUp

        "f" ->
            Just AcuteIllnessMUAC

        "g" ->
            Just AcuteIllnessNutrition

        "h" ->
            Just AcuteIllnessVitals

        "i" ->
            Just AcuteIllnessCall114

        "j" ->
            Just AcuteIllnessCOVIDTesting

        "k" ->
            Just AcuteIllnessExposure

        "l" ->
            Just AcuteIllnessContactHC

        "m" ->
            Just AcuteIllnessHealthEducation

        "n" ->
            Just AcuteIllnessIsolation

        "o" ->
            Just AcuteIllnessMalariaTesting

        "p" ->
            Just AcuteIllnessMedicationDistribution

        "q" ->
            Just AcuteIllnessSendToHC

        "r" ->
            Just AcuteIllnessSymptomsGeneral

        "s" ->
            Just AcuteIllnessSymptomsGI

        "t" ->
            Just AcuteIllnessSymptomsRespiratory

        "u" ->
            Just AcuteIllnessTravelHistory

        "v" ->
            Just AcuteIllnessPriorTreatment

        "w" ->
            Just AcuteIllnessOngoingTreatment

        _ ->
            Nothing


takenByToString : TakenBy -> String
takenByToString value =
    case value of
        TakenByNurse ->
            "nurse"

        TakenByCHW ->
            "chw"

        TakenByUnknown ->
            "unknown"


takenByFromString : String -> Maybe TakenBy
takenByFromString value =
    case value of
        "nurse" ->
            Just TakenByNurse

        "chw" ->
            Just TakenByCHW

        "unknown" ->
            Just TakenByUnknown

        _ ->
            Nothing
