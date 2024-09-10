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


wellChildActivityFromMapping : String -> Maybe WellChildActivity
wellChildActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just WellChildAlbendazole

        "b" ->
            Just WellChildBCGImmunisation

        "c" ->
            Just WellChildCaring

        "d" ->
            Just WellChildContributingFactors

        "e" ->
            Just WellChildDTPImmunisation

        "f" ->
            Just WellChildECD

        "g" ->
            Just WellChildFeeding

        "h" ->
            Just WellChildFollowUp

        "i" ->
            Just WellChildFoodSecurity

        "j" ->
            Just WellChildHeadCircumference

        "k" ->
            Just WellChildHealthEducation

        "l" ->
            Just WellChildHeight

        "m" ->
            Just WellChildHygiene

        "n" ->
            Just WellChildIPVImmunisation

        "o" ->
            Just WellChildMebendezole

        "p" ->
            Just WellChildMRImmunisation

        "q" ->
            Just WellChildMUAC

        "r" ->
            Just WellChildNCDA

        "s" ->
            Just WellChildNextVisit

        "t" ->
            Just WellChildNutrition

        "u" ->
            Just WellChildOPVImmunisation

        "v" ->
            Just WellChildPCV13Immunisation

        "w" ->
            Just WellChildPhoto

        "x" ->
            Just WellChildPregnancySummary

        "y" ->
            Just WellChildRotarixImmunisation

        "z" ->
            Just WellChildSendToHC

        "1" ->
            Just WellChildSymptomsReview

        "2" ->
            Just WellChildVitals

        "3" ->
            Just WellChildVitaminA

        "4" ->
            Just WellChildWeight

        "5" ->
            Just WellChildHPVImmunisation

        "6" ->
            Just WellChildDTPSAImmunisation

        _ ->
            Nothing


homeVisitActivityFromMapping : String -> Maybe HomeVisitActivity
homeVisitActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just HomeVisitCaring

        "b" ->
            Just HomeVisitFeeding

        "c" ->
            Just HomeVisitFoodSecurity

        "d" ->
            Just HomeVisitHygiene

        _ ->
            Nothing


childScoreboardActivityFromMapping : String -> Maybe ChildScoreboardActivity
childScoreboardActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just ChildScoreboardNCDA

        "b" ->
            Just ChildScoreboardBCGImmunisation

        "c" ->
            Just ChildScoreboardDTPImmunisation

        "i" ->
            Just ChildScoreboardDTPSAImmunisation

        "d" ->
            Just ChildScoreboardIPVImmunisation

        "e" ->
            Just ChildScoreboardMRImmunisation

        "f" ->
            Just ChildScoreboardOPVImmunisation

        "g" ->
            Just ChildScoreboardPCV13Immunisation

        "h" ->
            Just ChildScoreboardRotarixImmunisation

        _ ->
            Nothing


ncdActivityFromMapping : String -> Maybe NCDActivity
ncdActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just NCDCoreExam

        "b" ->
            Just NCDCoMorbidities

        "c" ->
            Just NCDCreatinineTest

        "d" ->
            Just NCDDangerSigns

        "e" ->
            Just NCDFamilyHistory

        "f" ->
            Just NCDFamilyPlanning

        "g" ->
            Just NCDHba1cTest

        "h" ->
            Just NCDHealthEducation

        "i" ->
            Just NCDHivTest

        "j" ->
            Just NCDLipidPanelTest

        "k" ->
            Just NCDLiverFunctionTest

        "l" ->
            Just NCDMedicationDistribution

        "m" ->
            Just NCDMedicationHistory

        "n" ->
            Just NCDOutsideCare

        "o" ->
            Just NCDPregnancyTest

        "p" ->
            Just NCDRandomBloodSugarTest

        "q" ->
            Just NCDReferral

        "r" ->
            Just NCDSocialHistory

        "s" ->
            Just NCDSymptomReview

        "t" ->
            Just NCDUrineDipstickTest

        "u" ->
            Just NCDVitals

        "v" ->
            Just NCDCreatinineTestResult

        "w" ->
            Just NCDLipidPanelTestResult

        "x" ->
            Just NCDLiverFunctionTestResult

        "y" ->
            Just NCDRandomBloodSugarTestResult

        "z" ->
            Just NCDUrineDipstickTestResult

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
