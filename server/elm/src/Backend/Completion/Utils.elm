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
            Just NCDHIVTest

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


hivActivityFromMapping : String -> Maybe HIVActivity
hivActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just HIVDiagnostics

        "b" ->
            Just HIVFollowUp

        "c" ->
            Just HIVHealthEducation

        "d" ->
            Just HIVMedication

        "e" ->
            Just HIVReferral

        "f" ->
            Just HIVSymptomReview

        "g" ->
            Just HIVTreatmentReview

        _ ->
            Nothing


tuberculosisActivityFromMapping : String -> Maybe TuberculosisActivity
tuberculosisActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just TuberculosisDiagnostics

        "b" ->
            Just TuberculosisDOT

        "c" ->
            Just TuberculosisFollowUp

        "d" ->
            Just TuberculosisHealthEducation

        "e" ->
            Just TuberculosisMedication

        "f" ->
            Just TuberculosisReferral

        "g" ->
            Just TuberculosisSymptomReview

        "h" ->
            Just TuberculosisTreatmentReview

        _ ->
            Nothing


prenatalActivityFromMapping : String -> Maybe PrenatalActivity
prenatalActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just PrenatalAppointmentConfirmation

        "b" ->
            Just PrenatalBirthPlan

        "c" ->
            Just PrenatalBreastExam

        "d" ->
            Just PrenatalCorePhysicalExam

        "e" ->
            Just PrenatalDangerSigns

        "f" ->
            Just PrenatalLastMenstrualPeriod

        "g" ->
            Just PrenatalMedicalHistory

        "h" ->
            Just PrenatalMedication

        "i" ->
            Just PrenatalObstetricHistory

        "j" ->
            Just PrenatalObstetricHistoryStep2

        "k" ->
            Just PrenatalObstetricalExam

        "l" ->
            Just PrenatalPregnancyTesting

        "m" ->
            Just PrenatalBloodGprsTest

        "n" ->
            Just PrenatalBreastfeeding

        "o" ->
            Just PrenatalFamilyPlanning

        "p" ->
            Just PrenatalFollowUp

        "q" ->
            Just PrenatalGuExam

        "r" ->
            Just PrenatalHealthEducation

        "s" ->
            Just PrenatalHemoglobinTest

        "t" ->
            Just PrenatalHepatitisBTest

        "u" ->
            Just PrenatalHIVPCRTest

        "v" ->
            Just PrenatalHIVTest

        "w" ->
            Just PrenatalMalariaTest

        "x" ->
            Just PrenatalMedicationDistribution

        "y" ->
            Just PrenatalMentalHealth

        "z" ->
            Just PrenatalNutrition

        "0" ->
            Just PrenatalOutsideCare

        "1" ->
            Just PrenatalPartnerHIVTest

        "2" ->
            Just PrenatalPhoto

        "3" ->
            Just PrenatalRandomBloodSugarTest

        "4" ->
            Just PrenatalSendToHC

        "5" ->
            Just PrenatalSpecialityCare

        "6" ->
            Just PrenatalSymptomReview

        "7" ->
            Just PrenatalSyphilisTest

        "8" ->
            Just PrenatalTetanusImmunisation

        "9" ->
            Just PrenatalUrineDipstickTest

        "@" ->
            Just PrenatalResource

        "#" ->
            Just PrenatalSocialHistory

        "$" ->
            Just PrenatalVitals

        "^" ->
            Just PrenatalTreatmentReview

        "&" ->
            Just PrenatalPregnancyOutcome

        "*" ->
            Just PrenatalPostpartumTreatmentReview

        "$+" ->
            Just PrenatalVitalsRecheck

        "m+" ->
            Just PrenatalBloodGprsTestResult

        "s+" ->
            Just PrenatalHemoglobinTestResult

        "t+" ->
            Just PrenatalHepatitisBTestResult

        "u+" ->
            Just PrenatalHIVPCRTestResult

        "v+" ->
            Just PrenatalHIVTestResult

        "w+" ->
            Just PrenatalMalariaTestResult

        "1+" ->
            Just PrenatalPartnerHIVTestResult

        "3+" ->
            Just PrenatalRandomBloodSugarTestResult

        "7+" ->
            Just PrenatalSyphilisTestResult

        "9+" ->
            Just PrenatalUrineDipstickTestResult

        "a1" ->
            Just PrenatalCalcium

        "b1" ->
            Just PrenatalFolate

        "c1" ->
            Just PrenatalIron

        "d1" ->
            Just PrenatalMebendazole

        "e1" ->
            Just PrenatalMMS

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
