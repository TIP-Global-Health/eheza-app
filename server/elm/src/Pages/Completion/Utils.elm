module Pages.Completion.Utils exposing (..)

import App.Types exposing (Site(..))
import Backend.Completion.Model
    exposing
        ( AcuteIllnessActivity(..)
        , ChildScoreboardActivity(..)
        , HIVActivity(..)
        , HomeVisitActivity(..)
        , NCDActivity(..)
        , NutritionChildActivity(..)
        , NutritionMotherActivity(..)
        , PrenatalActivity(..)
        , TakenBy(..)
        , TuberculosisActivity(..)
        , WellChildActivity(..)
        , WellChildEncounterType(..)
        )
import Pages.Completion.Model exposing (ReportType(..))


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportAcuteIllness ->
            "acute-illness"

        ReportChildScoreboard ->
            "child-scoreboard"

        ReportHIV ->
            "hiv"

        ReportHomeVisit ->
            "home-visit"

        ReportNCD ->
            "ncd"

        ReportNewbornExam ->
            "newborn-exam"

        ReportNutritionGroup ->
            "nutrition-group"

        ReportNutritionIndividual ->
            "nutrition-individual"

        ReportPrenatal ->
            "prenatal"

        ReportTuberculosis ->
            "tuberculosis"

        ReportWellChild ->
            "well-child"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "acute-illness" ->
            Just ReportAcuteIllness

        "child-scoreboard" ->
            Just ReportChildScoreboard

        "hiv" ->
            Just ReportHIV

        "home-visit" ->
            Just ReportHomeVisit

        "ncd" ->
            Just ReportNCD

        "newborn-exam" ->
            Just ReportNewbornExam

        "nutrition-group" ->
            Just ReportNutritionGroup

        "nutrition-individual" ->
            Just ReportNutritionIndividual

        "prenatal" ->
            Just ReportPrenatal

        "tuberculosis" ->
            Just ReportTuberculosis

        "well-child" ->
            Just ReportWellChild

        _ ->
            Nothing


allAcuteIllnessActivities : List AcuteIllnessActivity
allAcuteIllnessActivities =
    [ AcuteIllnessAcuteFindings
    , AcuteIllnessContactsTracing
    , AcuteIllnessCoreExam
    , AcuteIllnessDangerSigns
    , AcuteIllnessFollowUp
    , AcuteIllnessMUAC
    , AcuteIllnessNutrition
    , AcuteIllnessVitals
    , AcuteIllnessCall114
    , AcuteIllnessCOVIDTesting
    , AcuteIllnessExposure
    , AcuteIllnessContactHC
    , AcuteIllnessHealthEducation
    , AcuteIllnessIsolation
    , AcuteIllnessMalariaTesting
    , AcuteIllnessMedicationDistribution
    , AcuteIllnessSendToHC
    , AcuteIllnessSymptomsGeneral
    , AcuteIllnessSymptomsGI
    , AcuteIllnessSymptomsRespiratory
    , AcuteIllnessENT
    , AcuteIllnessEyes
    , AcuteIllnessGU
    , AcuteIllnessOral
    , AcuteIllnessTravelHistory
    , AcuteIllnessPriorTreatment
    , AcuteIllnessOngoingTreatment
    ]


allNutritionIndividualActivities : List NutritionChildActivity
allNutritionIndividualActivities =
    [ NutritionHeight
    , NutritionNutrition
    , NutritionPhoto
    , NutritionWeight
    , NutritionMUAC
    , NutritionContributingFactors
    , NutritionFollowUp
    , NutritionHealthEducation
    , NutritionSendToHC
    , NutritionNCDA
    ]


allNutritionChildGroupActivities : List NutritionChildActivity
allNutritionChildGroupActivities =
    allNutritionIndividualActivities ++ [ NutritionChildFbf ]


allNutritionMotherGroupActivities : List NutritionMotherActivity
allNutritionMotherGroupActivities =
    [ NutritionFamilyPlanning
    , NutritionLactation
    , NutritionMotherFbf
    ]


resolveSPVActivities : Site -> List WellChildActivity
resolveSPVActivities site =
    [ WellChildAlbendazole
    , WellChildBCGImmunisation
    , WellChildCaring
    , WellChildContributingFactors
    , WellChildDTPImmunisation
    , WellChildECD
    , WellChildFeeding
    , WellChildFollowUp
    , WellChildFoodSecurity
    , WellChildHeadCircumference
    , WellChildHealthEducation
    , WellChildHeight
    , WellChildHygiene
    , WellChildIPVImmunisation
    , WellChildMebendezole
    , WellChildMRImmunisation
    , WellChildMUAC
    , WellChildNCDA

    -- This is just an indication that next visit notice
    -- was presented. Not showing it.
    -- , WellChildNextVisit
    , WellChildNutrition
    , WellChildOPVImmunisation
    , WellChildPCV13Immunisation
    , WellChildPhoto
    , WellChildRotarixImmunisation
    , WellChildSendToHC
    , WellChildSymptomsReview
    , WellChildVitals
    , WellChildVitaminA
    , WellChildWeight
    ]
        ++ (case site of
                SiteBurundi ->
                    [ WellChildDTPSAImmunisation ]

                _ ->
                    [ WellChildHPVImmunisation ]
           )


newbornExamActivities : List WellChildActivity
newbornExamActivities =
    [ WellChildBCGImmunisation
    , WellChildContributingFactors
    , WellChildFollowUp
    , WellChildHeadCircumference
    , WellChildHealthEducation
    , WellChildNutrition
    , WellChildOPVImmunisation
    , WellChildPhoto
    , WellChildPregnancySummary
    , WellChildSendToHC
    , WellChildWeight
    ]


allHomeVisitActivities : List HomeVisitActivity
allHomeVisitActivities =
    [ HomeVisitCaring
    , HomeVisitFeeding
    , HomeVisitFoodSecurity
    , HomeVisitHygiene
    ]


resolveChildScoreboardActivities : Site -> List ChildScoreboardActivity
resolveChildScoreboardActivities site =
    [ ChildScoreboardNCDA
    , ChildScoreboardBCGImmunisation
    , ChildScoreboardDTPImmunisation
    , ChildScoreboardIPVImmunisation
    , ChildScoreboardMRImmunisation
    , ChildScoreboardOPVImmunisation
    , ChildScoreboardPCV13Immunisation
    , ChildScoreboardRotarixImmunisation
    ]
        ++ (case site of
                SiteBurundi ->
                    [ ChildScoreboardDTPSAImmunisation ]

                _ ->
                    []
           )


allNCDActivities : List NCDActivity
allNCDActivities =
    [ NCDCoreExam
    , NCDCoMorbidities
    , NCDCreatinineTest
    , NCDCreatinineTestResult
    , NCDDangerSigns
    , NCDFamilyHistory
    , NCDFamilyPlanning
    , NCDHba1cTest
    , NCDHealthEducation
    , NCDHIVTest
    , NCDLipidPanelTest
    , NCDLipidPanelTestResult
    , NCDLiverFunctionTest
    , NCDLiverFunctionTestResult
    , NCDMedicationDistribution
    , NCDMedicationHistory
    , NCDOutsideCare
    , NCDPregnancyTest
    , NCDRandomBloodSugarTest
    , NCDRandomBloodSugarTestResult
    , NCDReferral
    , NCDSocialHistory
    , NCDSymptomReview
    , NCDUrineDipstickTest
    , NCDUrineDipstickTestResult
    , NCDVitals
    ]


allHIVActivities : List HIVActivity
allHIVActivities =
    [ HIVDiagnostics
    , HIVFollowUp
    , HIVHealthEducation
    , HIVMedication
    , HIVReferral
    , HIVSymptomReview
    , HIVTreatmentReview
    ]


allTuberculosisActivities : List TuberculosisActivity
allTuberculosisActivities =
    [ TuberculosisDiagnostics
    , TuberculosisDOT
    , TuberculosisFollowUp
    , TuberculosisHealthEducation
    , TuberculosisMedication
    , TuberculosisReferral
    , TuberculosisSymptomReview
    , TuberculosisTreatmentReview
    ]


allPrenatalActivities : List PrenatalActivity
allPrenatalActivities =
    [ PrenatalAppointmentConfirmation
    , PrenatalBirthPlan
    , PrenatalBloodGprsTest
    , PrenatalBloodGprsTestResult
    , PrenatalBreastExam
    , PrenatalBreastfeeding
    , PrenatalCalcium
    , PrenatalCorePhysicalExam
    , PrenatalDangerSigns
    , PrenatalFamilyPlanning
    , PrenatalFolate
    , PrenatalFollowUp
    , PrenatalGuExam
    , PrenatalHealthEducation
    , PrenatalHemoglobinTest
    , PrenatalHemoglobinTestResult
    , PrenatalHepatitisBTest
    , PrenatalHepatitisBTestResult
    , PrenatalHIVPCRTest
    , PrenatalHIVPCRTestResult
    , PrenatalHIVTest
    , PrenatalHIVTestResult
    , PrenatalIron
    , PrenatalLastMenstrualPeriod
    , PrenatalMalariaTest
    , PrenatalMalariaTestResult
    , PrenatalMebendazole
    , PrenatalMedicalHistory
    , PrenatalMedication
    , PrenatalMedicationDistribution
    , PrenatalMentalHealth
    , PrenatalMMS
    , PrenatalNutrition
    , PrenatalObstetricalExam
    , PrenatalObstetricHistory
    , PrenatalObstetricHistoryStep2
    , PrenatalOutsideCare
    , PrenatalPartnerHIVTest
    , PrenatalPartnerHIVTestResult
    , PrenatalPhoto
    , PrenatalPostpartumTreatmentReview
    , PrenatalPregnancyOutcome
    , PrenatalPregnancyTesting
    , PrenatalRandomBloodSugarTest
    , PrenatalRandomBloodSugarTestResult
    , PrenatalResource
    , PrenatalSendToHC
    , PrenatalSocialHistory
    , PrenatalSpecialityCare
    , PrenatalSymptomReview
    , PrenatalSyphilisTest
    , PrenatalSyphilisTestResult
    , PrenatalTetanusImmunisation
    , PrenatalTreatmentReview
    , PrenatalUrineDipstickTest
    , PrenatalUrineDipstickTestResult
    , PrenatalVitals
    , PrenatalVitalsRecheck
    ]
