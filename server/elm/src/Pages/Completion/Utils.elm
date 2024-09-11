module Pages.Completion.Utils exposing (..)

import App.Types exposing (Site(..))
import Backend.Completion.Model
    exposing
        ( AcuteIllnessActivity(..)
        , ChildScoreboardActivity(..)
        , HomeVisitActivity(..)
        , NCDActivity(..)
        , NutritionChildActivity(..)
        , NutritionMotherActivity(..)
        , TakenBy(..)
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

        ReportWellChild ->
            "well-child"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "acute-illness" ->
            Just ReportAcuteIllness

        "child-scoreboard" ->
            Just ReportChildScoreboard

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

    -- Not implemented at current phase.
    -- , WellChildECD
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
