module Pages.Completion.Utils exposing (..)

import App.Types exposing (Site(..))
import Backend.Completion.Model
    exposing
        ( AcuteIllnessActivity(..)
        , NutritionChildActivity(..)
        , NutritionMotherActivity(..)
        , TakenBy(..)
        , WellChildActivity(..)
        )
import Pages.Completion.Model exposing (ReportType(..))


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportAcuteIllness ->
            "acute-illness"

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


allWellChildActivities : List WellChildActivity
allWellChildActivities =
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
    , WellChildNextVisit
    , WellChildNutrition
    , WellChildOPVImmunisation
    , WellChildPCV13Immunisation
    , WellChildPhoto
    , WellChildPregnancySummary
    , WellChildRotarixImmunisation
    , WellChildSendToHC
    , WellChildSymptomsReview
    , WellChildVitals
    , WellChildVitaminA
    , WellChildWeight
    , WellChildHPVImmunisation
    , WellChildDTPSAImmunisation
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
