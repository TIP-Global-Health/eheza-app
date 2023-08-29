module Pages.WellChild.Activity.Types exposing (..)

import Backend.Measurement.Model exposing (VaccineDose)


type VaccinationStatus
    = StatusBehind
    | StatusCompleted
    | StatusUpToDate


type DangerSignsTask
    = TaskSymptomsReview
    | TaskVitals


type NutritionAssessmentTask
    = TaskHeight
    | TaskHeadCircumference
    | TaskMuac
    | TaskNutrition
    | TaskWeight


type ImmunisationTask
    = TaskBCG
    | TaskDTP
    | TaskHPV
    | TaskIPV
    | TaskMR
    | TaskOPV
    | TaskPCV13
    | TaskRotarix
    | TaskOverview


type MedicationTask
    = TaskAlbendazole
    | TaskMebendezole
    | TaskVitaminA


type NextStepsTask
    = TaskContributingFactors
    | TaskHealthEducation
    | TaskSendToHC
    | TaskFollowUp
    | TaskNextVisit
