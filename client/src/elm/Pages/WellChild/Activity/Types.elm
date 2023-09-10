module Pages.WellChild.Activity.Types exposing (DangerSignsTask(..), ImmunisationTask(..), MedicationTask(..), NextStepsTask(..), NutritionAssessmentTask(..), VaccinationStatus(..))


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
