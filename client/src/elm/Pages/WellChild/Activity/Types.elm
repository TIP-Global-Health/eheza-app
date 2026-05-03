module Pages.WellChild.Activity.Types exposing (DangerSignsTask(..), HomeVisitTask(..), MedicationTask(..), NextStepsTask(..), NutritionAssessmentTask(..))


type DangerSignsTask
    = TaskSymptomsReview
    | TaskVitals


type NutritionAssessmentTask
    = TaskHeight
    | TaskHeadCircumference
    | TaskMuac
    | TaskNutrition
    | TaskWeight


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


type HomeVisitTask
    = TaskCaring
    | TaskFeeding
    | TaskHygiene
    | TaskFoodSecurity
