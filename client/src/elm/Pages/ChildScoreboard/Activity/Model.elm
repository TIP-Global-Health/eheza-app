module Pages.ChildScoreboard.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( HeightForm
        , ImmunisationTask
        , MuacForm
        , NCDAData
        , NCDAForm
        , NCDAStep
        , NutritionForm
        , VaccinationForm
        , VaccinationFormViewMode
        , WeightForm
        , emptyHeightForm
        , emptyMuacForm
        , emptyNCDAData
        , emptyNutritionForm
        , emptyVaccinationForm
        , emptyWeightForm
        )
import Pages.Page exposing (Page)


type alias Model =
    { nutritionAssessmentData : NutritionAssessmentData
    , ncdaData : NCDAData
    , immunisationData : ImmunisationData
    }


emptyModel : Model
emptyModel =
    { nutritionAssessmentData = emptyNutritionAssessmentData
    , ncdaData = emptyNCDAData
    , immunisationData = emptyImmunisationData
    }


type alias NutritionAssessmentData =
    { heightForm : HeightForm
    , muacForm : MuacForm
    , nutritionForm : NutritionForm
    , weightForm : WeightForm
    , activeTask : Maybe NutritionAssessmentTask
    }


emptyNutritionAssessmentData : NutritionAssessmentData
emptyNutritionAssessmentData =
    { heightForm = emptyHeightForm
    , muacForm = emptyMuacForm
    , nutritionForm = emptyNutritionForm
    , weightForm = emptyWeightForm
    , activeTask = Nothing
    }


type alias ImmunisationData =
    { bcgForm : ChildScoreboardVaccinationForm
    , dtpForm : ChildScoreboardVaccinationForm
    , ipvForm : ChildScoreboardVaccinationForm
    , mrForm : ChildScoreboardVaccinationForm
    , opvForm : ChildScoreboardVaccinationForm
    , pcv13Form : ChildScoreboardVaccinationForm
    , rotarixForm : ChildScoreboardVaccinationForm
    , activeTask : Maybe ImmunisationTask
    }


type alias ChildScoreboardVaccinationForm =
    VaccinationForm Msg


emptyImmunisationData : ImmunisationData
emptyImmunisationData =
    { bcgForm = emptyVaccinationForm
    , dtpForm = emptyVaccinationForm
    , ipvForm = emptyVaccinationForm
    , mrForm = emptyVaccinationForm
    , opvForm = emptyVaccinationForm
    , pcv13Form = emptyVaccinationForm
    , rotarixForm = emptyVaccinationForm
    , activeTask = Nothing
    }


type NutritionAssessmentTask
    = TaskHeight
    | TaskMuac
    | TaskNutrition
    | TaskWeight


type Msg
    = NoOp
    | SetActivePage Page
      -- NUTRITION ASSESMENT
    | SetActiveNutritionAssessmentTask NutritionAssessmentTask
    | SetHeight String
    | SaveHeight PersonId (Maybe ( ChildScoreboardHeightId, ChildScoreboardHeight )) (Maybe NutritionAssessmentTask)
    | SetMuac String
    | SaveMuac PersonId (Maybe ( ChildScoreboardMuacId, ChildScoreboardMuac )) (Maybe NutritionAssessmentTask)
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( ChildScoreboardNutritionId, ChildScoreboardNutrition )) (EverySet NutritionAssessment) (Maybe NutritionAssessmentTask)
    | SetWeight String
    | SaveWeight PersonId (Maybe ( ChildScoreboardWeightId, ChildScoreboardWeight )) (Maybe NutritionAssessmentTask)
      -- NCDA
    | SetUpdateANCVisits Bool
    | ToggleANCVisitDate NominalDate
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetChildReceivesVitaminA ReceiveOption
    | SetNCDAFormStep NCDAStep
    | SetNCDAHelperState (Maybe NCDASign)
    | SaveNCDA PersonId (Maybe ( ChildScoreboardNCDAId, ChildScoreboardNCDA ))
      -- IMMUNISATION
    | SetActiveImmunisationTask ImmunisationTask
    | SetVaccinationFormViewMode WellChildVaccineType VaccinationFormViewMode
    | SetUpdatePreviousVaccines WellChildVaccineType VaccineDose Bool
    | SetWillReceiveVaccineToday WellChildVaccineType VaccineDose Bool
    | SetAdministrationNote WellChildVaccineType AdministrationNote
    | SetVaccinationUpdateDateSelectorState WellChildVaccineType (Maybe (DateSelectorConfig Msg))
    | SetVaccinationUpdateDate WellChildVaccineType NominalDate
    | SaveVaccinationUpdateDate WellChildVaccineType VaccineDose
    | DeleteVaccinationUpdateDate WellChildVaccineType VaccineDose NominalDate
    | SaveBCGImmunisation PersonId (Maybe ( ChildScoreboardBCGImmunisationId, ChildScoreboardBCGImmunisation )) (Maybe ImmunisationTask)
    | SaveDTPImmunisation PersonId (Maybe ( ChildScoreboardDTPImmunisationId, ChildScoreboardDTPImmunisation )) (Maybe ImmunisationTask)
    | SaveIPVImmunisation PersonId (Maybe ( ChildScoreboardIPVImmunisationId, ChildScoreboardIPVImmunisation )) (Maybe ImmunisationTask)
    | SaveMRImmunisation PersonId (Maybe ( ChildScoreboardMRImmunisationId, ChildScoreboardMRImmunisation )) (Maybe ImmunisationTask)
    | SaveOPVImmunisation PersonId (Maybe ( ChildScoreboardOPVImmunisationId, ChildScoreboardOPVImmunisation )) (Maybe ImmunisationTask)
    | SavePCV13Immunisation PersonId (Maybe ( ChildScoreboardPCV13ImmunisationId, ChildScoreboardPCV13Immunisation )) (Maybe ImmunisationTask)
    | SaveRotarixImmunisation PersonId (Maybe ( ChildScoreboardRotarixImmunisationId, ChildScoreboardRotarixImmunisation )) (Maybe ImmunisationTask)
