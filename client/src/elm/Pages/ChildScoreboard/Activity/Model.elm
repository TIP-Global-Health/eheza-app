module Pages.ChildScoreboard.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( ImmunisationTask(..)
        , NCDAData
        , NCDAForm
        , NCDAStep
        , VaccinationForm
        , VaccinationFormViewMode
        , VaccinationProgressDict
        , emptyNCDAData
        , emptyVaccinationForm
        )
import Pages.Page exposing (Page)


type alias Model =
    { ncdaData : NCDAData
    , immunisationData : ImmunisationData
    }


emptyModel : Model
emptyModel =
    { ncdaData = emptyNCDAData
    , immunisationData = emptyImmunisationData
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


type Msg
    = SetActivePage Page
      -- NCDA
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetNumberANCVisits String
    | SetNutritionSupplementType NutritionSupplementType
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
