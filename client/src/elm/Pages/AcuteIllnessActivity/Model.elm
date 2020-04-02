module Pages.AcuteIllnessActivity.Model exposing (Model, Msg(..), SymptomsData, SymptomsGIForm, SymptomsGeneralForm, SymptomsRespiratoryForm, SymptomsTask(..), emptyModel, emptySymptomsData)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetActivePageSymptomsTask SymptomsTask
    | SaveSymptomsGeneral PersonId (Maybe ( SymptomsGeneralId, SymptomsGeneral )) (Maybe SymptomsTask)
    | SaveSymptomsRespiratory PersonId (Maybe ( SymptomsRespiratoryId, SymptomsRespiratory )) (Maybe SymptomsTask)
    | SaveSymptomsGI PersonId (Maybe ( SymptomsGIId, SymptomsGI )) (Maybe SymptomsTask)



-- | SetAcuteIllnessSign ChildAcuteIllnessSign
-- | SaveAcuteIllness PersonId (Maybe ( AcuteIllnessAcuteIllnessId, AcuteIllnessAcuteIllness ))


type alias Model =
    { symptomsData : SymptomsData
    }


emptyModel : Model
emptyModel =
    { symptomsData = emptySymptomsData
    }


type alias SymptomsData =
    { symptomsGeneralForm : SymptomsGeneralForm
    , symptomsRespiratoryForm : SymptomsRespiratoryForm
    , symptomsGIForm : SymptomsGIForm
    , activeTask : SymptomsTask
    }


emptySymptomsData : SymptomsData
emptySymptomsData =
    { symptomsGeneralForm = SymptomsGeneralForm Nothing
    , symptomsRespiratoryForm = SymptomsRespiratoryForm Nothing
    , symptomsGIForm = SymptomsGIForm Nothing
    , activeTask = SymptomsGeneral
    }


type SymptomsTask
    = SymptomsGeneral
    | SymptomsRespiratory
    | SymptomsGI


type alias SymptomsGeneralForm =
    { signs : Maybe (List SymptomsGeneralSign)
    }


type alias SymptomsRespiratoryForm =
    { signs : Maybe (List SymptomsRespiratorySign)
    }


type alias SymptomsGIForm =
    { signs : Maybe (List SymptomsGISign)
    }
