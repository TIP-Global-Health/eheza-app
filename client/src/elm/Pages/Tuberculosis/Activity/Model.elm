module Pages.Tuberculosis.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
import Pages.Page exposing (Page)


type alias Model =
    { diagnosticsData : DiagnosticsData
    , symptomReviewData : SymptomReviewData
    }


emptyModel : Model
emptyModel =
    { diagnosticsData = emptyDiagnosticsData
    , symptomReviewData = emptySymptomReviewData
    }


type alias DiagnosticsData =
    { form : DiagnosticsForm
    }


emptyDiagnosticsData : DiagnosticsData
emptyDiagnosticsData =
    { form = emptyDiagnosticsForm
    }


type alias DiagnosticsForm =
    { diagnosed : Maybe Bool
    , isPulmonary : Maybe Bool
    , isPulmonaryDirty : Bool
    }


emptyDiagnosticsForm : DiagnosticsForm
emptyDiagnosticsForm =
    { diagnosed = Nothing
    , isPulmonary = Nothing
    , isPulmonaryDirty = False
    }


type alias SymptomReviewData =
    { form : SymptomReviewForm
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    }


type alias SymptomReviewForm =
    { nightSweats : Maybe Bool
    , bloodInSputum : Maybe Bool
    , weightLoss : Maybe Bool
    , severeFatigue : Maybe Bool
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    { nightSweats = Nothing
    , bloodInSputum = Nothing
    , weightLoss = Nothing
    , severeFatigue = Nothing
    }


type NextStepsTask
    = TaskReferral
    | TaskHealthEducation
    | TaskFollowUp


type Msg
    = SetActivePage Page
    | SetDiagnosticsBoolInput (Bool -> DiagnosticsForm -> DiagnosticsForm) Bool
    | SaveDiagnostics PersonId (Maybe ( TuberculosisDiagnosticsId, TuberculosisDiagnostics ))
    | SetSymptomReviewBoolInput (Bool -> SymptomReviewForm -> SymptomReviewForm) Bool
    | SaveSymptomReview PersonId (Maybe ( TuberculosisSymptomReviewId, TuberculosisSymptomReview ))
