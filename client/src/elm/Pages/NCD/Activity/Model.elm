module Pages.NCD.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
      -- DangerSignsMsgs
    | SetDangerSign NCDDangerSign
    | SaveDangerSigns PersonId (Maybe ( NCDDangerSignsId, NCDDangerSigns ))
      -- SymptomReviewMsgs
    | SetGroup1Symptom NCDGroup1Symptom
    | SetGroup2Symptom NCDGroup2Symptom
    | SetPainSymptom NCDPainSymptom
    | SaveSymptomReview PersonId (Maybe ( NCDSymptomReviewId, NCDSymptomReview ))


type alias Model =
    { dangerSignsData : DangerSignsData
    , symptomReviewData : SymptomReviewData
    }


emptyModel : Model
emptyModel =
    { dangerSignsData = emptyDangerSignsData
    , symptomReviewData = emptySymptomReviewData
    }


type alias DangerSignsData =
    { form : DangerSignsForm
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { form = emptyDangerSignsForm
    }


type alias DangerSignsForm =
    { signs : Maybe (List NCDDangerSign)
    }


emptyDangerSignsForm : DangerSignsForm
emptyDangerSignsForm =
    DangerSignsForm Nothing


type alias SymptomReviewData =
    { form : SymptomReviewForm
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    }


type alias SymptomReviewForm =
    { group1Symptoms : Maybe (List NCDGroup1Symptom)
    , group2Symptoms : Maybe (List NCDGroup2Symptom)
    , painSymptoms : Maybe (List NCDPainSymptom)
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    SymptomReviewForm Nothing Nothing Nothing
