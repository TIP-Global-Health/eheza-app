module Pages.NCD.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page


type alias Model =
    { dangerSignsData : NCDDangerSignsData
    , symptomReviewData : NCDSymptomReviewData
    }


emptyModel : Model
emptyModel =
    { dangerSignsData = emptyNCDDangerSignsData
    , symptomReviewData = emptyNCDSymptomReviewData
    }


type alias NCDDangerSignsData =
    { form : NCDDangerSignsForm
    }


emptyNCDDangerSignsData : NCDDangerSignsData
emptyNCDDangerSignsData =
    { form = emptyNCDDangerSignsForm
    }


type alias NCDDangerSignsForm =
    { signs : Maybe (List NCDDangerSign)
    }


emptyNCDDangerSignsForm : NCDDangerSignsForm
emptyNCDDangerSignsForm =
    NCDDangerSignsForm Nothing


type alias NCDSymptomReviewData =
    { form : NCDSymptomReviewForm
    }


emptyNCDSymptomReviewData : NCDSymptomReviewData
emptyNCDSymptomReviewData =
    { form = emptyNCDSymptomReviewForm
    }


type alias NCDSymptomReviewForm =
    { group1Symptoms : Maybe (List NCDGroup1Symptom)
    , group2Symptoms : Maybe (List NCDGroup2Symptom)
    , painSymptoms : Maybe (List NCDPainSymptom)
    }


emptyNCDSymptomReviewForm : NCDSymptomReviewForm
emptyNCDSymptomReviewForm =
    NCDSymptomReviewForm Nothing Nothing Nothing
