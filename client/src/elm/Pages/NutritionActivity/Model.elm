module Pages.NutritionActivity.Model exposing
    ( Model
    , Msg(..)
    , NutritionData
    , NutritionForm
    , emptyModel
    )

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition NutritionEncounterId PersonId (Maybe ( NutritionNutritionId, NutritionNutrition ))


type alias Model =
    { nutritionData : NutritionData
    }


emptyModel : Model
emptyModel =
    { nutritionData = emptyNutritionData
    }


type alias NutritionData =
    { form : NutritionForm
    }


emptyNutritionData : NutritionData
emptyNutritionData =
    { form = NutritionForm Nothing
    }


type alias NutritionForm =
    { signs : Maybe (List ChildNutritionSign)
    }
