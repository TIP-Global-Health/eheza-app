module Pages.HomeVisit.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model
    exposing
        ( NutritionCaringForm
        , NutritionFeedingForm
        , NutritionFoodSecurityForm
        , NutritionHygieneForm
        , emptyNutritionCaringForm
        , emptyNutritionFeedingForm
        , emptyNutritionFoodSecurityForm
        , emptyNutritionHygieneForm
        )
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetFeedingBoolInput (Bool -> NutritionFeedingForm -> NutritionFeedingForm) Bool
    | SetNutritionSupplementType NutritionSupplementType
    | SetSachetsPerDay String
    | SaveFeeding PersonId (Maybe ( NutritionFeedingId, NutritionFeeding ))
    | SetHygieneBoolInput (Bool -> NutritionHygieneForm -> NutritionHygieneForm) Bool
    | SetMainWaterSource MainWaterSource
    | SaveHygiene PersonId (Maybe ( NutritionHygieneId, NutritionHygiene ))
    | SetFoodSecurityBoolInput (Bool -> NutritionFoodSecurityForm -> NutritionFoodSecurityForm) Bool
    | SetMainIncomeSource MainIncomeSource
    | SetWaterPreparationOption WaterPreparationOption
    | SaveFoodSecurity PersonId (Maybe ( NutritionFoodSecurityId, NutritionFoodSecurity ))
    | SetParentsAliveAndHealthy Bool
    | SetChildClean Bool
    | SetNutritionCaringOption CaringOption
    | SaveNutritionCaring PersonId (Maybe ( NutritionCaringId, NutritionCaring ))


type alias Model =
    { feedingForm : NutritionFeedingForm
    , hygieneForm : NutritionHygieneForm
    , foodSecurityForm : NutritionFoodSecurityForm
    , caringForm : NutritionCaringForm
    }


emptyModel : Model
emptyModel =
    { feedingForm = emptyNutritionFeedingForm
    , hygieneForm = emptyNutritionHygieneForm
    , foodSecurityForm = emptyNutritionFoodSecurityForm
    , caringForm = emptyNutritionCaringForm
    }
