module Pages.Completion.Model exposing (..)

import Backend.Completion.Model exposing (NutritionActivity(..))


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}


type Msg
    = NoOp


allNutritionActivities : List NutritionActivity
allNutritionActivities =
    [ NutritionHeight
    , NutritionNutrition
    , NutritionPhoto
    , NutritionWeight
    , NutritionMUAC
    , NutritionContributingFactors
    , NutritionFollowUp
    , NutritionHealthEducation
    , NutritionSendToHC
    , NutritionNCDA
    ]
