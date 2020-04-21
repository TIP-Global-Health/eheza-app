module Pages.NutritionActivity.Model exposing (HeightData, HeightForm, Model, Msg(..), MuacData, MuacForm, NutritionData, NutritionForm, PhotoData, PhotoForm, WeightData, WeightForm, emptyHeightData, emptyModel, emptyMuacData, emptyNutritionData, emptyPhotoData, emptyWeightData)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetHeight String
    | SaveHeight PersonId (Maybe ( NutritionHeightId, NutritionHeight ))
    | SetMuac String
    | SaveMuac PersonId (Maybe ( NutritionMuacId, NutritionMuac ))
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( NutritionNutritionId, NutritionNutrition ))
    | SetWeight String
    | SaveWeight PersonId (Maybe ( NutritionWeightId, NutritionWeight ))


type alias Model =
    { muacData : MuacData
    , heightData : HeightData
    , nutritionData : NutritionData
    , photoData : PhotoData
    , weightData : WeightData
    }


emptyModel : Model
emptyModel =
    { muacData = emptyMuacData
    , heightData = emptyHeightData
    , nutritionData = emptyNutritionData
    , photoData = emptyPhotoData
    , weightData = emptyWeightData
    }


type alias MuacData =
    { form : MuacForm
    }


emptyMuacData : MuacData
emptyMuacData =
    { form = MuacForm Nothing
    }


type alias MuacForm =
    { muac : Maybe Float
    }


type alias HeightData =
    { form : HeightForm
    }


emptyHeightData : HeightData
emptyHeightData =
    { form = HeightForm Nothing
    }


type alias HeightForm =
    { height : Maybe Float
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


type alias PhotoData =
    { form : PhotoForm
    }


emptyPhotoData : PhotoData
emptyPhotoData =
    { form = PhotoForm Nothing
    }


type alias PhotoForm =
    { url : Maybe PhotoUrl
    }


type alias WeightData =
    { form : WeightForm
    }


emptyWeightData : WeightData
emptyWeightData =
    { form = WeightForm Nothing
    }


type alias WeightForm =
    { weight : Maybe Float
    }
