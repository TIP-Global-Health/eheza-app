module Pages.NutritionActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (DropZoneFile)
import Pages.AcuteIllnessActivity.Model exposing (SendToHCForm)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetHeight String
    | SaveHeight PersonId (Maybe ( NutritionHeightId, NutritionHeight ))
    | SetMuac String
    | SaveMuac PersonId (Maybe ( NutritionMuacId, NutritionMuac ))
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( NutritionNutritionId, NutritionNutrition ))
    | DropZoneComplete DropZoneFile
    | SavePhoto PersonId (Maybe NutritionPhotoId) PhotoUrl
    | SetWeight String
    | SaveWeight PersonId (Maybe ( NutritionWeightId, NutritionWeight ))
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SaveSendToHC PersonId (Maybe ( NutritionSendToHCId, NutritionSendToHC ))


type alias Model =
    { muacData : MuacData
    , heightData : HeightData
    , nutritionData : NutritionData
    , photoData : PhotoData
    , weightData : WeightData
    , sendToHCData : SendToHCData
    }


emptyModel : Model
emptyModel =
    { muacData = emptyMuacData
    , heightData = emptyHeightData
    , nutritionData = emptyNutritionData
    , photoData = emptyPhotoData
    , weightData = emptyWeightData
    , sendToHCData = emptySendToHCData
    }


type alias MuacData =
    { form : MuacForm
    }


emptyMuacData : MuacData
emptyMuacData =
    { form = MuacForm Nothing False
    }


type alias MuacForm =
    { muac : Maybe Float
    , muacDirty : Bool
    }


type alias HeightData =
    { form : HeightForm
    }


emptyHeightData : HeightData
emptyHeightData =
    { form = HeightForm Nothing False
    }


type alias HeightForm =
    { height : Maybe Float
    , heightDirty : Bool
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
    { form = WeightForm Nothing False
    }


type alias WeightForm =
    { weight : Maybe Float
    , weightDirty : Bool
    }


type alias SendToHCData =
    { form : SendToHCForm
    }


emptySendToHCData : SendToHCData
emptySendToHCData =
    { form = SendToHCForm Nothing Nothing
    }
