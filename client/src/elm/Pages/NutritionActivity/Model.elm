module Pages.NutritionActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (DropZoneFile)
import Pages.AcuteIllnessActivity.Model exposing (HealthEducationForm, SendToHCForm)
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
    | SetActiveNextStepsTask NextStepsTask
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNotSendingToHC ReasonForNotSendingToHC
    | SaveSendToHC PersonId (Maybe ( NutritionSendToHCId, NutritionSendToHC )) (Maybe NextStepsTask)
    | SetProvidedEducationForDiagnosis Bool
    | SaveHealthEducation PersonId (Maybe ( NutritionHealthEducationId, NutritionHealthEducation )) (Maybe NextStepsTask)
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation


type alias Model =
    { muacData : MuacData
    , heightData : HeightData
    , nutritionData : NutritionData
    , photoData : PhotoData
    , weightData : WeightData
    , nextStepsData : NextStepsData
    , sendToHCData : SendToHCData
    , healthEducationData : HealthEducationData
    }


emptyModel : Model
emptyModel =
    { muacData = emptyMuacData
    , heightData = emptyHeightData
    , nutritionData = emptyNutritionData
    , photoData = emptyPhotoData
    , weightData = emptyWeightData
    , nextStepsData = emptyNextStepsData
    , sendToHCData = emptySendToHCData
    , healthEducationData = emptyHealthEducationData
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


type alias NextStepsData =
    { sendToHCForm : SendToHCForm
    , healthEducationForm : HealthEducationForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { sendToHCForm = SendToHCForm Nothing Nothing Nothing
    , healthEducationForm = HealthEducationForm Nothing Nothing
    , activeTask = Nothing
    }


type NextStepsTask
    = NextStepsSendToHC
    | NextStepsHealthEducation


type alias SendToHCData =
    { form : SendToHCForm
    }


emptySendToHCData : SendToHCData
emptySendToHCData =
    { form = SendToHCForm Nothing Nothing Nothing
    }


type alias HealthEducationData =
    { form : HealthEducationForm
    }


emptyHealthEducationData : HealthEducationData
emptyHealthEducationData =
    { form = HealthEducationForm Nothing Nothing
    }
