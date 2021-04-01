module Pages.NutritionActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (..)
import Backend.Measurement.Model exposing (NutritionAssesment)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetWarningPopupState (List NutritionAssesment)
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
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SaveHealthEducation PersonId (Maybe ( NutritionHealthEducationId, NutritionHealthEducation )) (Maybe NextStepsTask)
    | SetContributingFactorsSign ContributingFactorsSign
    | SaveContributingFactors PersonId (Maybe ( NutritionContributingFactorsId, NutritionContributingFactors )) (Maybe NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( NutritionFollowUpId, NutritionFollowUp )) (Maybe NextStepsTask)


type alias Model =
    { muacData : MuacData
    , heightData : HeightData
    , nutritionData : NutritionData
    , photoData : PhotoData
    , weightData : WeightData
    , nextStepsData : NextStepsData
    , warningPopupState : List NutritionAssesment
    }


emptyModel : Model
emptyModel =
    { muacData = emptyMuacData
    , heightData = emptyHeightData
    , nutritionData = emptyNutritionData
    , photoData = emptyPhotoData
    , weightData = emptyWeightData
    , nextStepsData = emptyNextStepsData
    , warningPopupState = []
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
    , contributingFactorsForm : ContributingFactorsForm
    , followUpForm : FollowUpForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { sendToHCForm = emptySendToHCForm
    , healthEducationForm = emptyHealthEducationForm
    , contributingFactorsForm = emptyContributingFactorsForm
    , followUpForm = emptyFollowUpForm
    , activeTask = Nothing
    }
