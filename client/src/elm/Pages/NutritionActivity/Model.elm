module Pages.NutritionActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Measurement.Model exposing (..)
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
    | SaveFollowUp PersonId (Maybe ( NutritionFollowUpId, NutritionFollowUp )) (EverySet NutritionAssesment) (Maybe NextStepsTask)


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
    MuacData emptyMuacForm


type alias MuacForm =
    { muac : Maybe Float
    , muacDirty : Bool
    }


emptyMuacForm : MuacForm
emptyMuacForm =
    MuacForm Nothing False


type alias HeightData =
    { form : HeightForm
    }


emptyHeightData : HeightData
emptyHeightData =
    HeightData emptyHeightForm


type alias HeightForm =
    { height : Maybe Float
    , heightDirty : Bool
    }


emptyHeightForm : HeightForm
emptyHeightForm =
    HeightForm Nothing False


type alias NutritionData =
    { form : NutritionForm
    }


emptyNutritionData : NutritionData
emptyNutritionData =
    NutritionData emptyNutritionForm


type alias NutritionForm =
    { signs : Maybe (List ChildNutritionSign)
    }


emptyNutritionForm : NutritionForm
emptyNutritionForm =
    NutritionForm Nothing


type alias PhotoData =
    { form : PhotoForm
    }


emptyPhotoData : PhotoData
emptyPhotoData =
    PhotoData emptyPhotoForm


type alias PhotoForm =
    { url : Maybe PhotoUrl
    }


emptyPhotoForm : PhotoForm
emptyPhotoForm =
    PhotoForm Nothing


type alias WeightData =
    { form : WeightForm
    }


emptyWeightData : WeightData
emptyWeightData =
    WeightData emptyWeightForm


type alias WeightForm =
    { weight : Maybe Float
    , weightDirty : Bool
    }


emptyWeightForm : WeightForm
emptyWeightForm =
    WeightForm Nothing False


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
