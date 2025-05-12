module Pages.Nutrition.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = NoOp
    | SetActivePage Page
    | SetWarningPopupState (List NutritionAssessment)
    | SetHeight String
    | SetHeightNotTaken Bool
    | SaveHeight (EverySet SkippedForm) PersonId (Maybe ( NutritionHeightId, NutritionHeight ))
    | SetMuac String
    | SaveMuac PersonId (Maybe ( NutritionMuacId, NutritionMuac ))
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( NutritionNutritionId, NutritionNutrition )) (EverySet NutritionAssessment)
    | DropZoneComplete DropZoneFile
    | SavePhoto PersonId (Maybe NutritionPhotoId) ImageUrl
    | SetWeight String
    | SaveWeight PersonId (Maybe ( NutritionWeightId, NutritionWeight ))
    | SetUpdateANCVisits Bool
    | ToggleANCVisitDate NominalDate
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetChildReceivesVitaminA ReceiveOption
    | SetStuntingLevel StuntingLevel
    | SetWeightForNCDA String
    | SetMuacForNCDA String
    | SetNCDAFormStep NCDAStep
    | SetNCDAHelperState (Maybe NCDASign)
    | SaveNCDA PersonId (Maybe ( NutritionNCDAId, NutritionNCDA ))
    | SetActiveNextStepsTask NextStepsTask
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNonReferral ReasonForNonReferral
    | SaveSendToHC PersonId (Maybe ( NutritionSendToHCId, NutritionSendToHC )) (Maybe NextStepsTask)
    | SetProvidedEducationForDiagnosis Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SaveHealthEducation PersonId (Maybe ( NutritionHealthEducationId, NutritionHealthEducation )) (Maybe NextStepsTask)
    | SetContributingFactorsSign ContributingFactorsSign
    | SaveContributingFactors PersonId (Maybe ( NutritionContributingFactorsId, NutritionContributingFactors )) (Maybe NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( NutritionFollowUpId, NutritionFollowUp )) (EverySet NutritionAssessment) (Maybe NextStepsTask)


type alias Model =
    { muacData : MuacData
    , heightData : HeightData
    , nutritionData : NutritionData
    , photoData : PhotoData
    , weightData : WeightData
    , ncdaData : NCDAData
    , nextStepsData : NextStepsData
    , warningPopupState : List NutritionAssessment
    }


emptyModel : Model
emptyModel =
    { muacData = emptyMuacData
    , heightData = emptyHeightData
    , nutritionData = emptyNutritionData
    , photoData = emptyPhotoData
    , weightData = emptyWeightData
    , ncdaData = emptyNCDAData
    , nextStepsData = emptyNextStepsData
    , warningPopupState = []
    }


type alias MuacData =
    { form : MuacForm
    }


emptyMuacData : MuacData
emptyMuacData =
    MuacData emptyMuacForm


type alias HeightData =
    { form : HeightForm
    }


emptyHeightData : HeightData
emptyHeightData =
    HeightData emptyHeightForm


type alias NutritionData =
    { form : NutritionForm
    }


emptyNutritionData : NutritionData
emptyNutritionData =
    NutritionData emptyNutritionForm


type alias PhotoData =
    { form : PhotoForm
    }


emptyPhotoData : PhotoData
emptyPhotoData =
    PhotoData emptyPhotoForm


type alias WeightData =
    { form : WeightForm
    }


emptyWeightData : WeightData
emptyWeightData =
    WeightData emptyWeightForm


type alias NextStepsData =
    { sendToHCForm : SendToHCForm
    , healthEducationForm : HealthEducationForm
    , contributingFactorsForm : ContributingFactorsForm
    , followUpForm : NutritionFollowUpForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { sendToHCForm = emptySendToHCForm
    , healthEducationForm = emptyHealthEducationForm
    , contributingFactorsForm = emptyContributingFactorsForm
    , followUpForm = emptyNutritionFollowUpForm
    , activeTask = Nothing
    }
