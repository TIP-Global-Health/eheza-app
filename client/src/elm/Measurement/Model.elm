module Measurement.Model exposing (..)

{-| These modules manage the UI for the various measurements relating to a
participant.
-}

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.ParticipantConsent.Model exposing (..)
import EverySet exposing (EverySet)
import Translate.Model exposing (Language)


{-| The strategy here, at least for now, is this:

  - The values in the `Model` here reflect what is entered in the UI. So, they
    are updated on every key-press etc.

  - The `update` function takes a parameter which represents the actual data.
    It updates that parameter only when the value has actually been saved.

So, basically we're doing pure UI here ... all other concerns are handled via
the `OutMsg` returned to the caller, which the caller is expected to do something
useful with.

This means that we need to be able to initialize our UI state here from some
backend state in order to perform an edit -- it's the caller's job to handle
that.

Ideally, we'll eventually use `Restful.RestfulData` to track underlying
data, UI edits, validation, and update status all in one type. If we had that,
we'd wouldn't really need our own model here (and we'd avoid some synchronization
issues) since the data itself would encapsulate an editor state.

-}
type alias ModelChild =
    { height : String
    , muac : String
    , nutritionSigns : EverySet ChildNutritionSign
    , photo : Maybe PhotoUrl
    , weight : String
    , counseling : Maybe ( CounselingTiming, EverySet CounselingTopicId )
    , fbfForm : FbfForm
    , contributingFactorsForm : ContributingFactorsForm
    , followUpForm : FollowUpForm
    , healthEducationForm : HealthEducationForm
    , sendToHCForm : SendToHCForm
    }


type alias ModelMother =
    { familyPlanningSigns : EverySet FamilyPlanningSign
    , participantConsent : ParticipantFormUI
    , lactationForm : LactationForm
    , fbfForm : FbfForm
    }


emptyModelChild : ModelChild
emptyModelChild =
    { height = ""
    , muac = ""
    , nutritionSigns = EverySet.empty
    , photo = Nothing
    , weight = ""
    , counseling = Nothing
    , fbfForm = FbfForm Nothing Nothing
    , contributingFactorsForm = emptyContributingFactorsForm
    , followUpForm = emptyFollowUpForm
    , healthEducationForm = emptyHealthEducationForm
    , sendToHCForm = emptySendToHCForm
    }


emptyModelMother : ModelMother
emptyModelMother =
    { familyPlanningSigns = EverySet.empty
    , participantConsent = emptyParticipantFormUI
    , lactationForm = LactationForm Nothing
    , fbfForm = FbfForm Nothing Nothing
    }


type alias FbfForm =
    { distributedAmount : Maybe Float
    , distributionNotice : Maybe DistributionNotice
    }


emptyFbfForm : FbfForm
emptyFbfForm =
    FbfForm Nothing Nothing


type alias ContributingFactorsForm =
    { signs : Maybe (List ContributingFactorsSign)
    }


emptyContributingFactorsForm : ContributingFactorsForm
emptyContributingFactorsForm =
    ContributingFactorsForm Nothing


type alias FollowUpForm =
    { option : Maybe FollowUpOption
    }


emptyFollowUpForm : FollowUpForm
emptyFollowUpForm =
    FollowUpForm Nothing


type alias HealthEducationForm =
    { educationForDiagnosis : Maybe Bool
    , reasonForNotProvidingHealthEducation : Maybe ReasonForNotProvidingHealthEducation
    }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    HealthEducationForm Nothing Nothing


type alias SendToHCForm =
    { handReferralForm : Maybe Bool
    , referToHealthCenter : Maybe Bool
    , reasonForNotSendingToHC : Maybe ReasonForNotSendingToHC
    }


emptySendToHCForm : SendToHCForm
emptySendToHCForm =
    SendToHCForm Nothing Nothing Nothing


{-| The UI for participant consent forms for a particular mother.

  - `expected` tracks which forms we expect to deal with for that mother.

  - `view` tracks which form we're looking at for the mother. If `Nothing`,
    we're looking at a list of the forms.

  - `progress` tracks the state of the UI for each particular form.

-}
type alias ParticipantFormUI =
    { expected : Dict ParticipantFormId ParticipantForm
    , view : Maybe ParticipantFormId
    , progress : Dict ParticipantFormId ParticipantFormProgress
    }


emptyParticipantFormUI : ParticipantFormUI
emptyParticipantFormUI =
    { expected = Dict.empty
    , view = Nothing
    , progress = Dict.empty
    }


type alias ParticipantFormProgress =
    { counselorSigned : Bool
    , participantSigned : Bool
    }


{-| The starting point for the UI where we haven't
obtained a consent yet.
-}
emptyParticipantFormProgress : ParticipantFormProgress
emptyParticipantFormProgress =
    { counselorSigned = False
    , participantSigned = False
    }


{-| The starting point for the UI when we have obtained
a consent.
-}
completedParticipantFormProgress : ParticipantFormProgress
completedParticipantFormProgress =
    { counselorSigned = True
    , participantSigned = True
    }


type alias FloatInputConstraints =
    { minVal : Float
    , maxVal : Float
    }


type alias FileId =
    Int


{-| Represents the "file" that DropZone gives us when
the upload is complete. There are several things we
could get from this ... for now, just the location.
-}
type alias DropZoneFile =
    { url : String
    }


type MsgChild
    = SelectNutritionSign Bool ChildNutritionSign
    | SelectCounselingTopic Bool CounselingTopicId
    | SendOutMsgChild OutMsgChild
    | SetDistributedAmountForChild String
    | SetDistributoinNoticeForChild DistributionNotice
    | UpdateHeight String
    | UpdateMuac String
    | UpdateWeight String
    | DropZoneComplete DropZoneFile
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNotSendingToHC ReasonForNotSendingToHC
    | SetProvidedEducationForDiagnosis Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SetContributingFactorsSign ContributingFactorsSign
    | SetFollowUpOption FollowUpOption


type MsgMother
    = SelectFamilyPlanningSign Bool FamilyPlanningSign
    | SelectLactationSign LactationSign Bool
    | ViewParticipantForm (Maybe ParticipantFormId)
    | SetCounselorSigned ParticipantFormId Bool
    | SetDistributedAmountForMother String
    | SetDistributoinNoticeForMother DistributionNotice
    | SetParticipantSigned ParticipantFormId Bool
    | SendOutMsgMother OutMsgMother


{-| This is sort of the "opposite" of `Msg`. Instead of representing messages
which we can handle, it represents messages we **can't** handle, and would
like the caller to take care of.

The `Maybe` IDs indicate whether we're trying to update an exsiting value, vs.
creating a new one.

-}
type OutMsgChild
    = FetchIndividualNutritionData PersonId
    | SaveHeight (Maybe HeightId) HeightInCm
    | SaveWeight (Maybe WeightId) WeightInKg
    | SaveMuac (Maybe MuacId) MuacInCm
    | SaveCounselingSession (Maybe CounselingSessionId) CounselingTiming (EverySet CounselingTopicId)
    | SaveChildNutritionSigns (Maybe ChildNutritionId) (EverySet ChildNutritionSign)
    | SavePhoto (Maybe PhotoId) PhotoUrl
    | SaveChildFbf (Maybe ChildFbfId) FbfValue
    | SaveContributingFactors (Maybe ContributingFactorsId) (EverySet ContributingFactorsSign)
    | SaveFollowUp (Maybe FollowUpId) (EverySet FollowUpOption)
    | SaveHealthEducation (Maybe GroupHealthEducationId) HealthEducationValue
    | SaveSendToHC (Maybe GroupSendToHCId) SendToHCValue


type OutMsgMother
    = SaveAttendance (Maybe AttendanceId) Bool
    | SaveFamilyPlanningSigns (Maybe FamilyPlanningId) (EverySet FamilyPlanningSign)
    | SaveCompletedForm (Maybe ParticipantConsentId) ParticipantFormId Language
    | SaveLactation (Maybe LactationId) (EverySet LactationSign)
    | SaveMotherFbf (Maybe MotherFbfId) FbfValue
