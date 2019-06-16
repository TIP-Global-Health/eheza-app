module Pages.PrenatalActivity.Model exposing
    ( CSectionReason(..)
    , HistoryData
    , HistoryTask(..)
    , LmpRange(..)
    , MedicalHistoryForm
    , Model
    , Msg(..)
    , ObstetricFormFirstStep
    , ObstetricFormSecondStep
    , ObstetricHistoryFormType(..)
    , PregnancyDatingData
    , PregnancyDatingForm
    , PreviousDeliveryPeriod(..)
    , SocialHistoryForm
    , decodeLmpRange
    , emptyHistoryData
    , emptyMedicalHistoryForm
    , emptyModel
    , emptyObstetricFormSecondStep
    , emptyPregnancyDatingData
    , emptySocialHistoryForm
    , encodeLmpRange
    )

import Date exposing (Date)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
      -- PregnancyDatingMsgs
    | ToggleDateSelector
    | SetLmpDate Date
    | SetLmpDateConfident Bool
    | SetLmpRange String
      -- HistoryMsgs, OB, Step 1
    | OBSaveFirstStep
    | SetActiveHistoryTask HistoryTask
    | SetCurrentlyPregnant Bool
    | SetOBIntInput (Int -> ObstetricFormFirstStep -> ObstetricFormFirstStep) String
      -- HistoryMsgs, OB, Step 2
    | SetCSectionReason CSectionReason
    | SetNumberOfCSections String
    | SetOBBoolInput (Bool -> ObstetricFormSecondStep -> ObstetricFormSecondStep) Bool
    | SetPreviousDeliveryPeriod PreviousDeliveryPeriod
      -- HistoryMsgs, Medical
    | SetMedicalBoolInput (Bool -> MedicalHistoryForm -> MedicalHistoryForm) Bool
      -- HistoryMsgs, Social
    | SetSocialBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool


type alias Model =
    { pregnancyDatingData : PregnancyDatingData
    , historyData : HistoryData
    }


emptyModel : Model
emptyModel =
    { pregnancyDatingData = emptyPregnancyDatingData
    , historyData = emptyHistoryData
    }


type alias PregnancyDatingData =
    { form : PregnancyDatingForm
    }


emptyPregnancyDatingData : PregnancyDatingData
emptyPregnancyDatingData =
    { form = emptyPregnancyDatingForm
    }


type alias HistoryData =
    { obstetricForm : ObstetricHistoryFormType
    , medicalForm : MedicalHistoryForm
    , socialForm : SocialHistoryForm
    , activeTask : HistoryTask
    }


emptyHistoryData : HistoryData
emptyHistoryData =
    { obstetricForm = FirstStep emptyObstetricFormFirstStep
    , medicalForm = emptyMedicalHistoryForm
    , socialForm = emptySocialHistoryForm
    , activeTask = Obstetric
    }


type ObstetricHistoryFormType
    = FirstStep ObstetricFormFirstStep
    | SecondStep ObstetricFormSecondStep


type HistoryTask
    = Obstetric
    | Medical
    | Social


type alias PregnancyDatingForm =
    { lmpRange : Maybe LmpRange
    , lmpDate : Maybe Date
    , lmpDateConfident : Maybe Bool
    , isDateSelectorOpen : Bool
    }


emptyPregnancyDatingForm : PregnancyDatingForm
emptyPregnancyDatingForm =
    PregnancyDatingForm Nothing Nothing Nothing False


type alias ObstetricFormFirstStep =
    { currentlyPregnant : Maybe Bool
    , termPregnancy : Maybe Int
    , preTermPregnancy : Maybe Int
    , stillbirthsAtTerm : Maybe Int
    , stillbirthsPreTerm : Maybe Int
    , abortions : Maybe Int
    , liveChildren : Maybe Int
    }


emptyObstetricFormFirstStep : ObstetricFormFirstStep
emptyObstetricFormFirstStep =
    { currentlyPregnant = Nothing
    , termPregnancy = Nothing
    , preTermPregnancy = Nothing
    , stillbirthsAtTerm = Nothing
    , stillbirthsPreTerm = Nothing
    , abortions = Nothing
    , liveChildren = Nothing
    }


type alias ObstetricFormSecondStep =
    { cSections : Maybe Int
    , cSectionInPreviousDelivery : Maybe Bool
    , reasonForCSection : Maybe CSectionReason
    , previousDeliveryPeriod : Maybe PreviousDeliveryPeriod
    , successiveAbortions : Maybe Bool
    , successivePrimatureDeliveries : Maybe Bool
    , stillbornPreviousDelivery : Maybe Bool
    , babyDiedOnDayOfBirthPreviousDelivery : Maybe Bool
    , partialPlacentaPreviousDelivery : Maybe Bool
    , severeHemorrhagingPreviousDelivery : Maybe Bool
    , preeclampsiaPreviousPregnancy : Maybe Bool
    , convulsionsPreviousDelivery : Maybe Bool
    , convulsionsAndUnconciousPreviousDelivery : Maybe Bool
    , gestatipnalDiabetesPreviousPregnancy : Maybe Bool
    , incompleteCervixPreviousPregnancy : Maybe Bool
    , rhNegative : Maybe Bool
    }


emptyObstetricFormSecondStep : ObstetricFormSecondStep
emptyObstetricFormSecondStep =
    { cSections = Nothing
    , cSectionInPreviousDelivery = Nothing
    , reasonForCSection = Nothing
    , previousDeliveryPeriod = Nothing
    , successiveAbortions = Nothing
    , successivePrimatureDeliveries = Nothing
    , stillbornPreviousDelivery = Nothing
    , babyDiedOnDayOfBirthPreviousDelivery = Nothing
    , partialPlacentaPreviousDelivery = Nothing
    , severeHemorrhagingPreviousDelivery = Nothing
    , preeclampsiaPreviousPregnancy = Nothing
    , convulsionsPreviousDelivery = Nothing
    , convulsionsAndUnconciousPreviousDelivery = Nothing
    , gestatipnalDiabetesPreviousPregnancy = Nothing
    , incompleteCervixPreviousPregnancy = Nothing
    , rhNegative = Nothing
    }


type alias MedicalHistoryForm =
    { uterineMyoma : Maybe Bool
    , diabates : Maybe Bool
    , cardiacDisease : Maybe Bool
    , renalDisease : Maybe Bool
    , hypertensionBeforePregnancy : Maybe Bool
    , tuberculosisPast : Maybe Bool
    , tuberculosisPresent : Maybe Bool
    , asthma : Maybe Bool
    , bowedLegs : Maybe Bool
    , hiv : Maybe Bool
    }


emptyMedicalHistoryForm : MedicalHistoryForm
emptyMedicalHistoryForm =
    { uterineMyoma = Nothing
    , diabates = Nothing
    , cardiacDisease = Nothing
    , renalDisease = Nothing
    , hypertensionBeforePregnancy = Nothing
    , tuberculosisPast = Nothing
    , tuberculosisPresent = Nothing
    , asthma = Nothing
    , bowedLegs = Nothing
    , hiv = Nothing
    }


type alias SocialHistoryForm =
    { accompaniedByPartner : Maybe Bool
    , partnerReceivedCounseling : Maybe Bool
    , mentalHealthHistory : Maybe Bool
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    SocialHistoryForm Nothing Nothing Nothing


type CSectionReason
    = Breech
    | Emergency
    | FailureToProgress
    | None
    | Other


type PreviousDeliveryPeriod
    = LessThan18Month
    | MoreThan5Years
    | Neither


type LmpRange
    = OneMonth
    | ThreeMonth
    | SixMonth


encodeLmpRange : LmpRange -> String
encodeLmpRange range =
    case range of
        OneMonth ->
            "one-month"

        ThreeMonth ->
            "three-month"

        SixMonth ->
            "six-month"


decodeLmpRange : String -> Maybe LmpRange
decodeLmpRange s =
    case s of
        "one-month" ->
            Just OneMonth

        "three-month" ->
            Just ThreeMonth

        "six-month" ->
            Just SixMonth

        _ ->
            Nothing
