module Pages.PrenatalActivity.Model exposing
    ( CSectionReason(..)
    , HistoryData
    , HistoryTask(..)
    , LmpRange(..)
    , Model
    , Msg(..)
    , ObstetricFormFirstStep
    , ObstetricFormSecondStep
    , ObstetricHistoryFormType(..)
    , PregnancyDatingData
    , PregnancyDatingForm
    , PreviousDeliveryPeriod(..)
    , decodeLmpRange
    , emptyHistoryData
    , emptyModel
    , emptyPregnancyDatingData
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
      -- HistoryMsgs
    | SetAbortions String
    | SetActiveHistoryTask HistoryTask
    | SetCurrentlyPregnant Bool
    | SetLiveChildren String
    | SetPreTermPregnancy String
    | SetStillbirthsAtTerm String
    | SetStillbirthsPreTerm String
    | SetTermPregnancy String


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
    , activeTask : HistoryTask
    }


emptyHistoryData : HistoryData
emptyHistoryData =
    { obstetricForm = FirstStep emptyObstetricFormFirstStep
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
    , termPreganancy : Maybe Int
    , preTermPreganancy : Maybe Int
    , stillbirthsAtTerm : Maybe Int
    , stillbirthsPreTerm : Maybe Int
    , abortions : Maybe Int
    , liveChildren : Maybe Int
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
    , preeclampsiaPreviousPreganancy : Maybe Bool
    , convulsionsPreviousDelivery : Maybe Bool
    , convulsionsAndUnconciousPreviousDelivery : Maybe Bool
    , gestatipnalDiabetesPreviousPreganancy : Maybe Bool
    , incompleteCervixPreviousPreganancy : Maybe Bool
    , rhNegative : Maybe Bool
    }


emptyObstetricFormFirstStep : ObstetricFormFirstStep
emptyObstetricFormFirstStep =
    { currentlyPregnant = Nothing
    , termPreganancy = Nothing
    , preTermPreganancy = Nothing
    , stillbirthsAtTerm = Nothing
    , stillbirthsPreTerm = Nothing
    , abortions = Nothing
    , liveChildren = Nothing
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
    , preeclampsiaPreviousPreganancy = Nothing
    , convulsionsPreviousDelivery = Nothing
    , convulsionsAndUnconciousPreviousDelivery = Nothing
    , gestatipnalDiabetesPreviousPreganancy = Nothing
    , incompleteCervixPreviousPreganancy = Nothing
    , rhNegative = Nothing
    }


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
