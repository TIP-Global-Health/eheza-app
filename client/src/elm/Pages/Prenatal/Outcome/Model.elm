module Pages.Prenatal.Outcome.Model exposing (Model, Msg(..), emptyModel)

import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, PregnancyOutcome)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import Pages.Page exposing (Page)


type alias Model =
    { pregnancyConcludedDate : Maybe Date
    , pregnancyOutcome : Maybe PregnancyOutcome
    , deliveryLocation : Maybe DeliveryLocation
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


type Msg
    = -- Page is the destination page where nurse / chw
      -- is forwarded after form is saved.
      SavePregnancyOutcome Date PregnancyOutcome DeliveryLocation Page
    | SetActivePage Page
    | SetDeliveryLocation Bool
    | SetPregnancyConcludedDate Date
    | SetPregnancyOutcome String
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))


emptyModel : Model
emptyModel =
    { pregnancyConcludedDate = Nothing
    , pregnancyOutcome = Nothing
    , deliveryLocation = Nothing
    , dateSelectorPopupState = Nothing
    }
