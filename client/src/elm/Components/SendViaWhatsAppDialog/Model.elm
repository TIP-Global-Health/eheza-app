module Components.SendViaWhatsAppDialog.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)


type alias Model =
    { state : Maybe DialogState
    }


emptyModel : Model
emptyModel =
    { state = Nothing
    }


type DialogState
    = Consent
    | PhoneVerification String
    | PhoneInput String
    | PhoneUpdateAtProfile String
    | PhoneUpdateConfirmation String
    | ComponentsSelection String (Maybe ReportComponentsList)
    | ConfirmationBeforeSending String


type alias ReportComponentsConfig msg =
    { reportType : ReportType
    , setReportComponentsFunc : Maybe ReportComponentsList -> msg
    }


type ReportType
    = ReportWellChild
    | ReportAntenatal


type ReportComponentsList
    = WellChild (EverySet ReportComponentWellChild)
    | Antenatal (EverySet ReportComponentAntenatal)


type ReportComponentWellChild
    = ComponentWellChildActiveDiagnoses
    | ComponentWellChildImmunizationHistory
    | ComponentWellChildECD
    | ComponentWellChildGrowth
    | ComponentWellChildNextAppointment


type ReportComponentAntenatal
    = ComponentAntenatalRiskFactors
    | ComponentAntenatalMedicalDiagnoses
    | ComponentAntenatalObstetricalDiagnoses
    | ComponentAntenatalMedicalHistory
      -- @todo: implement after pane is developed.
      -- | ComponentAntenatalCHWActivity
    | ComponentAntenatalPatientProgress
    | ComponentAntenatalLabsResults
    | ComponentAntenatalProgressPhotos


type Msg msg
    = SetState (Maybe DialogState)
    | UpdatePhoneAtProfile PersonId Person String
    | SetReportComponents msg String
