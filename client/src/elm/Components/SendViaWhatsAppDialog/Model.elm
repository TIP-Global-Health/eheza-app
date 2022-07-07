module Components.SendViaWhatsAppDialog.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)


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


type alias ReportComponentsConfig msg =
    { reportType : ReportType
    , setReportComponentsFunc : Maybe ReportComponentsList -> msg
    }


type ReportType
    = ReportWellChild
    | ReportAntenatal


type ReportComponentsList
    = WellChild (List ReportComponentWellChild)
    | Antnatal (List ReportComponentAntnatal)


type ReportComponentWellChild
    = ComponentWellChildActiveDiagnoses
    | ComponentWellChildImmunizationHistory
    | ComponentWellChildECD
    | ComponentWellChildGrowth
    | ComponentWellChildNextAppointment


type ReportComponentAntnatal
    = ComponentAntnatalRiskFactors
    | ComponentAntnatalMedicalDiagnoses
    | ComponentAntnatalObstetricalDiagnoses
    | ComponentAntnatalMedicalHistory
      -- @todo: implemnt after pane is developed.
      -- | ComponentAntnatalCHWActivity
    | ComponentAntnatalPatientProgress
    | ComponentAntnatalLabsResults
    | ComponentAntnatalProgressPhotos


type Msg
    = SetState (Maybe DialogState)
    | UpdatePhoneAtProfile PersonId Person String
