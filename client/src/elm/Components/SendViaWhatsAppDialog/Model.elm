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
    | PhoneInput PhoneData
    | PhoneUpdateAtProfile String
    | PhoneUpdateConfirmation String
    | ComponentsSelection String (Maybe ReportComponentsList)
    | ConfirmationBeforeExecuting String
    | ExecutionResult (Maybe String)


type alias PhoneData =
    { countryCode : CountryCode
    , phone : String
    }


emptyPhoneData : PhoneData
emptyPhoneData =
    { countryCode = CountryCodeRwanda
    , phone = ""
    }


type CountryCode
    = CountryCodeRwanda
    | CountryCodeUganda
    | CountryCodeCongo
    | CountryCodeKenya
    | CountryCodeTanzania
    | CountryCodeBurundi
    | CountryCodeUSACanada


type alias ReportComponentsConfig msg =
    { setReportComponentsMsg : Maybe ReportComponentsList -> msg }


type ReportType
    = ReportWellChild
    | ReportAntenatal
    | ReportAcuteIllness


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
      -- @todo: implement after pane is developed.
      -- | ComponentAntenatalCHWActivity
    | ComponentAntenatalPatientProgress
    | ComponentAntenatalLabsResults
    | ComponentAntenatalProgressPhotos


type Msg msg
    = SetState (Maybe DialogState)
    | SetPhoneNumber String
    | SetCountryCode String
    | UpdatePhoneAtProfile PersonId Person String
    | SetReportComponents msg String
    | Execute ReportType PersonId String
    | CancelExecute (Maybe msg)
    | SetExecutionResult (Maybe msg) String
