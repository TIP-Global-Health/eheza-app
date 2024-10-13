module Components.ReportToWhatsAppDialog.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Translate.Model exposing (Language)


type alias Model =
    { state : Maybe DialogState }


emptyModel : Model
emptyModel =
    { state = Nothing }


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
      -- We have this for testing only.
      -- Israel will not appear in available options.
    | CountryCodeIsrael


type alias ReportComponentsConfig msg =
    { setReportComponentsMsg : Maybe ReportComponentsList -> msg }


type ReportType
    = ReportAcuteIllness
    | ReportAntenatal
    | ReportNCD
    | ReportTuberculosis
    | ReportWellChild


type ReportComponentsList
    = WellChild (EverySet ReportComponentWellChild)
    | Antenatal (EverySet ReportComponentAntenatal)
    | NCD (EverySet ReportComponentNCD)


type ReportComponentWellChild
    = ComponentWellChildActiveDiagnoses
    | ComponentWellChildImmunizationHistory
    | ComponentWellChildECD
    | ComponentWellChildGrowth
    | ComponentWellChildNextAppointment


type ReportComponentAntenatal
    = ComponentAntenatalObstetricHistory
    | ComponentAntenatalMedicalHistory
    | ComponentAntenatalMedicalDiagnosis
    | ComponentAntenatalObstetricalDiagnosis
    | ComponentAntenatalImmunizationHistory
    | ComponentAntenatalCHWActivity
    | ComponentAntenatalPatientProgress
    | ComponentAntenatalLabsResults
    | ComponentAntenatalProgressPhotos


type ReportComponentNCD
    = ComponentNCDRiskFactors
    | ComponentNCDActiveDiagnosis
    | ComponentNCDMedicalDiagnosis
    | ComponentNCDPatientProgress
    | ComponentNCDLabsResults


type Msg msg
    = SetState (Maybe DialogState)
    | SetPhoneNumber String
    | SetCountryCode String
    | UpdatePhoneAtProfile PersonId Person String
    | SetReportComponents msg String
    | Execute Language ReportType PersonId String
    | CancelExecute (Maybe msg)
    | SetExecutionResult (Maybe msg) String
