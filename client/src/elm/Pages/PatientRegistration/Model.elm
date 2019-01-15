module Pages.PatientRegistration.Model exposing
    ( DialogState(..)
    , Model
    , Msg(..)
    , RegistrationForm
    , RegistrationStep(..)
    , emptyModel
    , initModel
    , validateRegistrationForm
    )

import Backend.Child.Model exposing (Child)
import Backend.Measurement.Model exposing (PhotoValue)
import Backend.Mother.Model exposing (Mother)
import EveryDict exposing (EveryDict)
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Validate exposing (Validation, andMap, andThen, bool, emptyString, field, format, mapError, oneOf, string, succeed)
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)
import Regex exposing (Regex)
import Uuid exposing (Uuid)


type alias Model =
    { photo : Maybe PhotoValue
    , registrationForm : Form () RegistrationForm
    , registrationStep : RegistrationStep
    , participantsData : ParticipantsData
    , dialogState : Maybe DialogState
    }


type alias ParticipantsData =
    { mothersToRegister : EveryDict Uuid Mother
    , childrenToRegister : EveryDict Uuid Child
    }


emptyModel : Model
emptyModel =
    { photo = Nothing
    , registrationForm = Form.initial [] validateRegistrationForm
    , registrationStep = First
    , participantsData = emptyParticipantsData
    , dialogState = Nothing
    }


initModel : Model -> Model
initModel model =
    { emptyModel | participantsData = model.participantsData }


emptyParticipantsData : ParticipantsData
emptyParticipantsData =
    { mothersToRegister = EveryDict.empty
    , childrenToRegister = EveryDict.empty
    }


type RegistrationStep
    = First
    | Second
    | Third


type Msg
    = AddNewPatient (Maybe Uuid)
    | DropZoneComplete DropZoneFile
    | MsgRegistrationForm Form.Msg
    | Reset
    | SetActivePage Page
    | SetDialogState (Maybe DialogState)
    | SetRegistrationStep RegistrationStep
    | Submit


type DialogState
    = ConfirmSubmision
    | SuccessfulSubmision


type alias RegistrationForm =
    { firstName : String
    , middleName : String
    , secondName : String
    , nationalIdNumber : String
    , dayOfBirth : String
    , monthOfBirth : String
    , yearOfBirth : String
    , isDateOfBirthEstimated : Bool
    , gender : String
    , levelOfEducation : String
    , profession : String
    , maritalStatus : String
    , hivStatus : String
    , modeOfDelivery : String
    , familyUbudehe : String
    , householdSize : String
    , numberOfChildren : String
    , motherName : String
    , motherNationalId : String
    , fatherName : String
    , fatherNationalId : String
    , caregiverName : String
    , caregiverNationalId : String
    , district : String
    , sector : String
    , cell : String
    , village : String
    , telephoneNumber : String
    , healthCenterName : String
    }


validateRegistrationForm : Validation () RegistrationForm
validateRegistrationForm =
    succeed RegistrationForm
        |> andMap (field "firstName" string)
        |> andMap (field "middleName" string)
        |> andMap (field "secondName" string)
        |> andMap (field "nationalIdNumber" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "dayOfBirth" string)
        |> andMap (field "monthOfBirth" string)
        |> andMap (field "yearOfBirth" string)
        |> andMap (field "isDateOfBirthEstimated" bool)
        |> andMap (field "gender" string)
        |> andMap (field "levelOfEducation" string)
        |> andMap (field "profession" string)
        |> andMap (field "maritalStatus" string)
        |> andMap (field "hivStatus" string)
        |> andMap (field "modeOfDelivery" string)
        |> andMap (field "familyUbudehe" string)
        |> andMap (field "householdSize" string)
        |> andMap (field "numberOfChildren" string)
        |> andMap (field "motherName" string)
        |> andMap (field "motherNationalId" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "fatherName" string)
        |> andMap (field "fatherNationalId" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "caregiverName" string)
        |> andMap (field "caregiverNationalId" (oneOf [ emptyString, validateAlphanumeric ]))
        |> andMap (field "district" string)
        |> andMap (field "sector" string)
        |> andMap (field "cell" string)
        |> andMap (field "village" string)
        |> andMap (field "telephoneNumber" string)
        |> andMap (field "healthCenterName" string)


validateAlphanumeric : Validation e String
validateAlphanumeric =
    string
        |> andThen
            (\s ->
                format alphanumericPattern s
                    |> mapError (\_ -> Form.Error.value InvalidFormat)
            )


alphanumericPattern : Regex
alphanumericPattern =
    Regex.regex "^[a-zA-Z0-9]*$"
