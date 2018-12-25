module Pages.PatientRegistration.Model exposing
    ( Model
    , Msg(..)
    , RegistrationForm
    , RegistrationStep(..)
    , emptyModel
    , validateRegistrationForm
    )

import Form exposing (Form)
import Form.Validate exposing (Validation, andMap, bool, field, string, succeed)
import Pages.Page exposing (Page)


type alias Model =
    { registrationForm : Form () RegistrationForm
    , registrationStep : RegistrationStep
    }


emptyModel : Model
emptyModel =
    { registrationForm = Form.initial [] validateRegistrationForm
    , registrationStep = First
    }


type RegistrationStep
    = First
    | Second
    | Third


type Msg
    = MsgRegistrationForm Form.Msg
    | SetActivePage Page
    | SetRegistrationStep RegistrationStep
    | Submit


type alias RegistrationForm =
    { firstName : String
    , secondName : String
    , nationalIdNumber : String
    , dayOfBirth : String
    , monthOfBirth : String
    , yearOfBirth : String
    , isMale : Bool
    , isFemale : Bool
    , levelOfEducation : String
    , profession : String
    , maritalStatus : String
    , hivStatus : String
    , familyUbudehe : String
    , numberOfChildren : String
    , district : String
    , sector : String
    , cell : String
    , village : String
    , telephoneNumber : String
    }


validateRegistrationForm : Validation () RegistrationForm
validateRegistrationForm =
    succeed RegistrationForm
        |> andMap (field "firstName" string)
        |> andMap (field "secondName" string)
        |> andMap (field "nationalIdNumber" string)
        |> andMap (field "dayOfBirth" string)
        |> andMap (field "monthOfBirth" string)
        |> andMap (field "yearOfBirth" string)
        |> andMap (field "isMale" bool)
        |> andMap (field "isFemale" bool)
        |> andMap (field "levelOfEducation" string)
        |> andMap (field "profession" string)
        |> andMap (field "maritalStatus" string)
        |> andMap (field "hivStatus" string)
        |> andMap (field "familyUbudehe" string)
        |> andMap (field "numberOfChildren" string)
        |> andMap (field "district" string)
        |> andMap (field "sector" string)
        |> andMap (field "cell" string)
        |> andMap (field "village" string)
        |> andMap (field "telephoneNumber" string)
