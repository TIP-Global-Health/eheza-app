module Pages.PatientRegistration.Model exposing
    ( Model
    , Msg(..)
    , RegistrationForm
    , emptyModel
    , validateRegistrationForm
    )

import Form exposing (Form)
import Form.Validate exposing (Validation, andMap, bool, field, string, succeed)
import Pages.Page exposing (Page)


type alias Model =
    { registrationForm : Form () RegistrationForm
    }


emptyModel : Model
emptyModel =
    { registrationForm = Form.initial [] validateRegistrationForm
    }


type Msg
    = MsgRegistrationForm Form.Msg
    | SetActivePage Page


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
