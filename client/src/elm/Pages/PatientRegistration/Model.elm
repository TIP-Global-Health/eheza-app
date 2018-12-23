module Pages.PatientRegistration.Model exposing
    ( Model
    , Msg(..)
    , RegistrationForm
    , emptyModel
    , validateRegistrationForm
    )

import Form exposing (Form)
import Form.Validate exposing (Validation, andMap, field, int, string, succeed)
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
    , dayOfBirth : Int
    , monthOfBirth : Int
    , yearOfBirth : Int
    }


validateRegistrationForm : Validation () RegistrationForm
validateRegistrationForm =
    succeed RegistrationForm
        |> andMap (field "firstName" string)
        |> andMap (field "secondName" string)
        |> andMap (field "nationalIdNumber" string)
        |> andMap (field "dayOfBirth" int)
        |> andMap (field "monthOfBirth" int)
        |> andMap (field "yearOfBirth" int)
