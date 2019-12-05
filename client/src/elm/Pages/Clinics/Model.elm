module Pages.Clinics.Model exposing (Model, Msg(..), emptyModel)

import Backend.Clinic.Model exposing (ClinicType)
import Backend.Model
import Pages.Page exposing (Page)


type alias Model =
    { clinicType : Maybe ClinicType
    }


emptyModel : Model
emptyModel =
    { clinicType = Nothing
    }


type Msg
    = MsgIndexedDb Backend.Model.MsgIndexedDb
    | SetActivePage Page
    | SetClinicType (Maybe ClinicType)
