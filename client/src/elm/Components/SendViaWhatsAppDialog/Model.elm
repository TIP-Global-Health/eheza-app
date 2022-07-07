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


type Msg
    = SetState (Maybe DialogState)
    | UpdatePhoneAtProfile PersonId Person
