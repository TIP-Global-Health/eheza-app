module Components.SendViaWhatsAppDialog.Model exposing (..)

import Backend.Entities exposing (..)


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


type Msg
    = SetState (Maybe DialogState)
    | SetInputNumber String
