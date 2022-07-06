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
    | PhoneVerification


type Msg
    = SetState (Maybe DialogState)
