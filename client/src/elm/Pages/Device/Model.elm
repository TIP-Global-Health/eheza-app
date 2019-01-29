module Pages.Device.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)


type alias Model =
    { -- The pairing code entered in the UI
      code : String
    }


emptyModel : Model
emptyModel =
    { code = ""
    }


type Msg
    = SetCode String
    | HandlePairClicked
    | TrySyncing
    | SetSyncing HealthCenterId Bool
