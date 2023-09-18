module Pages.Device.Model exposing (Model, Msg(..), emptyModel)

import Pages.Page exposing (Page)
import SyncManager.Model


type alias Model =
    { -- The pairing code entered in the UI
      code : String
    }


emptyModel : Model
emptyModel =
    { code = ""
    }


type Msg
    = SetActivePage Page
    | SetCode String
    | HandlePairClicked
    | MsgSyncManager SyncManager.Model.Msg
