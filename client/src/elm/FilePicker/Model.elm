module FilePicker.Model exposing (..)


type alias DropzoneConfig =
    { active : Bool
    , backendUrl : String
    , defaultMessage : String
    }


{-| These are the messages to send to the UI for the file picker.
-}
type Msg
    = Bind
    | Unbind


type alias Model =
    { isBound : Bool
    }


emptyModel : Model
emptyModel =
    { isBound = False
    }
