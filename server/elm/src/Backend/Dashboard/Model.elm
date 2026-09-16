module Backend.Dashboard.Model exposing (Msg(..))

import Json.Encode exposing (Value)


type Msg
    = SetData Value
