module Pages.Admin.Model exposing (..)

import Backend.Model
import Pages.Page exposing (Page)


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}


type Msg
    = SetActivePage Page
    | MsgBackend Backend.Model.MsgBackend
