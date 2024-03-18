module Pages.EducationSession.Model exposing (..)

import Backend.EducationSession.Model exposing (..)
import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}


type Msg
    = SetActivePage Page
