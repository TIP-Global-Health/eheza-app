module Pages.ChildScoreboard.Encounter.Model exposing (..)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    {}


type Msg
    = CloseEncounter ChildScoreboardEncounterId
    | SetActivePage Page


emptyModel : Model
emptyModel =
    {}
