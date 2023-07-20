module Pages.ChildScoreboard.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (NCDAData, NCDAForm, NCDAStep, emptyNCDAData)
import Pages.Page exposing (Page)


type alias Model =
    { ncdaData : NCDAData
    }


emptyModel : Model
emptyModel =
    { ncdaData = emptyNCDAData
    }


type Msg
    = SetActivePage Page
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetNumberANCVisits String
    | SetNutritionSupplementType NutritionSupplementType
    | SetNCDAFormStep NCDAStep
    | SetNCDAHelperState (Maybe NCDASign)
    | SaveNCDA PersonId (Maybe ( ChildScoreboardNCDAId, ChildScoreboardNCDA ))
