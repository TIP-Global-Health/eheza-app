module Pages.NutritionEncounter.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (NutritionMeasurements, ObstetricHistoryValue)
import Backend.NutritionEncounter.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Pages.Page exposing (Page)


type alias Model =
    {}


type Msg
    = CloseEncounter NutritionEncounterId
    | SetActivePage Page


emptyModel : Model
emptyModel =
    {}
