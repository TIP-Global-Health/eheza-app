module Pages.NutritionEncounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (NutritionMeasurements, ObstetricHistoryValue)
import Backend.NutritionEncounter.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab }


type Msg
    = CloseEncounter NutritionEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }
