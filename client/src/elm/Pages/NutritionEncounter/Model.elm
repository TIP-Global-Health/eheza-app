module Pages.NutritionEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildNutritionSign, NutritionMeasurements, ObstetricHistoryValue)
import Backend.NutritionEncounter.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    }


type Msg
    = CloseEncounter NutritionEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }


type alias AssembledData =
    { id : NutritionEncounterId
    , encounter : NutritionEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : NutritionMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    }
