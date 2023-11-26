module Pages.Nutrition.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (NutritionMeasurements)
import Backend.NutritionActivity.Model exposing (NutritionActivity)
import Backend.NutritionEncounter.Model exposing (..)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , skippedActivities : EverySet NutritionActivity

    -- @todo: remove
    , showEndEncounterDialog : Bool
    , dialogState : Maybe DialogType
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , skippedActivities = EverySet.empty
    , showEndEncounterDialog = False
    , dialogState = Nothing
    }


type Msg
    = CloseEncounter NutritionEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
      -- @todo: remove
    | SetEndEncounterDialogState Bool
    | SetDialogState (Maybe DialogType)


type Tab
    = Completed
    | Pending
    | Reports


type DialogType
    = DialogEndEncounter
    | DialogSkipNCDA


type alias AssembledData =
    { id : NutritionEncounterId
    , encounter : NutritionEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : NutritionMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    }
