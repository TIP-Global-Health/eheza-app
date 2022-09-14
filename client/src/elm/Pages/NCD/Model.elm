module Pages.NCD.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (NCDMeasurements)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias AssembledData =
    { id : NCDEncounterId
    , encounter : NCDEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : NCDMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( NCDEncounterId, NCDMeasurements ) )
    }
