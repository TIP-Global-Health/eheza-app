module Pages.Report.Model exposing (..)

import Backend.Measurement.Model
import Gizra.NominalDate exposing (NominalDate)


type alias LabsResultsDisplayConfig =
    { bloodGpRs : Bool
    , hemoglobin : Bool
    , hepatitisB : Bool
    , hivPCR : Bool
    , malaria : Bool
    , syphilis : Bool
    }


type alias LabsResultsValues encounterId =
    { hiv : List Backend.Measurement.Model.HIVTestValue
    , randomBloodSugar : List (Backend.Measurement.Model.RandomBloodSugarTestValue encounterId)
    , urineDipstick : List Backend.Measurement.Model.UrineDipstickTestValue
    , bloodGpRs : List (Backend.Measurement.Model.BloodGpRsTestValue encounterId)
    , hemoglobin : List Backend.Measurement.Model.HemoglobinTestValue
    , hepatitisB : List (Backend.Measurement.Model.HepatitisBTestValue encounterId)
    , hivPCR : List Backend.Measurement.Model.HIVPCRTestValue
    , malaria : List Backend.Measurement.Model.MalariaTestValue
    , syphilis : List (Backend.Measurement.Model.SyphilisTestValue encounterId)
    }
