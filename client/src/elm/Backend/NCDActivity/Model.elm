module Backend.NCDActivity.Model exposing (..)


type NCDActivity
    = DangerSigns
    | Examination
    | FamilyPlanning
    | Laboratory
    | MedicalHistory
    | NextSteps
    | SymptomReview


type NCDRecurrentActivity
    = LabResults
    | RecurrentNextSteps
