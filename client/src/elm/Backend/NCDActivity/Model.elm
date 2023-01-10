module Backend.NCDActivity.Model exposing (..)


type NCDActivity
    = DangerSigns
    | Examination
    | FamilyPlanning
    | Laboratory
    | MedicalHistory
    | NextSteps
    | OutsideCare
    | SymptomReview


type NCDRecurrentActivity
    = LabResults
    | RecurrentNextSteps
