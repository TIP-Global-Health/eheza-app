module Backend.Mother.Model exposing (ChildrenRelationType(..), EducationLevel(..), HIVStatus(..), MaritalStatus(..), Mother, Ubudehe(..), hivStatusToValue)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type ChildrenRelationType
    = MotherRelation
    | CaregiverRelation


type alias Mother =
    { name : String
    , avatarUrl : Maybe String
    , children : List ChildId
    , birthDate : Maybe NominalDate
    , relation : ChildrenRelationType
    , ubudehe : Maybe Ubudehe
    , educationLevel : Maybe EducationLevel
    }


type Ubudehe
    = Ubudehe1
    | Ubudehe2
    | Ubudehe3
    | Ubudehe4


type EducationLevel
    = NoSchooling
    | PrimarySchool
    | VocationalTrainingSchool
    | SecondarySchool
    | DiplomaProgram
    | HigherEducation
    | AdvancedDiploma


type MaritalStatus
    = Divorced
    | Married
    | Single
    | Widowed


type HIVStatus
    = HIVExposedInfant
    | Negative
    | NegativeDiscordantCouple
    | Positive
    | Unknown


hivStatusToValue : HIVStatus -> String
hivStatusToValue status =
    case status of
        HIVExposedInfant ->
            "hiv-exposed-infant"

        Negative ->
            "negative"

        NegativeDiscordantCouple ->
            "negative-dc"

        Positive ->
            "positive"

        Unknown ->
            "unknown"
