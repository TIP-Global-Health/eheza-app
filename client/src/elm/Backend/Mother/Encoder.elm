module Backend.Mother.Encoder exposing (encodeChildrenRelation, encodeEducationLevel, encodeHivStatus, encodeMaritalStatus, encodeMother)

import Backend.Mother.Model exposing (..)
import Backend.Patient.Encoder exposing (encodeUbudehe)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)


encodeMother : Mother -> List ( String, Value )
encodeMother mother =
    [ ( "label", string mother.name )
    , ( "avatar", maybe string mother.avatarUrl )
    , ( "date_birth", maybe encodeYYYYMMDD mother.birthDate )
    , ( "relation", encodeChildrenRelation mother.relation )
    , ( "ubudehe", maybe (encodeUbudehe >> int) mother.ubudehe )
    , ( "education_level", maybe (encodeEducationLevel >> int) mother.educationLevel )
    ]


encodeChildrenRelation : ChildrenRelationType -> Value
encodeChildrenRelation relation =
    case relation of
        MotherRelation ->
            string "mother"

        CaregiverRelation ->
            string "caregiver"


encodeEducationLevel : EducationLevel -> Int
encodeEducationLevel educationLevel =
    case educationLevel of
        NoSchooling ->
            0

        PrimarySchool ->
            1

        VocationalTrainingSchool ->
            2

        SecondarySchool ->
            3

        DiplomaProgram ->
            4

        HigherEducation ->
            5

        AdvancedDiploma ->
            6


encodeHivStatus : HIVStatus -> String
encodeHivStatus status =
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


encodeMaritalStatus : MaritalStatus -> String
encodeMaritalStatus status =
    case status of
        Divorced ->
            "divorced"

        Married ->
            "married"

        Single ->
            "single"

        Widowed ->
            "widowed"
