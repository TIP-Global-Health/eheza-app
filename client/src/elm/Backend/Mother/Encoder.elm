module Backend.Mother.Encoder exposing (encodeChildrenRelation, encodeEducationLevel, encodeHivStatus, encodeMaritalStatus, encodeMother, encodeUbudehe)

import Backend.Mother.Model exposing (..)
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
    , ( "education_level", maybe encodeEducationLevel mother.educationLevel )
    ]


encodeChildrenRelation : ChildrenRelationType -> Value
encodeChildrenRelation relation =
    case relation of
        MotherRelation ->
            string "mother"

        CaregiverRelation ->
            string "caregiver"


encodeUbudehe : Ubudehe -> Int
encodeUbudehe ubudehe =
    case ubudehe of
        Ubudehe1 ->
            1

        Ubudehe2 ->
            2

        Ubudehe3 ->
            3

        Ubudehe4 ->
            4


encodeEducationLevel : EducationLevel -> Value
encodeEducationLevel educationLevel =
    case educationLevel of
        NoSchooling ->
            int 0

        PrimarySchool ->
            int 1

        VocationalTrainingSchool ->
            int 2

        SecondarySchool ->
            int 3

        DiplomaProgram ->
            int 4

        HigherEducation ->
            int 5

        AdvancedDiploma ->
            int 6


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
