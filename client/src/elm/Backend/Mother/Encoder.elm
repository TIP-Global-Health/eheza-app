module Backend.Mother.Encoder exposing (encodeChildrenRelation, encodeEducationLevel, encodeHivStatus, encodeMaritalStatus, encodeMother)

import Backend.Mother.Model exposing (..)
import Backend.Participant.Encoder exposing (encodeGender, encodeUbudehe)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeMother : Mother -> List ( String, Value )
encodeMother mother =
    [ ( "label", string mother.name )
    , ( "first_name", string mother.firstName )
    , ( "middle_name", maybe string mother.middleName )
    , ( "second_name", string mother.secondName )
    , ( "national_id_number", maybe string mother.nationalIdNumber )
    , ( "avatar", maybe string mother.avatarUrl )
    , ( "date_birth", maybe encodeYYYYMMDD mother.birthDate )
    , ( "birth_date_estimated", bool mother.isDateOfBirthEstimated )
    , ( "relation", encodeChildrenRelation mother.relation )
    , ( "gender", encodeGender mother.gender )
    , ( "ubudehe", maybe (encodeUbudehe >> int) mother.ubudehe )
    , ( "education_level", maybe (encodeEducationLevel >> int) mother.educationLevel )
    , ( "profession", maybe string mother.profession )
    , ( "marital_status", maybe (encodeMaritalStatus >> string) mother.maritalStatus )
    , ( "hiv_status", maybe (encodeHivStatus >> string) mother.hivStatus )
    , ( "household_size", maybe int mother.householdSize )
    , ( "number_of_children", maybe int mother.numberOfChildren )
    , ( "province", maybe string mother.province )
    , ( "district", maybe string mother.district )
    , ( "sector", maybe string mother.sector )
    , ( "cell", maybe string mother.cell )
    , ( "village", maybe string mother.village )
    , ( "phone_number", maybe string mother.telephoneNumber )
    , ( "clinic", maybe encodeEntityUuid mother.clinic )
    , ( "health_center", maybe encodeEntityUuid mother.healthCenter )
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
