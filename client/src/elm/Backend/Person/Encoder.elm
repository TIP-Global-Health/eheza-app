module Backend.Person.Encoder exposing
    ( encodeEducationLevel
    , encodeGender
    , encodeHivStatus
    , encodeMaritalStatus
    , encodeModeOfDelivery
    , encodePerson
    , encodeUbudehe
    )

import Backend.Measurement.Model exposing (Gender(..))
import Backend.Person.Model exposing (..)
import Backend.Person.Utils exposing (genderToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


encodePerson : Person -> List ( String, Value )
encodePerson person =
    [ ( "label", string person.name )
    , ( "first_name", string person.firstName )
    , ( "second_name", string person.secondName )
    , ( "national_id_number", maybe string person.nationalIdNumber )
    , ( "hmis_number", maybe string person.hmisNumber )
    , ( "photo", maybe string person.avatarUrl )
    , ( "birth_date", maybe encodeYYYYMMDD person.birthDate )
    , ( "birth_date_estimated", bool person.isDateOfBirthEstimated )
    , ( "gender", encodeGender person.gender )
    , ( "hiv_status", maybe (encodeHivStatus >> string) person.hivStatus )
    , ( "number_of_children", maybe int person.numberOfChildren )
    , ( "mode_of_delivery", maybe (encodeModeOfDelivery >> string) person.modeOfDelivery )
    , ( "ubudehe", maybe (encodeUbudehe >> int) person.ubudehe )
    , ( "education_level", maybe (encodeEducationLevel >> int) person.educationLevel )
    , ( "marital_status", maybe (encodeMaritalStatus >> string) person.maritalStatus )
    , ( "province", maybe string person.province )
    , ( "district", maybe string person.district )
    , ( "sector", maybe string person.sector )
    , ( "cell", maybe string person.cell )
    , ( "village", maybe string person.village )
    , ( "phone_number", maybe string person.telephoneNumber )
    , ( "health_center", maybe encodeEntityUuid person.healthCenterId )
    , ( "deleted", bool person.deleted )
    , ( "type", string "person" )
    ]
        ++ encodeIfExists "shard" person.shard encodeEntityUuid


encodeModeOfDelivery : ModeOfDelivery -> String
encodeModeOfDelivery mode =
    case mode of
        VaginalDelivery vaginal ->
            case vaginal of
                Spontaneous True ->
                    "svd-episiotomy"

                Spontaneous False ->
                    "svd-no-episiotomy"

                WithVacuumExtraction ->
                    "vd-vacuum"

        CesareanDelivery ->
            "cesarean-delivery"


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


encodeGender : Gender -> Value
encodeGender gender =
    string <|
        genderToString gender


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
