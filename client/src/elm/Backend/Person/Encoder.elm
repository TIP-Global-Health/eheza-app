module Backend.Person.Encoder exposing (..)

import Backend.Measurement.Model exposing (Gender)
import Backend.Person.Model exposing (..)
import Backend.Person.Utils
    exposing
        ( educationLevelToInt
        , genderToString
        , hivStatusToString
        , maritalStatusToString
        , modeOfDeliveryToString
        , ubudeheToInt
        )
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


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
    , ( "hiv_status", maybe encodeHIVStatus person.hivStatus )
    , ( "number_of_children", maybe int person.numberOfChildren )
    , ( "mode_of_delivery", maybe encodeModeOfDelivery person.modeOfDelivery )
    , ( "ubudehe", maybe encodeUbudehe person.ubudehe )
    , ( "education_level", maybe encodeEducationLevel person.educationLevel )
    , ( "marital_status", maybe encodeMaritalStatus person.maritalStatus )
    , ( "province", maybe string person.province )
    , ( "district", maybe string person.district )
    , ( "sector", maybe string person.sector )
    , ( "cell", maybe string person.cell )
    , ( "village", maybe string person.village )
    , ( "latitude", maybe string person.registrationLatitude )
    , ( "longitude", maybe string person.registrationLongitude )
    , ( "save_gps_location", bool person.saveGPSLocation )
    , ( "phone_number", maybe string person.telephoneNumber )
    , ( "health_center", maybe encodeEntityUuid person.healthCenterId )
    , ( "deleted", bool person.deleted )
    , ( "type", string "person" )
    ]
        ++ encodeIfSet "shard" person.shard encodeEntityUuid


encodeGender : Gender -> Value
encodeGender =
    genderToString >> string


encodeModeOfDelivery : ModeOfDelivery -> Value
encodeModeOfDelivery =
    modeOfDeliveryToString >> string


encodeHIVStatus : HIVStatus -> Value
encodeHIVStatus =
    hivStatusToString >> string


encodeUbudehe : Ubudehe -> Value
encodeUbudehe =
    ubudeheToInt >> int


encodeEducationLevel : EducationLevel -> Value
encodeEducationLevel =
    educationLevelToInt >> int


encodeMaritalStatus : MaritalStatus -> Value
encodeMaritalStatus =
    maritalStatusToString >> string
