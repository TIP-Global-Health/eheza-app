module Backend.Child.Encoder exposing (encodeChild, encodeModeOfDelivery, encodeMotherField)

import Backend.Child.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.Person.Encoder exposing (encodeGender, encodeUbudehe)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeChild : Child -> List ( String, Value )
encodeChild child =
    [ ( "label", string child.name )
    , ( "first_name", string child.firstName )
    , ( "middle_name", maybe string child.middleName )
    , ( "second_name", string child.secondName )
    , ( "national_id_number", maybe string child.nationalIdNumber )
    , ( "avatar", maybe string child.avatarUrl )
    , encodeMotherField child.motherId
    , ( "date_birth", encodeYYYYMMDD child.birthDate )
    , ( "birth_date_estimated", bool child.isDateOfBirthEstimated )
    , ( "gender", encodeGender child.gender )
    , ( "mode_of_delivery", maybe (encodeModeOfDelivery >> string) child.modeOfDelivery )
    , ( "ubudehe", maybe (encodeUbudehe >> int) child.ubudehe )
    , ( "mother_name", maybe string child.motherName )
    , ( "mother_national_id", maybe string child.motherNationalId )
    , ( "father_name", maybe string child.fatherName )
    , ( "father_national_id", maybe string child.fatherNationalId )
    , ( "caregiver_name", maybe string child.caregiverName )
    , ( "caregiver_national_id", maybe string child.caregiverNationalId )
    , ( "province", maybe string child.province )
    , ( "district", maybe string child.district )
    , ( "sector", maybe string child.sector )
    , ( "cell", maybe string child.cell )
    , ( "village", maybe string child.village )
    , ( "phone_number", maybe string child.telephoneNumber )
    , ( "health_center", maybe encodeEntityUuid child.healthCenter )
    ]


encodeMotherField : Maybe MotherId -> ( String, Value )
encodeMotherField id =
    ( "mother", maybe encodeEntityUuid id )


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
