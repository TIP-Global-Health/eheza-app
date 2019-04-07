module Backend.Person.Form exposing (PersonForm, birthDate, birthDateEstimated, cell, district, educationLevel, emptyForm, firstName, gender, maritalStatus, middleName, nationalIdNumber, phoneNumber, photo, profession, province, secondName, sector, ubudehe, validateCell, validateDate, validateDistrict, validateEducationLevel, validateGender, validateMaritalStatus, validatePerson, validateProvince, validateSector, validateUbudehe, validateVillage, village)

import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (..)
import EveryDict
import Form exposing (..)
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Restful.Endpoint exposing (toEntityId)
import Translate exposing (ValidationError(..))
import Utils.Form exposing (fromDecoder, nullable)
import Utils.GeoLocation exposing (geoInfo)


type alias PersonForm =
    Form ValidationError Person


emptyForm : PersonForm
emptyForm =
    initial
        [ setBool birthDateEstimated False
        ]
        validatePerson


validatePerson : Validation ValidationError Person
validatePerson =
    let
        withFirstName firstNameValue =
            andThen (withFirstAndSecondNames firstNameValue) (field secondName string)

        withFirstAndSecondNames firstNameValue secondNameValue =
            andThen (withAllNames firstNameValue secondNameValue) (field middleName <| nullable string)

        combineNames first second middle =
            [ Just <| String.trim second
            , Just <| String.trim first
            , middle
            ]
                |> List.filterMap identity
                |> String.join " "

        withAllNames firstNameValue secondNameValue middleNameValue =
            succeed Person
                |> andMap (succeed (combineNames firstNameValue secondNameValue middleNameValue))
                |> andMap (succeed <| String.trim firstNameValue)
                |> andMap (succeed <| middleNameValue)
                |> andMap (succeed <| String.trim secondNameValue)
                |> andMap (field nationalIdNumber <| nullable string)
                |> andMap (field photo <| nullable string)
                |> andMap (field birthDate validateDate)
                |> andMap (field birthDateEstimated bool)
                |> andMap (field gender validateGender)
                |> andMap (field ubudehe validateUbudehe)
                |> andMap (field educationLevel validateEducationLevel)
                |> andMap (field profession <| nullable string)
                |> andMap (field maritalStatus validateMaritalStatus)
                |> andMap (field province validateProvince)
                |> andMap (field district validateDistrict)
                |> andMap (field sector validateSector)
                |> andMap (field cell validateCell)
                |> andMap (field village validateVillage)
                |> andMap (field phoneNumber <| nullable string)
    in
    andThen withFirstName (field firstName string)


validateProvince : Validation ValidationError (Maybe String)
validateProvince =
    int
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.provinces
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownProvince)
            )
        |> nullable


validateDistrict : Validation ValidationError (Maybe String)
validateDistrict =
    int
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.districts
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownDistrict)
            )
        |> nullable


validateSector : Validation ValidationError (Maybe String)
validateSector =
    int
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.sectors
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownSector)
            )
        |> nullable


validateCell : Validation ValidationError (Maybe String)
validateCell =
    int
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.cells
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownCell)
            )
        |> nullable


validateVillage : Validation ValidationError (Maybe String)
validateVillage =
    int
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.villages
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownVillage)
            )
        |> nullable


validateGender : Validation ValidationError Gender
validateGender =
    fromDecoder Translate.DecoderError decodeGender


validateUbudehe : Validation ValidationError (Maybe Ubudehe)
validateUbudehe =
    nullable (fromDecoder Translate.DecoderError decodeUbudehe)


validateDate : Validation ValidationError (Maybe NominalDate)
validateDate =
    nullable (fromDecoder Translate.DecoderError decodeYYYYMMDD)


validateEducationLevel : Validation ValidationError (Maybe EducationLevel)
validateEducationLevel =
    nullable (fromDecoder Translate.DecoderError decodeEducationLevel)


validateMaritalStatus : Validation ValidationError (Maybe MaritalStatus)
validateMaritalStatus =
    nullable (fromDecoder Translate.DecoderError decodeMaritalStatus)



-- Field names


firstName : String
firstName =
    "first_name"


middleName : String
middleName =
    "middle_name"


secondName : String
secondName =
    "second_name"


nationalIdNumber : String
nationalIdNumber =
    "national_id_number"


photo : String
photo =
    "photo"


birthDate : String
birthDate =
    "birth_date"


birthDateEstimated : String
birthDateEstimated =
    "birth_date_estimated"


gender : String
gender =
    "gender"


ubudehe : String
ubudehe =
    "ubudehe"


educationLevel : String
educationLevel =
    "education_level"


profession : String
profession =
    "profession"


maritalStatus : String
maritalStatus =
    "marital_status"


province : String
province =
    "province"


district : String
district =
    "district"


sector : String
sector =
    "sector"


cell : String
cell =
    "cell"


village : String
village =
    "village"


phoneNumber : String
phoneNumber =
    "phone_number"
