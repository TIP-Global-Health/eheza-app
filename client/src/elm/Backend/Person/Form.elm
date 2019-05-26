module Backend.Person.Form exposing (PersonForm, birthDate, birthDateEstimated, cell, district, educationLevel, emptyForm, firstName, gender, maritalStatus, nationalIdNumber, phoneNumber, photo, province, secondName, sector, ubudehe, validateCell, validateDate, validateDistrict, validateEducationLevel, validateGender, validateMaritalStatus, validatePerson, validateProvince, validateSector, validateUbudehe, validateVillage, village)

import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (..)
import EveryDict
import Form exposing (..)
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode
import Regex exposing (Regex)
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
            andThen (withAllNames firstNameValue) (field secondName validateLettersOnly)

        combineNames first second =
            [ String.trim second
            , String.trim first
            ]
                |> String.join " "

        withAllNames firstNameValue secondNameValue =
            succeed Person
                |> andMap (succeed (combineNames firstNameValue secondNameValue))
                |> andMap (succeed <| String.trim firstNameValue)
                |> andMap (succeed <| String.trim secondNameValue)
                |> andMap (field nationalIdNumber validateNationalIdNumber)
                |> andMap (field photo <| nullable string)
                |> andMap (field birthDate validateDate)
                |> andMap (field birthDateEstimated bool)
                |> andMap (field gender validateGender)
                |> andMap (field ubudehe validateUbudehe)
                |> andMap (field educationLevel validateEducationLevel)
                |> andMap (field maritalStatus validateMaritalStatus)
                |> andMap (field province validateProvince)
                |> andMap (field district validateDistrict)
                |> andMap (field sector validateSector)
                |> andMap (field cell validateCell)
                |> andMap (field village validateVillage)
                |> andMap (field phoneNumber <| nullable validateDigitsOnly)
    in
    andThen withFirstName (field firstName validateLettersOnly)


validateNationalIdNumber : Validation ValidationError (Maybe String)
validateNationalIdNumber =
    string
        |> andThen
            (\s ->
                if String.length s /= 18 then
                    fail <| customError (LengthError 18)

                else
                    format allDigitsPattern s
                        |> mapError (\_ -> customError DigitsOnly)
            )
        |> nullable


validateProvince : Validation ValidationError (Maybe String)
validateProvince =
    int
        |> mapError (\_ -> customError ReqiuredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.provinces
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownProvince)
            )


validateDistrict : Validation ValidationError (Maybe String)
validateDistrict =
    int
        |> mapError (\_ -> customError ReqiuredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.districts
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownDistrict)
            )


validateSector : Validation ValidationError (Maybe String)
validateSector =
    int
        |> mapError (\_ -> customError ReqiuredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.sectors
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownSector)
            )


validateCell : Validation ValidationError (Maybe String)
validateCell =
    int
        |> mapError (\_ -> customError ReqiuredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.cells
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownCell)
            )


validateVillage : Validation ValidationError (Maybe String)
validateVillage =
    int
        |> mapError (\_ -> customError ReqiuredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.villages
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownVillage)
            )


validateGender : Validation ValidationError Gender
validateGender =
    fromDecoder DecoderError Nothing decodeGender


validateUbudehe : Validation ValidationError (Maybe Ubudehe)
validateUbudehe =
    fromDecoder DecoderError (Just ReqiuredField) (Json.Decode.nullable decodeUbudehe)


validateDate : Validation ValidationError (Maybe NominalDate)
validateDate =
    fromDecoder DecoderError (Just ReqiuredField) (Json.Decode.nullable decodeYYYYMMDD)


validateEducationLevel : Validation ValidationError (Maybe EducationLevel)
validateEducationLevel =
    fromDecoder DecoderError (Just ReqiuredField) (Json.Decode.nullable decodeEducationLevel)


validateMaritalStatus : Validation ValidationError (Maybe MaritalStatus)
validateMaritalStatus =
    fromDecoder DecoderError (Just ReqiuredField) (Json.Decode.nullable decodeMaritalStatus)


validateDigitsOnly : Validation ValidationError String
validateDigitsOnly =
    string
        |> andThen
            (\s ->
                format allDigitsPattern s
                    |> mapError (\_ -> customError DigitsOnly)
            )


validateLettersOnly : Validation ValidationError String
validateLettersOnly =
    string
        |> andThen
            (\s ->
                format allLettersPattern s
                    |> mapError (\_ -> customError LettersOnly)
            )



-- Regex patterns


allLettersPattern : Regex
allLettersPattern =
    Regex.regex "^[a-zA-Z]*$"


allDigitsPattern : Regex
allDigitsPattern =
    Regex.regex "^[0-9]*$"



-- Field names


firstName : String
firstName =
    "first_name"


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
