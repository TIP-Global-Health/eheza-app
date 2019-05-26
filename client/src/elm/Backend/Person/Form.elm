module Backend.Person.Form exposing (PersonForm, birthDate, birthDateEstimated, cell, district, educationLevel, emptyForm, firstName, gender, maritalStatus, nationalIdNumber, phoneNumber, photo, province, secondName, sector, ubudehe, validateCell, validateDate, validateDistrict, validateEducationLevel, validateGender, validateMaritalStatus, validatePerson, validateProvince, validateSector, validateUbudehe, validateVillage, village)

import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (..)
import EveryDict
import Form exposing (..)
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
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
                |> andMap (field nationalIdNumber <| nullable string)
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
                |> andMap (field phoneNumber <| nullable string)
    in
    andThen withFirstName (field firstName validateLettersOnly)


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


validateLettersOnly : Validation ValidationError String
validateLettersOnly =
    string
        |> andThen
            (\s ->
                format lettersOnlyPattern s
                    |> mapError (\_ -> customError LettersOnly)
            )


lettersOnlyPattern : Regex
lettersOnlyPattern =
    Regex.regex "^[a-zA-Z]*$"



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
