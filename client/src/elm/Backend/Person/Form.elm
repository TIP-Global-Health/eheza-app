module Backend.Person.Form exposing (PersonForm, birthDate, birthDateEstimated, cell, district, educationLevel, emptyForm, firstName, gender, maritalStatus, nationalIdNumber, phoneNumber, photo, province, secondName, sector, ubudehe, validateBirthDate, validateCell, validateDistrict, validateEducationLevel, validateGender, validateMaritalStatus, validatePerson, validateProvince, validateSector, validateUbudehe, validateVillage, village)

import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (..)
import Date
import EveryDict
import Form exposing (..)
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, fromLocalDateTime)
import Json.Decode
import Maybe.Extra exposing (unwrap)
import Regex exposing (Regex)
import Restful.Endpoint exposing (toEntityId)
import Time.Date
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
        (validatePerson False Nothing)


validatePerson : Bool -> Maybe NominalDate -> Validation ValidationError Person
validatePerson isAdult maybeCurrentDate =
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
                |> andMap (field birthDate <| validateBirthDate isAdult maybeCurrentDate)
                |> andMap (field birthDateEstimated bool)
                |> andMap (field gender validateGender)
                |> andMap (field ubudehe validateUbudehe)
                |> andMap (field educationLevel <| validateEducationLevel isAdult)
                |> andMap (field maritalStatus <| validateMaritalStatus isAdult)
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
    fromDecoder DecoderError (Just ReqiuredField) decodeGender


validateUbudehe : Validation ValidationError (Maybe Ubudehe)
validateUbudehe =
    fromDecoder DecoderError (Just ReqiuredField) (Json.Decode.nullable decodeUbudehe)


validateBirthDate : Bool -> Maybe NominalDate -> Validation ValidationError (Maybe NominalDate)
validateBirthDate isAdult maybeCurrentDate =
    string
        |> mapError (\_ -> customError ReqiuredField)
        |> andThen
            (\s ->
                maybeCurrentDate
                    |> unwrap
                        -- When we don't know current date, try to decode input value.
                        (fromDecoder DecoderError Nothing (Json.Decode.nullable decodeYYYYMMDD))
                        (\currentDate ->
                            let
                                -- Convert to NominalDate.
                                maybeBirthDate =
                                    Date.fromString s
                                        |> Result.toMaybe
                                        |> Maybe.map fromLocalDateTime
                            in
                            maybeBirthDate
                                -- Calculate difference of years between input birt
                                -- date and current date.
                                |> Maybe.map (Time.Date.delta currentDate >> .years)
                                |> unwrap
                                    -- Conversion to NominalDate failed.
                                    (fail <| customError InvalidBirthDate)
                                    (\delta ->
                                        if delta > 12 && not isAdult then
                                            fail <| customError InvalidBirthDateForChild
                                            -- Invalid age for child.

                                        else if delta < 13 && isAdult then
                                            fail <| customError InvalidBirthDateForAdult
                                            -- Invalid age for adult.

                                        else
                                            succeed maybeBirthDate
                                    )
                        )
            )


validateEducationLevel : Bool -> Validation ValidationError (Maybe EducationLevel)
validateEducationLevel isAdult =
    if not isAdult then
        succeed Nothing

    else
        fromDecoder DecoderError (Just ReqiuredField) (Json.Decode.nullable decodeEducationLevel)


validateMaritalStatus : Bool -> Validation ValidationError (Maybe MaritalStatus)
validateMaritalStatus isAdult =
    if not isAdult then
        succeed Nothing

    else
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
