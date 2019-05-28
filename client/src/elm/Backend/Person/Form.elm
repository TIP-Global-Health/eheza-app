module Backend.Person.Form exposing (ExpectedAge(..), PersonForm, birthDate, birthDateEstimated, cell, district, educationLevel, emptyForm, expectedAgeFromForm, firstName, gender, maritalStatus, nationalIdNumber, phoneNumber, photo, province, secondName, sector, ubudehe, validateBirthDate, validateCell, validateDistrict, validateEducationLevel, validateGender, validateMaritalStatus, validatePerson, validateProvince, validateSector, validateUbudehe, validateVillage, village)

import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (..)
import Backend.Person.Utils exposing (isAdult)
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


{-| Sometimes, we are in a state where we are expecting the user to enter an
adult or a child, and sometimes we don't care -- they could be entering either.

This controls various aspects of validation. If set to `ExpectAdultOrChild`, we
also check the birth date actually entered in the form, and validate the rest
according that birth date.

-}
type ExpectedAge
    = ExpectAdult
    | ExpectChild
    | ExpectAdultOrChild


{-| Given the birth date actually entered into the form, what age range are we
looking at?
-}
expectedAgeFromForm : NominalDate -> PersonForm -> ExpectedAge
expectedAgeFromForm currentDate form =
    Form.getFieldAsString birthDate form
        |> .value
        |> Maybe.andThen (Date.fromString >> Result.toMaybe)
        |> Maybe.map fromLocalDateTime
        |> isAdult currentDate
        |> (\adult ->
                case adult of
                    Just True ->
                        ExpectAdult

                    Just False ->
                        ExpectChild

                    Nothing ->
                        ExpectAdultOrChild
           )


emptyForm : PersonForm
emptyForm =
    initial
        [ setBool birthDateEstimated False
        ]
        (validatePerson ExpectAdultOrChild Nothing)


{-| The `ExpectedAge` here is the external expectation ... that is, the
external limit on what the age ought to be, so we can't set an unexpected birth
date. Once the birth date is set in the form, we also apply internal logic
based on the birth date actually entered.
-}
validatePerson : ExpectedAge -> Maybe NominalDate -> Validation ValidationError Person
validatePerson externalExpectedAge maybeCurrentDate =
    let
        withFirstName firstNameValue =
            andThen (withAllNames firstNameValue) (field secondName validateLettersOnly)

        combineNames first second =
            [ String.trim second
            , String.trim first
            ]
                |> String.join " "

        withAllNames firstNameValue secondNameValue =
            validateBirthDate externalExpectedAge maybeCurrentDate
                |> field birthDate
                |> andThen (withNamesAndBirthDate firstNameValue secondNameValue)

        withNamesAndBirthDate firstNameValue secondNameValue birthDate =
            let
                expectedAge =
                    case externalExpectedAge of
                        ExpectChild ->
                            ExpectChild

                        ExpectAdult ->
                            ExpectAdult

                        ExpectAdultOrChild ->
                            -- If we could accept either, then see what birthdate
                            -- has actually been entered, if any.
                            maybeCurrentDate
                                |> Maybe.andThen (\currentDate -> isAdult currentDate birthDate)
                                |> (\isAdult ->
                                        case isAdult of
                                            Just True ->
                                                ExpectAdult

                                            Just False ->
                                                ExpectChild

                                            Nothing ->
                                                ExpectAdultOrChild
                                   )
            in
            succeed Person
                |> andMap (succeed (combineNames firstNameValue secondNameValue))
                |> andMap (succeed <| String.trim firstNameValue)
                |> andMap (succeed <| String.trim secondNameValue)
                |> andMap (field nationalIdNumber validateNationalIdNumber)
                |> andMap (field photo <| nullable string)
                |> andMap (succeed birthDate)
                |> andMap (field birthDateEstimated bool)
                |> andMap (field gender validateGender)
                |> andMap (field ubudehe validateUbudehe)
                |> andMap (field educationLevel <| validateEducationLevel expectedAge)
                |> andMap (field maritalStatus <| validateMaritalStatus expectedAge)
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
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.provinces
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownProvince)
            )


validateDistrict : Validation ValidationError (Maybe String)
validateDistrict =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.districts
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownDistrict)
            )


validateSector : Validation ValidationError (Maybe String)
validateSector =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.sectors
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownSector)
            )


validateCell : Validation ValidationError (Maybe String)
validateCell =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.cells
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownCell)
            )


validateVillage : Validation ValidationError (Maybe String)
validateVillage =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                EveryDict.get (toEntityId id) geoInfo.villages
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownVillage)
            )


validateGender : Validation ValidationError Gender
validateGender =
    fromDecoder DecoderError (Just RequiredField) decodeGender


validateUbudehe : Validation ValidationError (Maybe Ubudehe)
validateUbudehe =
    fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeUbudehe)


validateBirthDate : ExpectedAge -> Maybe NominalDate -> Validation ValidationError (Maybe NominalDate)
validateBirthDate expectedAge maybeCurrentDate =
    string
        |> mapError (\_ -> customError RequiredField)
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
                                        if delta > 12 && expectedAge == ExpectChild then
                                            fail <| customError InvalidBirthDateForChild
                                            -- Invalid age for child.

                                        else if delta < 13 && expectedAge == ExpectAdult then
                                            fail <| customError InvalidBirthDateForAdult
                                            -- Invalid age for adult.

                                        else
                                            succeed maybeBirthDate
                                    )
                        )
            )


validateEducationLevel : ExpectedAge -> Validation ValidationError (Maybe EducationLevel)
validateEducationLevel expectedAge =
    if expectedAge == ExpectAdult then
        fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeEducationLevel)

    else
        -- It's not required for others, but we'll keep it if provided.
        fromDecoder DecoderError Nothing (Json.Decode.nullable decodeEducationLevel)


validateMaritalStatus : ExpectedAge -> Validation ValidationError (Maybe MaritalStatus)
validateMaritalStatus expectedAge =
    if expectedAge == ExpectAdult then
        fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeMaritalStatus)

    else
        -- Not required, but keep it if provided.
        fromDecoder DecoderError Nothing (Json.Decode.nullable decodeMaritalStatus)


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
