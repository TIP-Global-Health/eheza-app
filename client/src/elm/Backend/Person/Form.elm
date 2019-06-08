module Backend.Person.Form exposing (ExpectedAge(..), PersonForm, allDigitsPattern, allLettersPattern, birthDate, birthDateEstimated, cell, district, educationLevel, emptyForm, expectedAgeFromForm, firstName, gender, healthCenter, hivStatus, maritalStatus, modeOfDelivery, nationalIdNumber, numberOfChildren, phoneNumber, photo, province, secondName, sector, ubudehe, validateBirthDate, validateCell, validateDigitsOnly, validateDistrict, validateEducationLevel, validateGender, validateHealthCenterId, validateHivStatus, validateLettersOnly, validateMaritalStatus, validateModeOfDelivery, validateNationalIdNumber, validatePerson, validateProvince, validateSector, validateUbudehe, validateVillage, village, withDefault)

import AllDict
import Backend.Entities exposing (HealthCenterId)
import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeHivStatus, decodeMaritalStatus, decodeModeOfDelivery, decodeUbudehe)
import Backend.Person.Model exposing (..)
import Backend.Person.Utils exposing (isAdult, isPersonAnAdult)
import Form exposing (..)
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, fromLocalDateTime)
import Json.Decode
import Maybe.Extra exposing (join, unwrap)
import Regex exposing (Regex)
import Restful.Endpoint exposing (decodeEntityUuid, toEntityId)
import Time.Date
import Time.Iso8601
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
    -- Our dates are formatted as 2019-07-02, which, strangely, Date.fromString
    -- doesn't handle correctly. So, we use Time.Iso8601 instead.
    Form.getFieldAsString birthDate form
        |> .value
        |> Maybe.andThen (Time.Iso8601.toDate >> Result.toMaybe)
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
        (validatePerson Nothing Nothing)


{-| The person supplied here is the related person, if we're constructing someone
who is the child or parent of a person we know.
-}
validatePerson : Maybe Person -> Maybe NominalDate -> Validation ValidationError Person
validatePerson maybeRelated maybeCurrentDate =
    let
        externalExpectedAge =
            Maybe.map2
                (\related currentDate ->
                    case isPersonAnAdult currentDate related of
                        Just True ->
                            -- If the related person is an adult, we expect a child
                            ExpectChild

                        Just False ->
                            -- If they are a child, we expect an adult
                            ExpectAdult

                        Nothing ->
                            -- If we don't know, we expect either
                            ExpectAdultOrChild
                )
                maybeRelated
                maybeCurrentDate
                |> Maybe.withDefault ExpectAdultOrChild

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
                |> andMap (field hivStatus validateHivStatus)
                |> andMap (field numberOfChildren <| nullable int)
                |> andMap (field modeOfDelivery <| validateModeOfDelivery expectedAge)
                |> andMap (field ubudehe (validateUbudehe maybeRelated))
                |> andMap (field educationLevel <| validateEducationLevel expectedAge)
                |> andMap (field maritalStatus <| validateMaritalStatus expectedAge)
                |> andMap (field province (validateProvince maybeRelated))
                |> andMap (field district (validateDistrict maybeRelated))
                |> andMap (field sector (validateSector maybeRelated))
                |> andMap (field cell (validateCell maybeRelated))
                |> andMap (field village (validateVillage maybeRelated))
                |> andMap (field phoneNumber <| nullable validateDigitsOnly)
                |> andMap (field healthCenter (validateHealthCenterId maybeRelated))
    in
    andThen withFirstName (field firstName validateLettersOnly)


validateNationalIdNumber : Validation ValidationError (Maybe String)
validateNationalIdNumber =
    string
        |> andThen
            (\s ->
                let
                    trimmed =
                        String.trim s
                in
                if String.length trimmed /= 18 then
                    fail <| customError (LengthError 18)

                else
                    format allDigitsPattern trimmed
                        |> mapError (\_ -> customError DigitsOnly)
            )
        |> nullable


withDefault : Maybe a -> Validation ValidationError (Maybe a) -> Validation ValidationError (Maybe a)
withDefault related =
    case related of
        Just _ ->
            defaultValue related

        Nothing ->
            identity


validateProvince : Maybe Person -> Validation ValidationError (Maybe String)
validateProvince related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                AllDict.get (toEntityId id) geoInfo.provinces
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownProvince)
            )
        |> withDefault (Maybe.andThen .province related)


validateDistrict : Maybe Person -> Validation ValidationError (Maybe String)
validateDistrict related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                AllDict.get (toEntityId id) geoInfo.districts
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownDistrict)
            )
        |> withDefault (Maybe.andThen .district related)


validateSector : Maybe Person -> Validation ValidationError (Maybe String)
validateSector related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                AllDict.get (toEntityId id) geoInfo.sectors
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownSector)
            )
        |> withDefault (Maybe.andThen .sector related)


validateCell : Maybe Person -> Validation ValidationError (Maybe String)
validateCell related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                AllDict.get (toEntityId id) geoInfo.cells
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownCell)
            )
        |> withDefault (Maybe.andThen .cell related)


validateVillage : Maybe Person -> Validation ValidationError (Maybe String)
validateVillage related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                AllDict.get (toEntityId id) geoInfo.villages
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownVillage)
            )
        |> withDefault (Maybe.andThen .village related)


validateGender : Validation ValidationError Gender
validateGender =
    fromDecoder DecoderError (Just RequiredField) decodeGender


validateUbudehe : Maybe Person -> Validation ValidationError (Maybe Ubudehe)
validateUbudehe related =
    fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeUbudehe)
        |> withDefault (Maybe.andThen .ubudehe related)


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
                                    Time.Iso8601.toDate s
                                        |> Result.toMaybe
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


validateHivStatus : Validation ValidationError (Maybe HIVStatus)
validateHivStatus =
    fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeHivStatus)


validateModeOfDelivery : ExpectedAge -> Validation ValidationError (Maybe ModeOfDelivery)
validateModeOfDelivery expectedAge =
    if expectedAge == ExpectChild then
        fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeModeOfDelivery)

    else
        -- It's not required for others, but we'll keep it if provided.
        nullable <| fromDecoder DecoderError Nothing decodeModeOfDelivery


validateEducationLevel : ExpectedAge -> Validation ValidationError (Maybe EducationLevel)
validateEducationLevel expectedAge =
    if expectedAge == ExpectAdult then
        fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeEducationLevel)

    else
        -- It's not required for others, but we'll keep it if provided.
        nullable <| fromDecoder DecoderError Nothing decodeEducationLevel


validateMaritalStatus : ExpectedAge -> Validation ValidationError (Maybe MaritalStatus)
validateMaritalStatus expectedAge =
    if expectedAge == ExpectAdult then
        fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeMaritalStatus)

    else
        -- Not required, but keep it if provided.
        nullable <| fromDecoder DecoderError Nothing decodeMaritalStatus


validateDigitsOnly : Validation ValidationError String
validateDigitsOnly =
    string
        |> andThen
            (\s ->
                String.trim s
                    |> format allDigitsPattern
                    |> mapError (\_ -> customError DigitsOnly)
            )


validateLettersOnly : Validation ValidationError String
validateLettersOnly =
    string
        |> andThen
            (\s ->
                String.trim s
                    |> format allLettersPattern
                    |> mapError (\_ -> customError LettersOnly)
            )


validateHealthCenterId : Maybe Person -> Validation ValidationError (Maybe HealthCenterId)
validateHealthCenterId related =
    fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeEntityUuid)
        |> withDefault (Maybe.andThen .healthCenterId related)



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


healthCenter : String
healthCenter =
    "health_center"


numberOfChildren : String
numberOfChildren =
    "number_of_children"


modeOfDelivery : String
modeOfDelivery =
    "mode_of_delivery"


hivStatus : String
hivStatus =
    "hiv_status"
