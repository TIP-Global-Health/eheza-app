module Backend.Person.Form exposing
    ( PersonForm
    , allDigitsPattern
    , applyDefaultValues
    , birthDate
    , birthDateEstimated
    , cell
    , district
    , educationLevel
    , emptyCreateForm
    , emptyEditForm
    , expectedAgeByForm
    , firstName
    , gender
    , healthCenter
    , hivStatus
    , hmisNumber
    , maritalStatus
    , modeOfDelivery
    , nationalIdNumber
    , numberOfChildren
    , phoneNumber
    , photo
    , province
    , secondName
    , sector
    , ubudehe
    , validateBirthDate
    , validateCell
    , validateDigitsOnly
    , validateDistrict
    , validateEducationLevel
    , validateGender
    , validateHealthCenterId
    , validateHivStatus
    , validateMaritalStatus
    , validateModeOfDelivery
    , validateNationalIdNumber
    , validatePerson
    , validateProvince
    , validateSector
    , validateUbudehe
    , validateVillage
    , village
    , withDefault
    )

import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeHivStatus, decodeMaritalStatus, decodeModeOfDelivery, decodeUbudehe)
import Backend.Person.Encoder
    exposing
        ( encodeEducationLevel
        , encodeHivStatus
        , encodeMaritalStatus
        , encodeModeOfDelivery
        , encodeUbudehe
        , genderToString
        )
import Backend.Person.Model exposing (..)
import Backend.Person.Utils exposing (expectedAgeByPerson, isAdult, isPersonAnAdult, resolveExpectedAge)
import Backend.Village.Model exposing (Village)
import Date
import Form exposing (..)
import Form.Field
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffYears, formatDDMMyyyy)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Regex exposing (Regex)
import Restful.Endpoint exposing (decodeEntityUuid, fromEntityId, fromEntityUuid, toEntityId, toEntityUuid)
import Translate exposing (ValidationError(..))
import Utils.Form exposing (fromDecoder, nullable)
import Utils.GeoLocation exposing (geoInfo, getGeoLocation)


type alias PersonForm =
    Form ValidationError Person


emptyCreateForm : PersonForm
emptyCreateForm =
    initial []
        (validatePerson Nothing (CreatePerson Nothing) Nothing)


emptyEditForm : PersonForm
emptyEditForm =
    initial []
        (validatePerson Nothing (toEntityUuid "1" |> EditPerson) Nothing)


{-| Given the birth date actually entered into the form, what age range are we
looking at?
-}
expectedAgeByForm : NominalDate -> PersonForm -> ParticipantDirectoryOperation -> ExpectedAge
expectedAgeByForm currentDate form operation =
    Form.getFieldAsString birthDate form
        |> .value
        |> Maybe.andThen (Date.fromIsoString >> Result.toMaybe)
        |> (\birthDate_ -> resolveExpectedAge currentDate birthDate_ operation)


applyDefaultValues : NominalDate -> Maybe Village -> Bool -> Maybe Person -> ParticipantDirectoryOperation -> PersonForm -> PersonForm
applyDefaultValues currentDate maybeVillage isChw maybeRelatedPerson operation form =
    let
        defaultProvince =
            if isChw then
                maybeVillage |> Maybe.map .province

            else
                maybeRelatedPerson
                    |> Maybe.andThen .province

        defaultProvinceId =
            defaultProvince
                |> Maybe.andThen (getGeoLocation Nothing)
                |> Maybe.map Tuple.first

        defaultDistrict =
            if isChw then
                maybeVillage |> Maybe.map .district

            else
                maybeRelatedPerson
                    |> Maybe.andThen .district

        defaultDistrictId =
            defaultDistrict
                |> Maybe.andThen (getGeoLocation defaultProvinceId)
                |> Maybe.map Tuple.first

        defaultSector =
            if isChw then
                maybeVillage |> Maybe.map .sector

            else
                maybeRelatedPerson
                    |> Maybe.andThen .sector

        defaultSectorId =
            defaultSector
                |> Maybe.andThen (getGeoLocation defaultDistrictId)
                |> Maybe.map Tuple.first

        defaultCell =
            if isChw then
                maybeVillage |> Maybe.map .cell

            else
                maybeRelatedPerson
                    |> Maybe.andThen .cell

        defaultCellId =
            defaultCell
                |> Maybe.andThen (getGeoLocation defaultSectorId)
                |> Maybe.map Tuple.first

        defaultVillage =
            if isChw then
                maybeVillage |> Maybe.map .village

            else
                maybeRelatedPerson
                    |> Maybe.andThen .village

        defaultVillageId =
            defaultVillage
                |> Maybe.andThen (getGeoLocation defaultCellId)
                |> Maybe.map Tuple.first

        defaultUbudehe =
            maybeRelatedPerson
                |> Maybe.andThen .ubudehe

        defaultHealthCenter =
            if isChw then
                maybeVillage
                    |> Maybe.map .healthCenterId

            else
                maybeRelatedPerson
                    |> Maybe.andThen .healthCenterId

        defaultHmisNumber =
            maybeRelatedPerson
                |> Maybe.andThen .hmisNumber

        defaultModeOfDelivery =
            maybeRelatedPerson
                |> Maybe.andThen .modeOfDelivery

        defaultHivStatus =
            maybeRelatedPerson
                |> Maybe.andThen .hivStatus

        defaultlEducationLevel =
            maybeRelatedPerson
                |> Maybe.andThen .educationLevel

        defaultlNumberOfChildrenl =
            maybeRelatedPerson
                |> Maybe.andThen .numberOfChildren

        defaultMaritalStatus =
            maybeRelatedPerson
                |> Maybe.andThen .maritalStatus

        defaultFirstName =
            maybeRelatedPerson |> Maybe.map .firstName

        defaultSecondName =
            maybeRelatedPerson |> Maybe.map .secondName

        defaultNationalIdNumber =
            maybeRelatedPerson |> Maybe.andThen .nationalIdNumber

        defaultBirthDate =
            maybeRelatedPerson |> Maybe.andThen .birthDate

        defaultAvatarUrl =
            maybeRelatedPerson |> Maybe.andThen .avatarUrl

        defaultTelephoneNumber =
            maybeRelatedPerson |> Maybe.andThen .telephoneNumber

        validation =
            validatePerson maybeRelatedPerson operation (Just currentDate)

        formFieldEmpty fieldName form_ =
            Form.getFieldAsString fieldName form_
                |> .value
                |> isNothing

        applyDefaultGender form_ =
            maybeRelatedPerson
                |> Maybe.map .gender
                |> unwrap
                    form_
                    (\gender_ ->
                        if formFieldEmpty gender form_ then
                            Form.update
                                validation
                                (Form.Input gender Form.Radio (Form.Field.String (genderToString gender_)))
                                form_

                        else
                            form_
                    )

        applyDefaultIsDateOfBirthEstimated form_ =
            maybeRelatedPerson
                |> Maybe.map .isDateOfBirthEstimated
                |> unwrap
                    form_
                    (\isEstimated ->
                        Form.getFieldAsBool birthDateEstimated form_
                            |> .value
                            |> isNothing
                            |> (\useDefault ->
                                    if useDefault then
                                        Form.update
                                            validation
                                            (Form.Input birthDateEstimated Form.Checkbox (Form.Field.Bool isEstimated))
                                            form_

                                    else
                                        form_
                               )
                    )

        applyDefaultTextInput fieldName maybeDefault toStringFunc form_ =
            maybeDefault
                |> unwrap
                    form_
                    (\default ->
                        if formFieldEmpty fieldName form_ then
                            Form.update
                                validation
                                (Form.Input fieldName Form.Text (Form.Field.String (toStringFunc default)))
                                form_

                        else
                            form_
                    )

        applyDefaultSelectInput fieldName maybeDefault toStringFunc form_ =
            maybeDefault
                |> unwrap
                    form_
                    (\default ->
                        if formFieldEmpty fieldName form_ then
                            Form.update
                                validation
                                (Form.Input fieldName Form.Select (Form.Field.String (toStringFunc default)))
                                form_

                        else
                            form_
                    )

        applyDefaultLocation fieldName maybeDefault form_ =
            case maybeDefault of
                Just defaultId ->
                    if formFieldEmpty fieldName form_ then
                        Form.update
                            validation
                            (Form.Input fieldName Form.Select (Form.Field.String (String.fromInt <| fromEntityId defaultId)))
                            form_

                    else
                        form_

                Nothing ->
                    form_
    in
    case operation of
        CreatePerson _ ->
            form
                |> applyDefaultSelectInput ubudehe defaultUbudehe (encodeUbudehe >> String.fromInt)
                |> applyDefaultLocation province defaultProvinceId
                |> applyDefaultLocation district defaultDistrictId
                |> applyDefaultLocation sector defaultSectorId
                |> applyDefaultLocation cell defaultCellId
                |> applyDefaultLocation village defaultVillageId
                |> applyDefaultSelectInput healthCenter defaultHealthCenter fromEntityUuid

        EditPerson _ ->
            form
                |> applyDefaultTextInput photo defaultAvatarUrl identity
                |> applyDefaultTextInput firstName defaultFirstName identity
                |> applyDefaultTextInput secondName defaultSecondName identity
                |> applyDefaultTextInput nationalIdNumber defaultNationalIdNumber identity
                |> applyDefaultTextInput birthDate defaultBirthDate formatDDMMyyyy
                |> applyDefaultIsDateOfBirthEstimated
                |> applyDefaultSelectInput hmisNumber defaultHmisNumber identity
                |> applyDefaultGender
                |> applyDefaultSelectInput hivStatus defaultHivStatus encodeHivStatus
                |> applyDefaultSelectInput educationLevel defaultlEducationLevel (encodeEducationLevel >> String.fromInt)
                |> applyDefaultSelectInput maritalStatus defaultMaritalStatus encodeMaritalStatus
                |> applyDefaultSelectInput modeOfDelivery defaultModeOfDelivery encodeModeOfDelivery
                |> applyDefaultSelectInput numberOfChildren defaultlNumberOfChildrenl String.fromInt
                |> applyDefaultSelectInput ubudehe defaultUbudehe (encodeUbudehe >> String.fromInt)
                |> applyDefaultLocation province defaultProvinceId
                |> applyDefaultLocation district defaultDistrictId
                |> applyDefaultLocation sector defaultSectorId
                |> applyDefaultLocation cell defaultCellId
                |> applyDefaultLocation village defaultVillageId
                |> applyDefaultTextInput phoneNumber defaultTelephoneNumber identity
                |> applyDefaultSelectInput healthCenter defaultHealthCenter fromEntityUuid


{-| The person supplied here is the related person, if we're constructing someone
who is the child or parent of a person we know.
-}
validatePerson : Maybe Person -> ParticipantDirectoryOperation -> Maybe NominalDate -> Validation ValidationError Person
validatePerson maybeRelated operation maybeCurrentDate =
    let
        externalExpectedAge =
            Maybe.map2
                (\related currentDate -> expectedAgeByPerson currentDate related operation)
                maybeRelated
                maybeCurrentDate
                |> Maybe.withDefault ExpectAdultOrChild

        withFirstName firstNameValue =
            andThen (withAllNames firstNameValue) (field secondName string)

        combineNames first second =
            if String.trim first == "" then
                String.trim second

            else
                [ String.trim second
                , String.trim first
                ]
                    |> String.join " "

        withAllNames firstNameValue secondNameValue =
            validateBirthDate externalExpectedAge maybeCurrentDate
                |> field birthDate
                |> andThen (withNamesAndBirthDate firstNameValue secondNameValue)

        withNamesAndBirthDate firstNameValue secondNameValue birthDate_ =
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
                                |> Maybe.andThen (\currentDate -> isAdult currentDate birthDate_)
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
                |> andMap (field hmisNumber validateHmisNumber)
                |> andMap (field photo <| nullable string)
                |> andMap (succeed birthDate_)
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
                |> andMap (succeed False)
                |> andMap (succeed Nothing)
    in
    andThen withFirstName (field firstName (oneOf [ string, emptyString ]))


validateNationalIdNumber : Validation ValidationError (Maybe String)
validateNationalIdNumber =
    string
        |> andThen
            (\s ->
                let
                    trimmed =
                        String.trim s
                in
                if String.length trimmed /= 16 then
                    fail <| customError (LengthError 16)

                else
                    format allDigitsPattern trimmed
                        |> mapError (\_ -> customError DigitsOnly)
            )
        |> nullable


validateHmisNumber : Validation ValidationError (Maybe String)
validateHmisNumber =
    string
        |> andThen
            (\s ->
                let
                    trimmed =
                        String.trim s

                    error =
                        customError InvalidHmisNumber
                in
                String.toInt s
                    |> Maybe.map
                        (\number ->
                            if number > 0 && number < 16 then
                                succeed trimmed

                            else
                                fail error
                        )
                    |> Maybe.withDefault (fail error)
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
                Dict.get (toEntityId id) geoInfo.provinces
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
                Dict.get (toEntityId id) geoInfo.districts
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
                Dict.get (toEntityId id) geoInfo.sectors
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
                Dict.get (toEntityId id) geoInfo.cells
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
                Dict.get (toEntityId id) geoInfo.villages
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
                            -- Calculate difference of years between input birth
                            -- date and current date.
                            Date.fromIsoString s
                                |> Result.toMaybe
                                |> Maybe.map
                                    (\birthDate_ ->
                                        let
                                            delta =
                                                diffYears birthDate_ currentDate
                                        in
                                        if delta > 12 && expectedAge == ExpectChild then
                                            fail <| customError InvalidBirthDateForChild
                                            -- Invalid age for child.

                                        else if delta < 13 && expectedAge == ExpectAdult then
                                            fail <| customError InvalidBirthDateForAdult
                                            -- Invalid age for adult.

                                        else
                                            succeed (Just birthDate_)
                                    )
                                |> Maybe.withDefault (fail <| customError InvalidBirthDate)
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


validateHealthCenterId : Maybe Person -> Validation ValidationError (Maybe HealthCenterId)
validateHealthCenterId related =
    fromDecoder DecoderError (Just RequiredField) (Json.Decode.nullable decodeEntityUuid)
        |> withDefault (Maybe.andThen .healthCenterId related)



-- Regex patterns


allDigitsPattern : Regex
allDigitsPattern =
    Regex.fromString "^[0-9]*$"
        |> Maybe.withDefault Regex.never



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


hmisNumber : String
hmisNumber =
    "hmis_number"


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
