module Backend.Person.Form exposing (..)

import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.Measurement.Model exposing (Gender)
import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeHivStatus, decodeMaritalStatus, decodeModeOfDelivery, decodeUbudehe)
import Backend.Person.Model exposing (..)
import Backend.Person.Utils
    exposing
        ( educationLevelToInt
        , expectedAgeByPerson
        , genderToString
        , generateFullName
        , hivStatusToString
        , isAdult
        , maritalStatusToString
        , modeOfDeliveryToString
        , resolveExpectedAge
        , ubudeheToInt
        )
import Backend.Village.Model exposing (Village)
import Date
import Form exposing (..)
import Form.Field
import Form.Validate exposing (..)
import GeoLocation.Model exposing (GeoInfo, ReverseGeoInfo)
import GeoLocation.Utils exposing (getGeoInfo, getGeoLocation)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffYears, formatYYYYMMDD)
import Json.Decode
import Maybe.Extra exposing (isNothing, unwrap)
import Regex exposing (Regex)
import Restful.Endpoint exposing (decodeEntityUuid, fromEntityId, fromEntityUuid, toEntityId, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Translate exposing (ValidationError(..))
import Utils.Form exposing (fromDecoder, nullable)


type alias PersonForm =
    Form ValidationError Person


emptyCreateForm : Site -> PersonForm
emptyCreateForm site =
    initial []
        (validatePerson site Nothing (CreatePerson Nothing) Nothing)


emptyEditForm : Site -> PersonForm
emptyEditForm site =
    initial []
        (validatePerson site Nothing (toEntityUuid "1" |> EditPerson) Nothing)


type alias ContactForm =
    Form ValidationError Person


emptyContactForm : Site -> ContactForm
emptyContactForm site =
    initial [] (validateContact site)


{-| Given the birth date actually entered into the form, what age range are we
looking at?
-}
expectedAgeByForm : NominalDate -> PersonForm -> ParticipantDirectoryOperation -> ExpectedAge
expectedAgeByForm currentDate form operation =
    Form.getFieldAsString birthDate form
        |> .value
        |> Maybe.andThen (Date.fromIsoString >> Result.toMaybe)
        |> (\birthDate_ -> resolveExpectedAge currentDate birthDate_ operation)


applyDefaultValuesForPerson : NominalDate -> Site -> ReverseGeoInfo -> Maybe Village -> Bool -> Maybe Person -> ParticipantDirectoryOperation -> PersonForm -> PersonForm
applyDefaultValuesForPerson currentDate site reverseGeoInfo maybeVillage isChw maybeRelatedPerson operation form =
    let
        defaultProvince =
            if isChw then
                maybeVillage |> Maybe.map .province

            else
                maybeRelatedPerson
                    |> Maybe.andThen .province

        defaultProvinceId =
            defaultProvince
                |> Maybe.andThen (getGeoLocation reverseGeoInfo Nothing)
                |> Maybe.map Tuple.first

        defaultDistrict =
            if isChw then
                maybeVillage |> Maybe.map .district

            else
                maybeRelatedPerson
                    |> Maybe.andThen .district

        defaultDistrictId =
            defaultDistrict
                |> Maybe.andThen (getGeoLocation reverseGeoInfo defaultProvinceId)
                |> Maybe.map Tuple.first

        defaultSector =
            if isChw then
                maybeVillage |> Maybe.map .sector

            else
                maybeRelatedPerson
                    |> Maybe.andThen .sector

        defaultSectorId =
            defaultSector
                |> Maybe.andThen (getGeoLocation reverseGeoInfo defaultDistrictId)
                |> Maybe.map Tuple.first

        defaultCell =
            if isChw then
                maybeVillage |> Maybe.map .cell

            else
                maybeRelatedPerson
                    |> Maybe.andThen .cell

        defaultCellId =
            defaultCell
                |> Maybe.andThen (getGeoLocation reverseGeoInfo defaultSectorId)
                |> Maybe.map Tuple.first

        defaultVillage =
            if isChw then
                maybeVillage |> Maybe.map .village

            else
                maybeRelatedPerson
                    |> Maybe.andThen .village

        defaultVillageId =
            defaultVillage
                |> Maybe.andThen (getGeoLocation reverseGeoInfo defaultCellId)
                |> Maybe.map Tuple.first

        defaultUbudehe =
            -- Ubudehe is Rwanda specific. For all other sites
            -- we preset NoUbudehe, and the inoput is hidden on form.
            if site == SiteRwanda then
                Maybe.andThen .ubudehe maybeRelatedPerson

            else
                Just NoUbudehe

        defaultHealthCenter =
            if isChw then
                maybeVillage
                    |> Maybe.map .healthCenterId

            else
                maybeRelatedPerson
                    |> Maybe.andThen .healthCenterId

        validation =
            validatePerson site maybeRelatedPerson operation (Just currentDate)

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
                |> applyDefaultSelectInput ubudehe defaultUbudehe (ubudeheToInt >> String.fromInt)
                |> applyDefaultLocation province defaultProvinceId
                |> applyDefaultLocation district defaultDistrictId
                |> applyDefaultLocation sector defaultSectorId
                |> applyDefaultLocation cell defaultCellId
                |> applyDefaultLocation village defaultVillageId
                |> applyDefaultSelectInput healthCenter defaultHealthCenter fromEntityUuid

        EditPerson _ ->
            let
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
            in
            form
                |> applyDefaultTextInput photo defaultAvatarUrl identity
                |> applyDefaultTextInput firstName defaultFirstName identity
                |> applyDefaultTextInput secondName defaultSecondName identity
                |> applyDefaultTextInput nationalIdNumber defaultNationalIdNumber identity
                |> applyDefaultTextInput birthDate defaultBirthDate formatYYYYMMDD
                |> applyDefaultIsDateOfBirthEstimated
                |> applyDefaultSelectInput hmisNumber defaultHmisNumber identity
                |> applyDefaultGender
                |> applyDefaultSelectInput hivStatus defaultHivStatus hivStatusToString
                |> applyDefaultSelectInput educationLevel defaultlEducationLevel (educationLevelToInt >> String.fromInt)
                |> applyDefaultSelectInput maritalStatus defaultMaritalStatus maritalStatusToString
                |> applyDefaultSelectInput modeOfDelivery defaultModeOfDelivery modeOfDeliveryToString
                |> applyDefaultSelectInput numberOfChildren defaultlNumberOfChildrenl String.fromInt
                |> applyDefaultSelectInput ubudehe defaultUbudehe (ubudeheToInt >> String.fromInt)
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
validatePerson : Site -> Maybe Person -> ParticipantDirectoryOperation -> Maybe NominalDate -> Validation ValidationError Person
validatePerson site maybeRelated operation maybeCurrentDate =
    let
        geoInfo =
            getGeoInfo site

        externalExpectedAge =
            Maybe.map2
                (\related currentDate -> expectedAgeByPerson currentDate related operation)
                maybeRelated
                maybeCurrentDate
                |> Maybe.withDefault ExpectAdultOrChild

        withFirstName firstNameValue =
            andThen (withAllNames firstNameValue) (field secondName string)

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

                nationalIdNumberValidator =
                    case site of
                        SiteBurundi ->
                            nullable string

                        _ ->
                            validateNationalIdNumber
            in
            succeed Person
                |> andMap (succeed (generateFullName firstNameValue secondNameValue))
                |> andMap (succeed <| String.trim firstNameValue)
                |> andMap (succeed <| String.trim secondNameValue)
                |> andMap (field nationalIdNumber nationalIdNumberValidator)
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
                |> andMap (field province (validateProvince geoInfo maybeRelated))
                |> andMap (field district (validateDistrict geoInfo maybeRelated))
                |> andMap (field sector (validateSector geoInfo maybeRelated))
                |> andMap (field cell (validateCell geoInfo maybeRelated))
                |> andMap (field village (validateVillage geoInfo maybeRelated))
                |> andMap (field phoneNumber <| nullable validateDigitsOnly)
                |> andMap (field healthCenter (validateHealthCenterId maybeRelated))
                |> andMap (succeed False)
                |> andMap (succeed Nothing)
    in
    andThen withFirstName (field firstName (oneOf [ string, emptyString ]))


validateContact : Site -> Validation ValidationError Person
validateContact site =
    let
        geoInfo =
            getGeoInfo site

        withFirstName firstNameValue =
            andThen (withAllNames firstNameValue) (field secondName string)

        withAllNames firstNameValue secondNameValue =
            succeed Person
                |> andMap (succeed (generateFullName firstNameValue secondNameValue))
                |> andMap (succeed <| String.trim firstNameValue)
                |> andMap (succeed <| String.trim secondNameValue)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed False)
                |> andMap (field gender validateGender)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (succeed Nothing)
                |> andMap (field province (validateProvinceForContact geoInfo))
                |> andMap (field district (validateDistrictForContact geoInfo))
                |> andMap (field sector (validateSectorForContact geoInfo))
                |> andMap (field cell (validateCellForContact geoInfo))
                |> andMap (field village (validateVillageForContact geoInfo))
                |> andMap (field phoneNumber <| nullable validateDigitsOnly)
                |> andMap (succeed Nothing)
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
                    error =
                        customError InvalidHmisNumber
                in
                String.toInt s
                    |> Maybe.map
                        (\number ->
                            if number > 0 && number < 16 then
                                succeed <| String.trim s

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


validateProvince : GeoInfo -> Maybe Person -> Validation ValidationError (Maybe String)
validateProvince geoInfo related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.provinces
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownProvince)
            )
        |> withDefault (Maybe.andThen .province related)


validateProvinceForContact : GeoInfo -> Validation ValidationError (Maybe String)
validateProvinceForContact geoInfo =
    int
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.provinces
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownProvince)
            )
        |> nullable


validateDistrict : GeoInfo -> Maybe Person -> Validation ValidationError (Maybe String)
validateDistrict geoInfo related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.districts
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownDistrict)
            )
        |> withDefault (Maybe.andThen .district related)


validateDistrictForContact : GeoInfo -> Validation ValidationError (Maybe String)
validateDistrictForContact geoInfo =
    int
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.districts
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownDistrict)
            )
        |> nullable


validateSector : GeoInfo -> Maybe Person -> Validation ValidationError (Maybe String)
validateSector geoInfo related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.sectors
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownSector)
            )
        |> withDefault (Maybe.andThen .sector related)


validateSectorForContact : GeoInfo -> Validation ValidationError (Maybe String)
validateSectorForContact geoInfo =
    int
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.sectors
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownSector)
            )
        |> nullable


validateCell : GeoInfo -> Maybe Person -> Validation ValidationError (Maybe String)
validateCell geoInfo related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.cells
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownCell)
            )
        |> withDefault (Maybe.andThen .cell related)


validateCellForContact : GeoInfo -> Validation ValidationError (Maybe String)
validateCellForContact geoInfo =
    int
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.cells
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownCell)
            )
        |> nullable


validateVillage : GeoInfo -> Maybe Person -> Validation ValidationError (Maybe String)
validateVillage geoInfo related =
    int
        |> mapError (\_ -> customError RequiredField)
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.villages
                    |> Maybe.map (.name >> Just >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownVillage)
            )
        |> withDefault (Maybe.andThen .village related)


validateVillageForContact : GeoInfo -> Validation ValidationError (Maybe String)
validateVillageForContact geoInfo =
    int
        |> andThen
            (\id ->
                Dict.get (toEntityId id) geoInfo.villages
                    |> Maybe.map (.name >> succeed)
                    |> Maybe.withDefault (fail <| customError UnknownVillage)
            )
        |> nullable


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
    nullable <| fromDecoder DecoderError Nothing decodeHivStatus


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
