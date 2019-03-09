module Pages.ParticipantRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Child.Encoder exposing (encodeModeOfDelivery)
import Backend.Child.Model exposing (ModeOfDelivery(..), allModesOfDelivery)
import Backend.Measurement.Model exposing (PhotoValue)
import Backend.Model exposing (ModelBackend, ModelIndexedDb, MsgBackend(..))
import Backend.Mother.Encoder exposing (encodeEducationLevel, encodeHivStatus, encodeMaritalStatus)
import Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..), allEducationLevels, allHivStatuses, allMaritalStatuses)
import Backend.Participant.Encoder exposing (encodeUbudehe)
import Backend.Participant.Model exposing (Gender(..), Ubudehe(..), allUbudehes)
import Dict
import EveryDict
import EveryDictList
import Form exposing (Form)
import Form.Error
import Form.Input
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.ParticipantRegistration.Model exposing (..)
import Pages.ParticipantRegistration.Utils exposing (getFormFieldValue, getRegistratingParticipant)
import Participant.Model exposing (Participant, ParticipantId(..), ParticipantType(..))
import Participant.Utils exposing (childParticipant, motherParticipant)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Time.Date
import Translate exposing (Language, TranslationId, translate)
import User.Model exposing (User)
import Utils.Form exposing (isFormFieldSet, isFormFieldValid)
import Utils.GeoLocation exposing (GeoInfo, geoInfo)
import Utils.Html exposing (script, thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> ModelIndexedDb -> Model -> Html Msg
view language currentDate db model =
    div [ class "wrap wrap-alt-2" ]
        [ viewHeader language currentDate model
        , div
            [ class "ui full segment blue" ]
            [ viewBody language currentDate db model
            ]
        , viewModal <| viewDialog language model.dialogState db
        ]


viewHeader : Language -> NominalDate -> Model -> Html Msg
viewHeader language currentDate model =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.RegisterNewParticipant ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewBody : Language -> NominalDate -> ModelIndexedDb -> Model -> Html Msg
viewBody language currentDate db model =
    let
        previousPhase =
            List.head model.previousPhases

        body =
            case model.registrationPhase of
                ParticipantSearch searchString ->
                    viewSearchForm language currentDate db searchString model.submittedSearch model.relationParticipant

                ParticipantRegistration step ->
                    viewRegistrationForm language currentDate step model.registrationForm geoInfo model.photo model.relationParticipant previousPhase

                ParticipantView participantId ->
                    viewParticipantDetailsForm language currentDate participantId db previousPhase
    in
    div [ class "content" ]
        [ body ]


viewRegistrationForm :
    Language
    -> NominalDate
    -> RegistrationStep
    -> Form () RegistrationForm
    -> GeoInfo
    -> Maybe PhotoValue
    -> Maybe ParticipantId
    -> Maybe RegistrationPhase
    -> Html Msg
viewRegistrationForm language currentDate step registrationForm geoInfo photo maybeRelationParticipant maybePreviousPhase =
    let
        -- FORM FIELDS --
        firstName =
            Form.getFieldAsString "firstName" registrationForm

        middleName =
            Form.getFieldAsString "middleName" registrationForm

        secondName =
            Form.getFieldAsString "secondName" registrationForm

        nationalIdNumber =
            Form.getFieldAsString "nationalIdNumber" registrationForm

        dayOfBirth =
            Form.getFieldAsString "dayOfBirth" registrationForm

        monthOfBirth =
            Form.getFieldAsString "monthOfBirth" registrationForm

        yearOfBirth =
            Form.getFieldAsString "yearOfBirth" registrationForm

        isDateOfBirthEstimated =
            Form.getFieldAsBool "isDateOfBirthEstimated" registrationForm

        gender =
            Form.getFieldAsString "gender" registrationForm

        levelOfEducation =
            Form.getFieldAsString "levelOfEducation" registrationForm

        profession =
            Form.getFieldAsString "profession" registrationForm

        maritalStatus =
            Form.getFieldAsString "maritalStatus" registrationForm

        hivStatus =
            Form.getFieldAsString "hivStatus" registrationForm

        modeOfDelivery =
            Form.getFieldAsString "modeOfDelivery" registrationForm

        -- END STEP 1 FIELDS --
        familyUbudehe =
            Form.getFieldAsString "familyUbudehe" registrationForm

        householdSize =
            Form.getFieldAsString "householdSize" registrationForm

        numberOfChildren =
            Form.getFieldAsString "numberOfChildren" registrationForm

        motherName =
            Form.getFieldAsString "motherName" registrationForm

        motherNationalId =
            Form.getFieldAsString "motherNationalId" registrationForm

        fatherName =
            Form.getFieldAsString "fatherName" registrationForm

        fatherNationalId =
            Form.getFieldAsString "fatherNationalId" registrationForm

        caregiverName =
            Form.getFieldAsString "caregiverName" registrationForm

        caregiverNationalId =
            Form.getFieldAsString "caregiverNationalId" registrationForm

        province =
            Form.getFieldAsString "province" registrationForm

        district =
            Form.getFieldAsString "district" registrationForm

        sector =
            Form.getFieldAsString "sector" registrationForm

        cell =
            Form.getFieldAsString "cell" registrationForm

        village =
            Form.getFieldAsString "village" registrationForm

        telephoneNumber =
            Form.getFieldAsString "telephoneNumber" registrationForm

        -- END STEP 2 FIELDS --
        healthCenterName =
            Form.getFieldAsString "healthCenterName" registrationForm

        -- END STEP 3 FIELDS --
        maybeRegistratingParticipant =
            getRegistratingParticipant currentDate
                (getFormFieldValue dayOfBirth)
                (getFormFieldValue monthOfBirth)
                (getFormFieldValue yearOfBirth)
                maybeRelationParticipant

        emptyOption =
            ( "", "" )

        formContent =
            case step of
                First ->
                    let
                        staticComponents =
                            let
                                dayOptions =
                                    emptyOption
                                        :: (List.repeat 31 "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                           )

                                monthOptions =
                                    emptyOption
                                        :: (List.repeat 12 "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                           )

                                currentYear =
                                    Time.Date.year currentDate

                                ( totalYears, startFromYear ) =
                                    maybeRelationParticipant
                                        |> unwrap
                                            ( 100, currentYear - 99 )
                                            (\relationParticipant ->
                                                case relationParticipant of
                                                    ParticipantChild _ ->
                                                        ( 88, currentYear - 99 )

                                                    ParticipantMother _ ->
                                                        ( 13, currentYear - 12 )
                                            )

                                yearOptions =
                                    emptyOption
                                        :: (List.repeat totalYears "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + startFromYear, toString <| index + startFromYear ))
                                                |> List.reverse
                                           )
                            in
                            [ viewTextInput language Translate.FirstName firstName True
                            , viewTextInput language Translate.MiddleName middleName False
                            , viewTextInput language Translate.SecondName secondName True
                            , viewTextInput language Translate.NationalIdNumber nationalIdNumber False
                            , div [ class "ui grid" ]
                                [ div [ class "six wide column birtdate-label required" ]
                                    [ text <| translate language Translate.DateOfBirth ++ ":" ]
                                , div [ class "three wide column month-input" ]
                                    [ span [] [ text <| translate language Translate.Month ]
                                    , Form.Input.selectInput monthOptions monthOfBirth []
                                    ]
                                , div [ class "three wide column day-input" ]
                                    [ span [] [ text <| translate language Translate.Day ]
                                    , Form.Input.selectInput dayOptions dayOfBirth []
                                    ]
                                , div [ class "three wide column year-input" ]
                                    [ span [] [ text <| translate language Translate.Year ]
                                    , Form.Input.selectInput yearOptions yearOfBirth []
                                    ]
                                , div [ class "one wide column estimated-input" ]
                                    [ span [] [ text <| translate language Translate.Estimated ]
                                    , Form.Input.checkboxInput isDateOfBirthEstimated []
                                    ]
                                ]
                            ]

                        dynamicComponents =
                            let
                                viewAge delta =
                                    let
                                        age =
                                            if delta.years > 0 then
                                                if delta.years == 1 then
                                                    translate language <| Translate.ChartPhrase Translate.OneYear

                                                else
                                                    translate language <| Translate.ChartPhrase (Translate.YearsPlural delta.years)

                                            else if delta.months > 0 then
                                                if delta.months == 1 then
                                                    translate language <| Translate.AgeSingleMonthWithoutDay 1

                                                else
                                                    translate language <| Translate.AgeMonthsWithoutDay delta.months

                                            else if delta.days == 1 then
                                                translate language <| Translate.AgeSingleDayWithoutMonth 0 1

                                            else
                                                translate language <| Translate.AgeDays delta.days
                                    in
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
                                            [ text "Age:" ]
                                        , div [ class "ten wide column" ]
                                            [ text age ]
                                        ]

                                viewGender =
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
                                            [ text <| translate language Translate.GenderLabel ++ ":" ]
                                        , Form.Input.radioInput "male" gender [ class "one wide column gender-input" ]
                                        , div [ class "three wide column" ]
                                            [ text <| translate language (Translate.Gender Male) ]
                                        , Form.Input.radioInput "female" gender [ class "one wide column gender-input" ]
                                        , div [ class "three wide column" ]
                                            [ text <| translate language (Translate.Gender Female) ]
                                        ]
                            in
                            case maybeRegistratingParticipant of
                                Just (MotherParticipant age) ->
                                    let
                                        motherInputs =
                                            let
                                                viewLevelOfEducation =
                                                    let
                                                        options =
                                                            allEducationLevels
                                                                |> List.map
                                                                    (\level ->
                                                                        ( toString (encodeEducationLevel level)
                                                                        , translate language (Translate.LevelOfEducation level)
                                                                        )
                                                                    )
                                                                |> (::) emptyOption
                                                    in
                                                    viewSelectInput language Translate.LevelOfEducationLabel options levelOfEducation "ten" "select-input" True

                                                viewProfession =
                                                    viewTextInput language Translate.Profession profession False

                                                viewMaritalStatus =
                                                    let
                                                        options =
                                                            allMaritalStatuses
                                                                |> List.map
                                                                    (\status ->
                                                                        ( encodeMaritalStatus status
                                                                        , translate language <| Translate.MaritalStatus status
                                                                        )
                                                                    )
                                                                |> (::) emptyOption
                                                    in
                                                    viewSelectInput language Translate.MaritalStatusLabel options maritalStatus "ten" "select-input" True

                                                viewHIVStatus =
                                                    let
                                                        options =
                                                            allHivStatuses
                                                                |> List.map
                                                                    (\status ->
                                                                        ( encodeHivStatus status
                                                                        , translate language (Translate.HIVStatus status)
                                                                        )
                                                                    )
                                                                |> (::) emptyOption
                                                    in
                                                    viewSelectInput language Translate.HIVStatusLabel options hivStatus "ten" "select-input" True
                                            in
                                            [ viewGender
                                            , viewLevelOfEducation
                                            , viewProfession
                                            , viewMaritalStatus
                                            , viewHIVStatus
                                            ]
                                    in
                                    viewAge age :: motherInputs

                                Just (ChildParticipant age) ->
                                    let
                                        childInputs =
                                            let
                                                viewModeOfDelivery =
                                                    let
                                                        options =
                                                            allModesOfDelivery
                                                                |> List.map
                                                                    (\mode ->
                                                                        ( encodeModeOfDelivery mode
                                                                        , translate language (Translate.ModeOfDelivery mode)
                                                                        )
                                                                    )
                                                                |> (::) emptyOption
                                                    in
                                                    viewSelectInput language Translate.ModeOfDeliveryLabel options modeOfDelivery "ten" "select-input" True
                                            in
                                            [ viewGender
                                            , viewModeOfDelivery
                                            ]
                                    in
                                    viewAge age :: childInputs

                                Nothing ->
                                    []
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.ParticipantDemographicInformation ++ ":" ]
                    , viewPhoto language photo
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form" ] <|
                            staticComponents
                                ++ dynamicComponents
                    ]

                Second ->
                    let
                        viewFamilyUbudehe =
                            let
                                options =
                                    allUbudehes
                                        |> List.map
                                            (\ubudehe ->
                                                ( toString ubudehe
                                                , toString (encodeUbudehe ubudehe)
                                                )
                                            )
                                        |> (::) emptyOption
                            in
                            viewSelectInput language Translate.FamilyUbudehe options familyUbudehe "ten" "select-input" True

                        familyInfoInputs =
                            viewFamilyUbudehe
                                :: (case maybeRegistratingParticipant of
                                        Just (MotherParticipant _) ->
                                            let
                                                viewHouseholdSize =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: (List.repeat 30 "."
                                                                        |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                                                   )
                                                    in
                                                    viewSelectInput language Translate.HouseholdSize options householdSize "four" "" False

                                                viewNumberOfChildren =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: (List.repeat 21 "."
                                                                        |> List.indexedMap (\index _ -> ( toString index, toString index ))
                                                                   )
                                                    in
                                                    viewSelectInput language Translate.NumberOfChildren options numberOfChildren "four" "" True
                                            in
                                            [ viewHouseholdSize, viewNumberOfChildren ]

                                        Just (ChildParticipant _) ->
                                            [ viewTextInput language Translate.MotherNameLabel motherName True
                                            , viewTextInput language Translate.MotherNationalId motherNationalId False
                                            , viewTextInput language Translate.FatherName fatherName True
                                            , viewTextInput language Translate.FatherNationalId fatherNationalId False
                                            , viewTextInput language Translate.CaregiverName caregiverName False
                                            , viewTextInput language Translate.CaregiverNationalId caregiverNationalId False
                                            ]

                                        -- We should never get here, as at second step
                                        -- registrating participant is already determined.
                                        Nothing ->
                                            []
                                   )

                        geoLocationDictToOptions dict =
                            dict
                                |> EveryDict.toList
                                |> List.map
                                    (\( id, geoLocation ) ->
                                        ( toString <| fromEntityId id, geoLocation.name )
                                    )

                        filterGeoLocationDictByParent parentId dict =
                            dict
                                |> EveryDict.filter
                                    (\_ geoLocation ->
                                        (Just <| toEntityId parentId) == geoLocation.parent
                                    )

                        geoLocationInputClass isDisabled =
                            "select-input"
                                ++ (if isDisabled then
                                        " disabled"

                                    else
                                        ""
                                   )

                        viewProvince =
                            let
                                options =
                                    emptyOption
                                        :: geoLocationDictToOptions geoInfo.provinces

                                disabled =
                                    isFormFieldSet district
                            in
                            viewSelectInput language
                                Translate.Province
                                options
                                province
                                "ten"
                                (geoLocationInputClass disabled)
                                True

                        viewDistrict =
                            let
                                options =
                                    emptyOption
                                        :: (case getFormFieldValue province of
                                                0 ->
                                                    []

                                                provinceId ->
                                                    geoInfo.districts
                                                        |> filterGeoLocationDictByParent provinceId
                                                        |> geoLocationDictToOptions
                                           )

                                disabled =
                                    isFormFieldSet sector
                            in
                            viewSelectInput language
                                Translate.District
                                options
                                district
                                "ten"
                                (geoLocationInputClass disabled)
                                True

                        viewSector =
                            let
                                options =
                                    emptyOption
                                        :: (case getFormFieldValue district of
                                                0 ->
                                                    []

                                                districtId ->
                                                    geoInfo.sectors
                                                        |> filterGeoLocationDictByParent districtId
                                                        |> geoLocationDictToOptions
                                           )

                                disabled =
                                    isFormFieldSet cell
                            in
                            viewSelectInput language
                                Translate.Sector
                                options
                                sector
                                "ten"
                                (geoLocationInputClass disabled)
                                True

                        viewCell =
                            let
                                options =
                                    emptyOption
                                        :: (case getFormFieldValue sector of
                                                0 ->
                                                    []

                                                sectorId ->
                                                    geoInfo.cells
                                                        |> filterGeoLocationDictByParent sectorId
                                                        |> geoLocationDictToOptions
                                           )

                                disabled =
                                    isFormFieldSet village
                            in
                            viewSelectInput language
                                Translate.Cell
                                options
                                cell
                                "ten"
                                (geoLocationInputClass disabled)
                                True

                        viewVillage =
                            let
                                options =
                                    emptyOption
                                        :: (case getFormFieldValue cell of
                                                0 ->
                                                    []

                                                cellId ->
                                                    geoInfo.villages
                                                        |> filterGeoLocationDictByParent cellId
                                                        |> geoLocationDictToOptions
                                           )
                            in
                            viewSelectInput language
                                Translate.Village
                                options
                                village
                                "ten"
                                (geoLocationInputClass False)
                                True

                        viewTelephoneNumber =
                            viewTextInput language Translate.TelephoneNumber telephoneNumber False
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.FamilyInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form family-info" ]
                            familyInfoInputs
                    , h3 [ class "ui header" ]
                        [ text <| translate language Translate.AddressInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form address-info" ]
                            [ viewProvince
                            , viewDistrict
                            , viewSector
                            , viewCell
                            , viewVillage
                            ]
                    , h3 [ class "ui header" ]
                        [ text <| translate language Translate.ContactInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form address-info" ]
                            [ viewTelephoneNumber ]
                    ]

                Third ->
                    let
                        viewHealthCenterName =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column" ]
                                    [ text <| translate language Translate.HealthCenterName ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput healthCenterName [] ]
                                ]
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.RegistratingHealthCenter ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form registrating-health-center" ]
                            [ viewHealthCenterName ]
                    ]

        rightButton =
            let
                maybeRegistratingParticipant =
                    getRegistratingParticipant currentDate
                        (getFormFieldValue dayOfBirth)
                        (getFormFieldValue monthOfBirth)
                        (getFormFieldValue yearOfBirth)
                        maybeRelationParticipant

                ( label, action, disabled ) =
                    case step of
                        First ->
                            let
                                validatedFields =
                                    [ nationalIdNumber ]

                                requiredFields =
                                    [ firstName, secondName, dayOfBirth, monthOfBirth, yearOfBirth ]
                                        ++ (case maybeRegistratingParticipant of
                                                Just (MotherParticipant _) ->
                                                    [ levelOfEducation
                                                    , hivStatus
                                                    ]

                                                Just (ChildParticipant _) ->
                                                    [ modeOfDelivery ]

                                                Nothing ->
                                                    []
                                           )
                            in
                            ( Translate.Next
                            , SetRegistrationPhase (ParticipantRegistration Second)
                            , not (List.all isFormFieldSet requiredFields && List.all isFormFieldValid validatedFields)
                            )

                        Second ->
                            let
                                ( requiredFields, validatedFields ) =
                                    let
                                        requiredCommon =
                                            [ familyUbudehe, district, sector, cell, village ]
                                    in
                                    case maybeRegistratingParticipant of
                                        Just (MotherParticipant _) ->
                                            ( requiredCommon ++ [ numberOfChildren ]
                                            , []
                                            )

                                        Just (ChildParticipant _) ->
                                            ( requiredCommon ++ [ motherName, fatherName ]
                                            , [ motherNationalId, fatherNationalId, caregiverNationalId ]
                                            )

                                        Nothing ->
                                            ( requiredCommon, [] )
                            in
                            ( Translate.Next
                            , SetRegistrationPhase (ParticipantRegistration Third)
                            , not (List.all isFormFieldSet requiredFields && List.all isFormFieldValid validatedFields)
                            )

                        Third ->
                            ( Translate.Submit
                            , SetDialogState <| Just ConfirmSubmision
                            , False
                            )
            in
            button
                [ classList
                    [ ( "ui primary button", True )
                    , ( "disabled", disabled )
                    ]
                , onClick action
                ]
                [ text <| translate language label ++ " >" ]
    in
    div [ class "wrap-list registration-page form" ]
        [ div [ class "ui form registration" ]
            formContent
        , div [ class "actions" ]
            [ viewBackButton language maybePreviousPhase, rightButton ]
        ]


viewSearchForm :
    Language
    -> NominalDate
    -> ModelIndexedDb
    -> Maybe String
    -> Maybe String
    -> Maybe ParticipantId
    -> Html Msg
viewSearchForm language currentDate db searchString submittedSearch maybeRelationParticipant =
    let
        ( disableSubmitButton, searchValue ) =
            case searchString of
                Just string ->
                    ( String.length string < 3, string )

                Nothing ->
                    ( True, "" )

        searchForm =
            Html.form
                [ onSubmit <| SearchForParticipant searchValue
                , action "javascript:void(0);"
                ]
                [ div
                    [ class "ui search form" ]
                    [ div []
                        [ input
                            [ placeholder <| translate language Translate.PlaceholderEnterParticipantName
                            , type_ "text"
                            , onInput <| SetRegistrationPhase << ParticipantSearch << Just
                            , value searchValue
                            , autofocus True
                            ]
                            []

                        -- , i [ class "icon icon-username" ] []
                        ]
                    ]
                , button
                    [ class "ui fluid primary button"
                    , disabled disableSubmitButton
                    , type_ "submit"
                    ]
                    [ text <| translate language Translate.Search ]
                ]

        ( searchResultsSummary, searchResultsParticipants ) =
            submittedSearch
                |> unwrap
                    ( [], [] )
                    (\searchValue ->
                        let
                            -- When relation participant is set, if it's a child, search search results
                            -- should display only mothers.
                            -- If it's a mother, search results should display only children that
                            -- are not attached to mother.
                            ( relationParticipantConditionMother, relationParticipantConditionChild ) =
                                maybeRelationParticipant
                                    |> unwrap
                                        ( \mother -> True, \child -> True )
                                        (\relationParticipant ->
                                            case relationParticipant of
                                                ParticipantMother _ ->
                                                    ( \mother -> False, \child -> isNothing child.motherId )

                                                ParticipantChild _ ->
                                                    ( \mother -> True, \child -> False )
                                        )

                            relationDependentAction =
                                if isJust maybeRelationParticipant then
                                    Just Link

                                else
                                    Just Forward

                            searchResults =
                                Dict.get (String.trim searchValue) db.nameSearches
                                    |> Maybe.withDefault NotAsked

                            mothersData =
                                searchResults
                                    |> RemoteData.map
                                        (\results ->
                                            results.mothers
                                                |> EveryDictList.toList
                                                |> List.filter
                                                    (\( _, mother ) ->
                                                        relationParticipantConditionMother mother
                                                    )
                                        )

                            childrenData =
                                searchResults
                                    |> RemoteData.map
                                        (\results ->
                                            results.children
                                                |> EveryDictList.toList
                                                |> List.filter
                                                    (\( _, child ) ->
                                                        relationParticipantConditionChild child
                                                    )
                                        )

                            totalData =
                                RemoteData.map2
                                    (\mothers children -> List.length mothers + List.length children)
                                    mothersData
                                    childrenData

                            summary =
                                -- We just use the `mothers` here to get a
                                viewWebData language
                                    (\total -> text <| translate language <| Translate.ReportResultsOfSearch total)
                                    identity
                                    totalData
                        in
                        ( [ summary ]
                        , (mothersData
                            |> RemoteData.withDefault []
                            |> List.map (\( uuid, mother ) -> viewParticipant language motherParticipant uuid mother relationDependentAction)
                          )
                            ++ (childrenData
                                    |> RemoteData.withDefault []
                                    |> List.map (\( uuid, child ) -> viewParticipant language childParticipant uuid child relationDependentAction)
                               )
                        )
                    )
    in
    div [ class "wrap-list registration-page search" ]
        [ h3 [ class "ui header" ]
            [ text <| translate language Translate.ParticipantInformation ++ ": " ]
        , span [ class "search-helper" ] [ text <| translate language Translate.SearchHelper ]
        , h3 [ class "ui header" ]
            [ text <| translate language Translate.ParticipantDirectory ++ ": " ]
        , searchForm
        , div [ class "results-summary" ]
            searchResultsSummary
        , div [ class "ui unstackable items participants-list" ]
            searchResultsParticipants
        , div [ class "register-helper" ]
            [ text <| translate language Translate.RegisterHelper ]
        , div [ class "actions" ]
            [ button
                [ class "ui primary button"
                , onClick <| SetRegistrationPhase (ParticipantRegistration First)
                ]
                [ text <| translate language Translate.RegisterNewParticipant ]
            ]
        ]


viewParticipantDetailsForm :
    Language
    -> NominalDate
    -> ParticipantId
    -> ModelIndexedDb
    -> Maybe RegistrationPhase
    -> Html Msg
viewParticipantDetailsForm language currentDate participantId db maybePreviousPhase =
    let
        ( topLabel, bottomLabel ) =
            case participantId of
                ParticipantMother motherId ->
                    ( Translate.MotherDemographicInformation
                    , Translate.Children
                    )

                ParticipantChild childId ->
                    ( Translate.ChildDemographicInformation
                    , Translate.Mother
                    )

        ( familyMembers, participantHtml ) =
            let
                addParticipantModal participantClass label =
                    div [ class "ui grid" ]
                        [ div [ class "four wide column" ]
                            [ span [ class <| "icon-participant add " ++ participantClass ] [] ]
                        , div [ class "eight wide column add-participant-label" ]
                            [ text <| translate language label ]
                        , div [ class "three wide column" ]
                            [ div [ class "add-participant-icon-wrapper" ]
                                [ span
                                    [ class "add-participant-icon"
                                    , onClick <| SetRelationParticipant <| Just participantId
                                    ]
                                    []
                                ]
                            ]
                        ]
            in
            case participantId of
                ParticipantChild childId ->
                    let
                        participantData =
                            childParticipant.getValue childId db

                        motherData =
                            RemoteData.andThen
                                (\participant ->
                                    case participant.motherId of
                                        Nothing ->
                                            -- We have the data, and there is no mother
                                            Success Nothing

                                        Just motherId ->
                                            EveryDict.get motherId db.mothers
                                                |> Maybe.withDefault NotAsked
                                                |> RemoteData.map (\mother -> Just ( motherId, mother ))
                                )
                                participantData

                        viewMotherData data =
                            case data of
                                Just ( motherId, mother ) ->
                                    div
                                        [ class "ui unstackable items participants-list" ]
                                        [ viewParticipant language motherParticipant motherId mother Nothing ]

                                Nothing ->
                                    div
                                        [ class "ui unstackable items participants-list" ]
                                        [ addParticipantModal "mother" Translate.AddMother ]

                        viewParticipantData participant =
                            viewParticipant language childParticipant childId participant Nothing
                    in
                    ( viewWebData language viewMotherData identity motherData
                    , viewWebData language viewParticipantData identity participantData
                    )

                ParticipantMother motherId ->
                    let
                        participantData =
                            motherParticipant.getValue motherId db

                        addChildModal =
                            addParticipantModal "child" Translate.AddChild

                        familyMembersListData =
                            EveryDict.get motherId db.childrenOfMother
                                |> Maybe.withDefault NotAsked
                                |> RemoteData.map EveryDict.toList

                        viewFamilyMembersList familyMembersList =
                            familyMembersList
                                |> List.map (\( childId, child ) -> viewParticipant language childParticipant childId child Nothing)
                                |> List.append [ addChildModal ]
                                |> List.reverse
                                |> div [ class "ui unstackable items participants-list" ]

                        viewParticipantData participant =
                            viewParticipant language motherParticipant motherId participant Nothing
                    in
                    ( viewWebData language viewFamilyMembersList identity familyMembersListData
                    , viewWebData language viewParticipantData identity participantData
                    )
    in
    div [ class "wrap-list registration-page view" ]
        [ h3
            [ class "ui header" ]
            [ text <| translate language topLabel ++ ": " ]
        , div
            [ class "ui unstackable items" ]
            [ participantHtml ]
        , div [ class "separator-line" ] []
        , h3
            [ class "ui header" ]
            [ text <| translate language bottomLabel ++ ": " ]
        , familyMembers
        , div
            [ class "actions" ]
            [ viewBackButton language maybePreviousPhase ]
        ]


viewParticipant : Language -> Participant id value activity msg -> id -> value -> Maybe ParticipantAction -> Html Msg
viewParticipant language config id participant maybeActionType =
    let
        participantId =
            config.toParticipantId id

        ( typeForThumbnail, healthCenter ) =
            case participantId of
                ParticipantMother _ ->
                    ( "mother", "TODO" )

                ParticipantChild _ ->
                    ( "child", "TODO" )

        content =
            div [ class "content" ] <|
                div [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text <| config.getName participant ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ config.getBirthDate participant
                                |> Maybe.map (renderDate language >> text)
                                |> showMaybe
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ config.getVillage participant |> Maybe.withDefault "" |> text ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.HealthCenter ++ ": " ]
                        , span [] [ text healthCenter ]
                        ]
                    ]
                    :: (case maybeActionType of
                            Just actionType ->
                                let
                                    ( action, actionClass ) =
                                        case actionType of
                                            Forward ->
                                                ( SetRegistrationPhase <| ParticipantView participantId, "forward" )

                                            Link ->
                                                ( MakeRelation participantId, "link" )
                                in
                                [ div [ class "action" ]
                                    [ div [ class "action-icon-wrapper" ]
                                        [ span
                                            [ class <| "action-icon " ++ actionClass
                                            , onClick action
                                            ]
                                            []
                                        ]
                                    ]
                                ]

                            Nothing ->
                                []
                       )
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage typeForThumbnail (config.getAvatarUrl participant) (config.getName participant) 120 120 ]
        , content
        ]


viewDialog : Language -> Maybe DialogState -> ModelIndexedDb -> Maybe (Html Msg)
viewDialog language dialogState db =
    dialogState
        |> Maybe.andThen
            (\state ->
                case state of
                    ConfirmSubmision ->
                        Just <| confirmSubmisionDialog language

                    Registering participant ->
                        Just <| successfulRegistrationDialog language participant db

                    SuccessfulRelation participantId ->
                        Just <| successfulRelationDialog language participantId
            )


confirmSubmisionDialog : Language -> Html Msg
confirmSubmisionDialog language =
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.ConfirmationRequired ]
        , div
            [ class "content" ]
            [ text <| translate language Translate.ConfirmRegisterParticipant ]
        , div
            [ class "actions" ]
            [ div
                [ class "two ui buttons" ]
                [ button
                    [ class "ui  fluid button"
                    , onClick <| SetDialogState Nothing
                    ]
                    [ text <| translate language Translate.No ]
                , button
                    [ class "ui  primary fluid button"
                    , onClick Submit
                    ]
                    [ text <| translate language Translate.Yes ]
                ]
            ]
        ]


successfulRegistrationDialog : Language -> ParticipantType -> ModelIndexedDb -> Html Msg
successfulRegistrationDialog language participantType db =
    let
        -- We want to figure out whether we've added a child a or mother, what
        -- the status of the request is, and whether the added child already
        -- has a mother or not. If some further relation should be suggested,
        -- we return a `Just ParticipantId` to reflect what we just added. If
        -- no further relation should be suggested, we return `Nothing`.  And,
        -- it's all wrapped in a `RemoteData`.
        addedParticipantIdData =
            case participantType of
                ChildParticipant _ ->
                    db.postChild
                        |> RemoteData.andThen
                            (\childId ->
                                EveryDict.get childId db.children
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map
                                        (\child ->
                                            case child.motherId of
                                                Just _ ->
                                                    Nothing

                                                Nothing ->
                                                    Just (ParticipantChild childId)
                                        )
                            )

                MotherParticipant _ ->
                    RemoteData.map (Just << ParticipantMother) db.postMother

        message participantId =
            case participantId of
                Just (ParticipantMother _) ->
                    text <| translate language Translate.RegistrationSuccessfulSuggestAddingChild

                Just (ParticipantChild _) ->
                    text <| translate language Translate.RegistrationSuccessfulSuggestAddingMother

                Nothing ->
                    text <| translate language Translate.RegistrationSuccessfulParticipantAdded

        buttons participantId =
            if isJust participantId then
                div [ class "ui buttons two" ]
                    [ button
                        [ class "ui fluid button"
                        , onClick Reset
                        ]
                        [ text <| translate language Translate.No ]
                    , button
                        [ class "ui primary fluid button"
                        , onClick <| SetRelationParticipant participantId
                        ]
                        [ text <| translate language Translate.Yes ]
                    ]

            else
                div
                    [ class "ui primary fluid button no-side-margins"
                    , onClick Reset
                    ]
                    [ text <| translate language Translate.OK ]
    in
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.RegistrationSuccessful ]
        , div
            [ class "content" ]
            [ viewWebData language message identity addedParticipantIdData ]
        , div
            [ class "actions" ]
            [ viewWebData language buttons identity addedParticipantIdData ]
        ]


successfulRelationDialog : Language -> ParticipantId -> Html Msg
successfulRelationDialog language relationParticipant =
    let
        message =
            case relationParticipant of
                ParticipantChild _ ->
                    Translate.RelationSuccessfulChildWithMother

                ParticipantMother _ ->
                    Translate.RelationSuccessfulMotherWithChild

        button =
            div
                [ class "ui primary fluid button no-side-margins"
                , onClick Reset
                ]
                [ text <| translate language Translate.OK ]
    in
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.RelationSuccessful ]
        , div
            [ class "content" ]
            [ text <| translate language message ]
        , div
            [ class "actions" ]
            [ button ]
        ]


viewTextInput : Language -> TranslationId -> Form.FieldState e String -> Bool -> Html Form.Msg
viewTextInput language labelId field isRequired =
    let
        formatValidationError =
            not isRequired && field.error == Just Form.Error.InvalidFormat
    in
    div [ class "ui grid" ]
        [ div
            [ classList
                [ ( "six wide column", True )
                , ( "required", isRequired )
                ]
            ]
            [ text <| translate language labelId ++ ":" ]
        , div [ class "ten wide column" ]
            [ Form.Input.textInput field [ classList [ ( "error", formatValidationError ) ] ] ]
        ]


viewSelectInput :
    Language
    -> TranslationId
    -> List ( String, String )
    -> Form.FieldState e String
    -> String
    -> String
    -> Bool
    -> Html Form.Msg
viewSelectInput language labelId options field width inputClass isRequired =
    div [ class "ui grid" ]
        [ div
            [ classList
                [ ( "six wide column", True )
                , ( "required", isRequired )
                ]
            ]
            [ text <| translate language labelId ++ ":" ]
        , div [ class <| width ++ " wide column" ]
            [ Form.Input.selectInput options field [ class inputClass ] ]
        ]


viewPhoto : Language -> Maybe PhotoValue -> Html Msg
viewPhoto language photo =
    div
        [ class "ui grid photo" ]
        [ Maybe.map viewPhotoThumb photo
            |> showMaybe
            |> List.singleton
            |> div [ class "eight wide column" ]
        , div
            [ id "dropzone"
            , class "eight wide column dropzone"
            , on "dropzonecomplete" (Json.Decode.map DropZoneComplete decodeDropZoneFile)
            ]
            [ div
                [ class "dz-message"
                , attribute "data-dz-message" ""
                ]
                [ span
                    []
                    [ text <| translate language Translate.DropzoneDefaultMessage ]
                ]
            ]

        -- This runs the function from our `app.js` at the precise moment this gets
        -- written to the DOM. Indeed very convenient.
        , script "bindDropZone()"
        ]


viewPhotoThumb : PhotoValue -> Html any
viewPhotoThumb photo =
    div []
        [ img
            [ src photo.url
            , class "ui small image"
            ]
            []
        ]


viewBackButton : Language -> Maybe RegistrationPhase -> Html Msg
viewBackButton language maybePreviousPhase =
    let
        action =
            if isJust maybePreviousPhase then
                StepBack

            else
                SetActivePage PinCodePage
    in
    button
        [ class "ui primary button"
        , onClick action
        ]
        [ text <| "< " ++ translate language Translate.Back ]
