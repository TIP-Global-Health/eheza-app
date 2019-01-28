module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Child.Model exposing (ModeOfDelivery(..), modeOfDeliveryToValue)
import Backend.Measurement.Model exposing (PhotoValue)
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..), toStringEducationLevel)
import Backend.Patient.Model exposing (Gender(..), Ubudehe(..))
import EveryDict
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
import Pages.PatientRegistration.Model
    exposing
        ( DialogState(..)
        , Model
        , Msg(..)
        , ParticipantsData
        , PatientType(..)
        , RegistrationForm
        , RegistrationPhase(..)
        , RegistrationStep(..)
        )
import Pages.PatientRegistration.Utils exposing (getCommonDetails, getFormFieldValue, getRegistratingParticipant)
import Participant.Model exposing (ParticipantType(..))
import Time.Date
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import Utils.Form exposing (isFormFieldSet, isFormFieldValid)
import Utils.Html exposing (script, thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderDate)


view : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
view language currentDate user backend cache model =
    div [ class "wrap wrap-alt-2" ]
        [ viewHeader language currentDate user backend cache model
        , div
            [ class "ui full segment blue" ]
            [ viewBody language currentDate user backend cache model
            ]
        , viewModal <| viewDialog language model.dialogState
        ]


viewHeader : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
viewHeader language currentDate user backend cache model =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.RegisterNewPatient ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage LoginPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewBody : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
viewBody language currentDate user backend cache model =
    let
        previousPhase =
            List.head model.previousPhases

        body =
            case model.registrationPhase of
                ParticipantSearch searchString ->
                    viewSearchForm language currentDate user backend cache model.participantsData searchString model.submittedSearch model.relationPatient

                ParticipantRegistration step ->
                    viewRegistrationForm language currentDate user backend cache step model.registrationForm model.photo model.relationPatient previousPhase

                ParticipantView patientType ->
                    viewPatientDetailsForm language currentDate user backend cache patientType model.participantsData previousPhase
    in
    div [ class "content" ]
        [ body ]


viewRegistrationForm :
    Language
    -> NominalDate
    -> User
    -> ModelBackend
    -> ModelCached
    -> RegistrationStep
    -> Form () RegistrationForm
    -> Maybe PhotoValue
    -> Maybe PatientType
    -> Maybe RegistrationPhase
    -> Html Msg
viewRegistrationForm language currentDate user backend cache step registrationForm photo maybeRelationPatient maybePreviousPhase =
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
                maybeRelationPatient

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
                                    maybeRelationPatient
                                        |> unwrap
                                            ( 100, currentYear - 99 )
                                            (\relationPatient ->
                                                case relationPatient of
                                                    PatientChild _ _ ->
                                                        ( 88, currentYear - 99 )

                                                    PatientMother _ _ ->
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
                                                            emptyOption
                                                                :: [ ( toStringEducationLevel NoSchooling, translate language <| Translate.LevelOfEducation NoSchooling )
                                                                   , ( toStringEducationLevel PrimarySchool, translate language <| Translate.LevelOfEducation PrimarySchool )
                                                                   , ( toStringEducationLevel VocationalTrainingSchool, translate language <| Translate.LevelOfEducation VocationalTrainingSchool )
                                                                   , ( toStringEducationLevel SecondarySchool, translate language <| Translate.LevelOfEducation SecondarySchool )
                                                                   , ( toStringEducationLevel DiplomaProgram, translate language <| Translate.LevelOfEducation DiplomaProgram )
                                                                   , ( toStringEducationLevel HigherEducation, translate language <| Translate.LevelOfEducation HigherEducation )
                                                                   , ( toStringEducationLevel AdvancedDiploma, translate language <| Translate.LevelOfEducation AdvancedDiploma )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.LevelOfEducationLabel options levelOfEducation "ten" "select-input" True

                                                viewProfession =
                                                    viewTextInput language Translate.Profession profession False

                                                viewMaritalStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( String.toLower <| toString Divorced, translate language <| Translate.MaritalStatus Divorced )
                                                                   , ( String.toLower <| toString Maried, translate language <| Translate.MaritalStatus Maried )
                                                                   , ( String.toLower <| toString Single, translate language <| Translate.MaritalStatus Single )
                                                                   , ( String.toLower <| toString Widowed, translate language <| Translate.MaritalStatus Widowed )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.MaritalStatusLabel options maritalStatus "ten" "select-input" True

                                                viewHIVStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( String.toLower <| toString NA, translate language <| Translate.HIVStatus NA )
                                                                   , ( String.toLower <| toString Negative, translate language <| Translate.HIVStatus Negative )
                                                                   , ( String.toLower <| toString Positive, translate language <| Translate.HIVStatus Positive )
                                                                   ]
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
                                                            emptyOption
                                                                :: [ ( modeOfDeliveryToValue SpontaneousVaginalDeliveryWithEpisiotomy, translate language <| Translate.ModeOfDelivery SpontaneousVaginalDeliveryWithEpisiotomy )
                                                                   , ( modeOfDeliveryToValue SpontaneousVaginalDeliveryWithoutEpisiotomy, translate language <| Translate.ModeOfDelivery SpontaneousVaginalDeliveryWithoutEpisiotomy )
                                                                   , ( modeOfDeliveryToValue VaginalDeliveryWithVacuumExtraction, translate language <| Translate.ModeOfDelivery VaginalDeliveryWithVacuumExtraction )
                                                                   , ( modeOfDeliveryToValue CesareanDelivery, translate language <| Translate.ModeOfDelivery CesareanDelivery )
                                                                   ]
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
                        [ text <| translate language Translate.PatientDemographicInformation ++ ":" ]
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
                                    -- TODO: verify which values are required and translate.
                                    emptyOption
                                        :: [ ( toString Ubudehe1, toString Ubudehe1 )
                                           , ( toString Ubudehe2, toString Ubudehe2 )
                                           , ( toString Ubudehe3, toString Ubudehe3 )
                                           , ( toString Ubudehe4, toString Ubudehe4 )
                                           ]
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

                        viewDistrict =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.District ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput district [] ]
                                ]

                        viewSector =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.Sector ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput sector [] ]
                                ]

                        viewCell =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.Cell ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput cell [] ]
                                ]

                        viewVillage =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.Village ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput village [] ]
                                ]

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
                            [ viewDistrict
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
                        maybeRelationPatient

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
    -> User
    -> ModelBackend
    -> ModelCached
    -> ParticipantsData
    -> Maybe String
    -> Maybe String
    -> Maybe PatientType
    -> Html Msg
viewSearchForm language currentDate user backend cache participantsData searchString submittedSearch maybeRelationPatient =
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
                            [ placeholder <| translate language Translate.PlaceholderEnterPatientName
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

        ( searchResultsSummary, searchResultsPatients ) =
            submittedSearch
                |> unwrap
                    ( [], [] )
                    (\searchValue ->
                        let
                            -- When relation patient is set, if it's a child, search search results
                            -- should display only mothers.
                            -- If it's a mother, search results should display only children that
                            -- are not attached to mother.
                            ( relationPatientConditionMother, relationPatientConditionChild ) =
                                maybeRelationPatient
                                    |> unwrap
                                        ( \mother -> True, \child -> True )
                                        (\relationPatient ->
                                            case relationPatient of
                                                PatientMother _ _ ->
                                                    ( \mother -> False, \child -> isNothing child.motherUuid )

                                                PatientChild _ _ ->
                                                    ( \mother -> True, \child -> False )
                                        )

                            mothers =
                                participantsData.mothersToRegister
                                    |> EveryDict.toList
                                    |> List.filter
                                        (\( _, mother ) ->
                                            String.contains searchValue mother.name && relationPatientConditionMother mother
                                        )

                            children =
                                participantsData.childrenToRegister
                                    |> EveryDict.toList
                                    |> List.filter
                                        (\( _, child ) ->
                                            String.contains searchValue child.name && relationPatientConditionChild child
                                        )

                            total =
                                List.length mothers + List.length children
                        in
                        ( [ text <| translate language <| Translate.ReportResultsOfSearch total ]
                        , (mothers
                            |> List.map (\( uuid, mother ) -> viewPatient language (PatientMother uuid mother) True)
                          )
                            ++ (children
                                    |> List.map (\( uuid, child ) -> viewPatient language (PatientChild uuid child) True)
                               )
                        )
                    )
    in
    div [ class "wrap-list registration-page search" ]
        [ h3 [ class "ui header" ]
            [ text <| translate language Translate.PatientInformation ++ ": " ]
        , span [ class "search-helper" ] [ text <| translate language Translate.SearchHelper ]
        , h3 [ class "ui header" ]
            [ text <| translate language Translate.ParticipantDirectory ++ ": " ]
        , searchForm
        , div [ class "results-summary" ]
            searchResultsSummary
        , div [ class "ui unstackable items patients-list" ]
            searchResultsPatients
        , div [ class "register-helper" ]
            [ text <| translate language Translate.RegisterHelper ]
        , div [ class "actions" ]
            [ button
                [ class "ui primary button"
                , onClick <| SetRegistrationPhase (ParticipantRegistration First)
                ]
                [ text <| translate language Translate.RegisterNewPatient ]
            ]
        ]


viewPatientDetailsForm :
    Language
    -> NominalDate
    -> User
    -> ModelBackend
    -> ModelCached
    -> PatientType
    -> ParticipantsData
    -> Maybe RegistrationPhase
    -> Html Msg
viewPatientDetailsForm language currentDate user backend cache viewedPatient participantsData maybePreviousPhase =
    let
        ( topLabel, bottomLabel, familyMembersList ) =
            case viewedPatient of
                PatientMother _ mother ->
                    ( Translate.MotherDemographicInformation
                    , Translate.Children
                    , participantsData.childrenToRegister
                        |> EveryDict.toList
                        |> List.filterMap
                            (\( uuid, child ) ->
                                if List.member uuid mother.childrenUuids then
                                    Just <| PatientChild uuid child

                                else
                                    Nothing
                            )
                    )

                PatientChild _ child ->
                    ( Translate.ChildDemographicInformation
                    , Translate.Mother
                    , child.motherUuid
                        |> unwrap
                            []
                            (\motherUuid ->
                                case EveryDict.get motherUuid participantsData.mothersToRegister of
                                    Just mother ->
                                        [ PatientMother motherUuid mother ]

                                    Nothing ->
                                        []
                            )
                    )

        familyMembers =
            let
                addPatientModal patientClass label =
                    div [ class "ui grid" ]
                        [ div [ class "four wide column" ]
                            [ span [ class <| "icon-participant add " ++ patientClass ] [] ]
                        , div [ class "eight wide column add-patient-label" ]
                            [ text <| translate language label ]
                        , div [ class "three wide column" ]
                            [ div [ class "add-patient-icon-wrapper" ]
                                [ span
                                    [ class "add-patient-icon"
                                    , onClick <| SetRelationPatient <| Just viewedPatient
                                    ]
                                    []
                                ]
                            ]
                        ]
            in
            case viewedPatient of
                PatientChild _ _ ->
                    case familyMembersList of
                        [ patientMother ] ->
                            [ viewPatient language patientMother False ]

                        _ ->
                            [ addPatientModal "mother" Translate.AddMother ]

                PatientMother _ _ ->
                    let
                        addChildModal =
                            addPatientModal "child" Translate.AddChild
                    in
                    if List.isEmpty familyMembersList then
                        [ addChildModal ]

                    else
                        familyMembersList
                            |> List.map (\childPatient -> viewPatient language childPatient False)
                            |> List.append [ addChildModal ]
                            |> List.reverse
    in
    div [ class "wrap-list registration-page view" ]
        [ h3 [ class "ui header" ]
            [ text <| translate language topLabel ++ ": " ]
        , div [ class "ui unstackable items" ]
            [ viewPatient language viewedPatient False ]
        , div [ class "separator-line" ] []
        , h3 [ class "ui header" ]
            [ text <| translate language bottomLabel ++ ": " ]
        , div [ class "ui unstackable items patients-list" ]
            familyMembers
        , div [ class "actions" ]
            [ viewBackButton language maybePreviousPhase ]
        ]


viewPatient : Language -> PatientType -> Bool -> Html Msg
viewPatient language patientType addAction =
    let
        ( typeForThumbnail, details, healthCenter ) =
            case patientType of
                PatientMother _ mother ->
                    ( "mother", getCommonDetails mother, "Unknown" )

                PatientChild _ child ->
                    ( "child", getCommonDetails child, "Unknown" )

        content =
            div [ class "content" ] <|
                div [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text details.name ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ renderDate language details.birthDate
                                |> text
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ details.village |> Maybe.withDefault "" |> text ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.HealthCenter ++ ": " ]
                        , span [] [ text healthCenter ]
                        ]
                    ]
                    :: (if addAction then
                            [ div [ class "action" ]
                                [ div [ class "action-icon-wrapper" ]
                                    [ span
                                        [ class "action-icon"
                                        , onClick <| SetRegistrationPhase <| ParticipantView patientType
                                        ]
                                        []
                                    ]
                                ]
                            ]

                        else
                            []
                       )
    in
    div
        [ class "item patient-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage typeForThumbnail details.avatarUrl details.name 120 120 ]
        , content
        ]


viewDialog : Language -> Maybe DialogState -> Maybe (Html Msg)
viewDialog language dialogState =
    dialogState
        |> Maybe.andThen
            (\state ->
                case state of
                    ConfirmSubmision ->
                        Just <| confirmSubmisionDialog language

                    SuccessfulSubmision maybePatientType ->
                        Just <| successfulSubmisionDialog language maybePatientType
            )


confirmSubmisionDialog : Language -> Html Msg
confirmSubmisionDialog language =
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.ConfirmationRequired ]
        , div
            [ class "content" ]
            [ text <| translate language Translate.ConfirmRegisterPatient ]
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


successfulSubmisionDialog : Language -> Maybe PatientType -> Html Msg
successfulSubmisionDialog language maybePatientType =
    let
        ( helper, resetButtonLabel ) =
            case maybePatientType of
                Just (PatientMother _ _) ->
                    ( Translate.RegistartionSuccessfulAddChild, Translate.No )

                Just (PatientChild _ _) ->
                    ( Translate.RegistartionSuccessfulAddMother, Translate.No )

                Nothing ->
                    ( Translate.RegistartionSuccessfulRelationCreated, Translate.OK )

        buttons =
            div
                [ classList
                    [ ( "one", isNothing maybePatientType )
                    , ( "two", isJust maybePatientType )
                    , ( "ui buttons", True )
                    ]
                ]
            <|
                button
                    [ class "ui fluid button"
                    , onClick Reset
                    ]
                    [ text <| translate language resetButtonLabel ]
                    :: (if isJust maybePatientType then
                            [ button
                                [ class "ui primary fluid button"
                                , onClick <| SetRelationPatient maybePatientType
                                ]
                                [ text <| translate language Translate.Yes ]
                            ]

                        else
                            []
                       )
    in
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.RegistartionSuccessful ]
        , div
            [ class "content" ]
            [ text <| translate language helper ]
        , div
            [ class "actions" ]
            [ buttons ]
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
                GoBack

            else
                SetActivePage LoginPage
    in
    button
        [ class "ui primary button"
        , onClick action
        ]
        [ text <| "< " ++ translate language Translate.Back ]
