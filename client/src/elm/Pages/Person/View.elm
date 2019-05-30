module Pages.Person.View exposing (view, viewCreateForm)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Encoder exposing (encodeEducationLevel, encodeMaritalStatus, encodeUbudehe)
import Backend.Person.Form exposing (ExpectedAge(..), PersonForm, expectedAgeFromForm, validatePerson)
import Backend.Person.Model exposing (Gender(..), Person, allEducationLevels, allMaritalStatuses, allUbudehes)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Date.Extra as Date exposing (Interval(Year))
import DateSelector.SelectorDropdown
import EveryDict
import EveryDictList
import Form exposing (Form)
import Form.Field
import Form.Input
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityId)
import Set
import Time.Iso8601
import Translate exposing (Language, TranslationId, translate)
import Utils.Form exposing (dateInput, getValueAsInt, isFormFieldSet, viewFormError)
import Utils.GeoLocation exposing (GeoInfo, geoInfo, getGeoLocation)
import Utils.Html exposing (script, thumbnailImage, viewLoading)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    let
        person =
            EveryDict.get id db.people
                |> Maybe.withDefault NotAsked

        headerName =
            person
                |> RemoteData.map .name
                |> RemoteData.withDefault (translate language Translate.Person ++ " " ++ fromEntityUuid id)
    in
    div
        [ class "page-person" ]
        [ viewHeader language headerName
        , div
            [ class "ui full segment blue" ]
            [ viewWebData language (viewParticipantDetailsForm language currentDate db id) identity person
            ]
        ]


viewHeader : Language -> String -> Html App.Model.Msg
viewHeader language name =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text name ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewRelationship : Language -> NominalDate -> ModelIndexedDb -> MyRelationship -> Html App.Model.Msg
viewRelationship language currentDate db relationship =
    let
        relatedTo =
            EveryDict.get relationship.relatedTo db.people
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "ui unstackable items participants-list" ]
        [ viewWebData language (viewParticipant language currentDate (Just relationship) relationship.relatedTo) identity relatedTo ]


viewParticipantDetailsForm : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html App.Model.Msg
viewParticipantDetailsForm language currentDate db id person =
    let
        viewFamilyMembers relationships =
            relationships
                |> EveryDictList.map (always (viewRelationship language currentDate db))
                |> EveryDictList.values
                |> div []

        familyMembers =
            EveryDict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> viewWebData language viewFamilyMembers identity

        isAdult =
            isPersonAnAdult currentDate person

        ( typeForAddFamilyMember, labelForAddFamilyMember ) =
            case isAdult of
                Just True ->
                    ( "child", Translate.AddChild )

                Just False ->
                    ( "mother", Translate.AddParentOrCaregiver )

                Nothing ->
                    ( "mother", Translate.AddFamilyMember )

        addFamilyMember =
            div [ class "ui unstackable items participants-list" ]
                [ div
                    [ class "item participant-view" ]
                    [ div
                        [ class "ui image" ]
                        [ span
                            [ class ("icon-participant add " ++ typeForAddFamilyMember)
                            , style
                                [ ( "height", "120px" )
                                , ( "width", "120px" )
                                ]
                            ]
                            []
                        ]
                    , div
                        [ class "content" ]
                        [ div
                            [ class "details" ]
                            [ h2
                                [ class "ui header add-participant-label" ]
                                [ text <| translate language labelForAddFamilyMember ]
                            ]
                        , div
                            [ class "action" ]
                            [ div
                                [ class "add-participant-icon-wrapper"
                                , onClick <| App.Model.SetActivePage <| UserPage <| PersonsPage Nothing (Just id)
                                ]
                                [ span [ class "add-participant-icon" ] [] ]
                            ]
                        ]
                    ]
                ]
    in
    div [ class "registration-page view" ]
        [ h3
            [ class "ui header" ]
            [ text <| translate language Translate.DemographicInformation ++ ": " ]
        , div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate Nothing id person ]
        , h3
            [ class "ui header" ]
            [ text <| translate language Translate.FamilyMembers ++ ": " ]
        , familyMembers
        , p [] []
        , addFamilyMember
        ]


viewParticipant : Language -> NominalDate -> Maybe MyRelationship -> PersonId -> Person -> Html App.Model.Msg
viewParticipant language currentDate myRelationship id person =
    let
        typeForThumbnail =
            case isPersonAnAdult currentDate person of
                Just True ->
                    "mother"

                Just False ->
                    "child"

                Nothing ->
                    "mother"

        relationshipLabel =
            myRelationship
                |> Maybe.map
                    (\relationship ->
                        span
                            [ class "relationship" ]
                            [ text " ("
                            , text <| translate language <| Translate.MyRelatedBy relationship.relatedBy
                            , text ")"
                            ]
                    )
                |> Maybe.withDefault emptyNode

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text person.name
                        , relationshipLabel
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ person.birthDate
                                |> Maybe.map (renderDate language >> text)
                                |> showMaybe
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ person.village |> Maybe.withDefault "" |> text ]
                        ]
                    ]
                ]
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage typeForThumbnail person.avatarUrl person.name 120 120 ]
        , content
        ]


viewPhotoThumb : String -> Html any
viewPhotoThumb url =
    div []
        [ img
            [ src url
            , class "ui small image"
            ]
            []
        ]


applyDefaultValues : Maybe Person -> NominalDate -> PersonForm -> PersonForm
applyDefaultValues maybeRelatedPerson currentDate form =
    let
        defaultProvinceId =
            maybeRelatedPerson
                |> Maybe.andThen .province
                |> Maybe.andThen (getGeoLocation Nothing)
                |> Maybe.map Tuple.first

        defaultDistrictId =
            maybeRelatedPerson
                |> Maybe.andThen .district
                |> Maybe.andThen (getGeoLocation defaultProvinceId)
                |> Maybe.map Tuple.first

        defaultSectorId =
            maybeRelatedPerson
                |> Maybe.andThen .sector
                |> Maybe.andThen (getGeoLocation defaultDistrictId)
                |> Maybe.map Tuple.first

        defaultCellId =
            maybeRelatedPerson
                |> Maybe.andThen .cell
                |> Maybe.andThen (getGeoLocation defaultSectorId)
                |> Maybe.map Tuple.first

        defaultVillageId =
            maybeRelatedPerson
                |> Maybe.andThen .village
                |> Maybe.andThen (getGeoLocation defaultCellId)
                |> Maybe.map Tuple.first

        applyDefaultLocation fieldName maybeDefault form =
            case maybeDefault of
                Just defaultId ->
                    Form.getFieldAsString fieldName form
                        |> .value
                        |> Maybe.map String.isEmpty
                        |> Maybe.withDefault True
                        |> (\useDefault ->
                                if useDefault then
                                    Form.update
                                        (validatePerson maybeRelatedPerson (Just currentDate))
                                        (Form.Input fieldName Form.Select (Form.Field.String (toString <| fromEntityId defaultId)))
                                        form

                                else
                                    form
                           )

                Nothing ->
                    form
    in
    form
        |> applyDefaultLocation Backend.Person.Form.province defaultProvinceId
        |> applyDefaultLocation Backend.Person.Form.district defaultDistrictId
        |> applyDefaultLocation Backend.Person.Form.sector defaultSectorId
        |> applyDefaultLocation Backend.Person.Form.cell defaultCellId
        |> applyDefaultLocation Backend.Person.Form.village defaultVillageId


viewCreateForm : Language -> NominalDate -> Maybe PersonId -> Model -> ModelIndexedDb -> Html Msg
viewCreateForm language currentDate relationId model db =
    let
        formBeforeDefaults =
            model.form

        personForm =
            applyDefaultValues maybeRelatedPerson currentDate formBeforeDefaults

        request =
            db.postPerson

        maybeRelatedPerson =
            relationId
                |> Maybe.andThen (\id -> EveryDict.get id db.people)
                |> Maybe.andThen RemoteData.toMaybe

        emptyOption =
            ( "", "" )

        header =
            div [ class "ui basic segment head" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Translate.People ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage PinCodePage
                    ]
                    [ span [ class "icon-back" ] []
                    , span [] []
                    ]
                ]

        errors =
            Form.getErrors personForm

        requestStatus =
            case request of
                Success _ ->
                    -- We only show the success message until you make changes.
                    if Set.isEmpty (Form.getChangedFields personForm) then
                        div
                            [ class "ui success message" ]
                            [ div [ class "header" ] [ text <| translate language Translate.Success ]
                            , div [] [ text <| translate language Translate.PersonHasBeenSaved ]
                            ]

                    else
                        emptyNode

                Failure err ->
                    div
                        [ class "ui warning message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.BackendError ]
                        , viewError language err
                        ]

                Loading ->
                    viewLoading

                NotAsked ->
                    emptyNode

        birthDateField =
            Form.getFieldAsString Backend.Person.Form.birthDate personForm

        expectedAge =
            maybeRelatedPerson
                |> Maybe.andThen
                    (\related ->
                        case isPersonAnAdult currentDate related of
                            Just True ->
                                Just ExpectChild

                            Just False ->
                                Just ExpectAdult

                            Nothing ->
                                Nothing
                    )
                -- If we don't have a related person, or don't know whether
                -- that person is an adult, then we check whether a birthdate
                -- has been entered into the form so far.
                |> Maybe.withDefault (expectedAgeFromForm currentDate personForm)

        birthDateEstimatedField =
            Form.getFieldAsBool Backend.Person.Form.birthDateEstimated personForm

        birthDateInput =
            let
                today =
                    toLocalDateTime currentDate 0 0 0 0

                selectedDate =
                    Form.getFieldAsString Backend.Person.Form.birthDate personForm
                        |> .value
                        |> Maybe.andThen (Time.Iso8601.toDate >> Result.toMaybe)
                        |> Maybe.map (\date -> toLocalDateTime date 12 0 0 0)
            in
            div [ class "ui grid" ]
                [ div
                    [ class "six wide column" ]
                    []
                , div
                    [ class "seven wide column required" ]
                    [ text <| translate language Translate.DateOfBirth ++ ":"
                    , br [] []
                    , DateSelector.SelectorDropdown.view
                        ToggleDateSelector
                        (DateSelected relationId)
                        model.isDateSelectorOpen
                        (Date.add Year -60 today)
                        today
                        selectedDate
                    ]
                , div
                    [ class "three wide column" ]
                    [ text <| translate language Translate.Estimated ++ ":"
                    , br [] []
                    , Form.Input.checkboxInput birthDateEstimatedField
                        [ classList
                            [ ( "error", isJust birthDateEstimatedField.liveError )
                            , ( "field", True )
                            ]
                        ]
                        |> Html.map (MsgForm relationId)
                    ]
                ]

        genderField =
            Form.getFieldAsString Backend.Person.Form.gender personForm

        genderInput =
            div [ class "ui grid" ]
                [ div
                    [ class "six wide column required" ]
                    [ text <| translate language Translate.GenderLabel ++ ":" ]
                , Form.Input.radioInput "male"
                    genderField
                    [ class "one wide column gender-input" ]
                , div
                    [ class "three wide column" ]
                    [ text <| translate language (Translate.Gender Male) ]
                , Form.Input.radioInput "female"
                    genderField
                    [ class "one wide column gender-input" ]
                , div
                    [ class "three wide column" ]
                    [ text <| translate language (Translate.Gender Female) ]
                ]

        educationLevelOptions =
            allEducationLevels
                |> List.map
                    (\level ->
                        ( toString (encodeEducationLevel level)
                        , translate language (Translate.LevelOfEducation level)
                        )
                    )
                |> (::) emptyOption

        maritalStatusOptions =
            allMaritalStatuses
                |> List.map
                    (\status ->
                        ( encodeMaritalStatus status
                        , translate language <| Translate.MaritalStatus status
                        )
                    )
                |> (::) emptyOption

        photoUrl =
            Form.getFieldAsString Backend.Person.Form.photo personForm
                |> .value

        viewPhoto =
            divKeyed
                [ class "ui grid photo" ]
                [ Maybe.map viewPhotoThumb photoUrl
                    |> Maybe.Extra.toList
                    |> div [ class "eight wide column" ]
                    |> keyed "thumb"
                , div
                    [ id "dropzone"
                    , class "eight wide column dropzone"
                    , on "dropzonecomplete" (Json.Decode.map (DropZoneComplete relationId) decodeDropZoneFile)
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
                    |> keyed "dropzone"

                -- This runs the function from our `app.js` at the precise moment this gets
                -- written to the DOM. Indeed very convenient.
                , script "bindDropZone()"
                    |> keyed "script"
                ]

        demographicFields =
            viewPhoto
                :: (List.map (Html.map (MsgForm relationId)) <|
                        [ viewTextInput language Translate.FirstName Backend.Person.Form.firstName True personForm
                        , viewTextInput language Translate.SecondName Backend.Person.Form.secondName True personForm
                        , viewTextInput language Translate.NationalIdNumber Backend.Person.Form.nationalIdNumber False personForm
                        ]
                   )
                ++ [ birthDateInput ]
                ++ (List.map (Html.map (MsgForm relationId)) <|
                        (genderInput
                            :: (if expectedAge /= ExpectChild then
                                    [ viewSelectInput language Translate.LevelOfEducationLabel educationLevelOptions Backend.Person.Form.educationLevel "ten" "select-input" True personForm
                                    , viewSelectInput language Translate.MaritalStatusLabel maritalStatusOptions Backend.Person.Form.maritalStatus "ten" "select-input" True personForm
                                    ]

                                else
                                    []
                               )
                        )
                   )

        ubudeheOptions =
            allUbudehes
                |> List.map
                    (\ubudehe ->
                        ( toString (encodeUbudehe ubudehe)
                        , toString (encodeUbudehe ubudehe)
                        )
                    )
                |> (::) emptyOption

        familyInformationFields =
            [ viewSelectInput language Translate.FamilyUbudehe ubudeheOptions Backend.Person.Form.ubudehe "ten" "select-input" True personForm
            ]

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

        province =
            Form.getFieldAsString Backend.Person.Form.province personForm

        district =
            Form.getFieldAsString Backend.Person.Form.district personForm

        sector =
            Form.getFieldAsString Backend.Person.Form.sector personForm

        cell =
            Form.getFieldAsString Backend.Person.Form.cell personForm

        village =
            Form.getFieldAsString Backend.Person.Form.village personForm

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
                Backend.Person.Form.province
                "ten"
                (geoLocationInputClass disabled)
                True
                personForm

        viewDistrict =
            let
                options =
                    emptyOption
                        :: (case getValueAsInt province of
                                Nothing ->
                                    []

                                Just provinceId ->
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
                Backend.Person.Form.district
                "ten"
                (geoLocationInputClass disabled)
                True
                personForm

        viewSector =
            let
                options =
                    emptyOption
                        :: (case getValueAsInt district of
                                Nothing ->
                                    []

                                Just districtId ->
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
                Backend.Person.Form.sector
                "ten"
                (geoLocationInputClass disabled)
                True
                personForm

        viewCell =
            let
                options =
                    emptyOption
                        :: (case getValueAsInt sector of
                                Nothing ->
                                    []

                                Just sectorId ->
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
                Backend.Person.Form.cell
                "ten"
                (geoLocationInputClass disabled)
                True
                personForm

        viewVillage =
            let
                options =
                    emptyOption
                        :: (case getValueAsInt cell of
                                Nothing ->
                                    []

                                Just cellId ->
                                    geoInfo.villages
                                        |> filterGeoLocationDictByParent cellId
                                        |> geoLocationDictToOptions
                           )
            in
            viewSelectInput language
                Translate.Village
                options
                Backend.Person.Form.village
                "ten"
                (geoLocationInputClass False)
                True
                personForm

        addressFields =
            [ viewProvince
            , viewDistrict
            , viewSector
            , viewCell
            , viewVillage
            ]

        contactInformationSection =
            if expectedAge /= ExpectChild then
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.ContactInformation ++ ":" ]
                , [ viewTextInput language Translate.TelephoneNumber Backend.Person.Form.phoneNumber False personForm ]
                    |> fieldset [ class "registration-form address-info" ]
                    |> Html.map (MsgForm relationId)
                ]

            else
                []

        healthCenterSection =
            let
                options =
                    emptyOption
                        :: (db.healthCenters
                                |> RemoteData.map
                                    (\dict ->
                                        dict
                                            |> EveryDictList.toList
                                            |> List.map
                                                (\( id, healthCenter ) ->
                                                    ( fromEntityUuid id
                                                    , healthCenter.name
                                                    )
                                                )
                                    )
                                |> RemoteData.withDefault []
                           )
            in
            [ h3
                [ class "ui header" ]
                [ text <| translate language Translate.RegistratingHealthCenter ++ ":" ]
            , [ viewSelectInput language Translate.HealthCenter options Backend.Person.Form.healthCenter "ten" "select-input" True personForm ]
                |> fieldset [ class "registration-form health-center" ]
                |> Html.map (MsgForm relationId)
            ]

        submitButton =
            button
                [ classList
                    [ ( "ui button primary fluid", True )
                    , ( "loading", RemoteData.isLoading request )
                    , ( "disabled", RemoteData.isLoading request )
                    ]
                , type_ "submit"
                , onClick Form.Submit
                ]
                [ text <| translate language Translate.Save ]

        formContent =
            [ h3
                [ class "ui header" ]
                [ text <| translate language Translate.ParticipantDemographicInformation ++ ":" ]
            , demographicFields
                |> fieldset [ class "registration-form" ]
            , h3
                [ class "ui header" ]
                [ text <| translate language Translate.FamilyInformation ++ ":" ]
            , familyInformationFields
                |> fieldset [ class "registration-form family-info" ]
                |> Html.map (MsgForm relationId)
            , h3
                [ class "ui header" ]
                [ text <| translate language Translate.AddressInformation ++ ":" ]
            , addressFields
                |> fieldset [ class "registration-form address-info" ]
                |> Html.map (MsgForm relationId)
            ]
                ++ contactInformationSection
                ++ healthCenterSection
                ++ [ p [] []
                   , submitButton
                        |> Html.map (MsgForm relationId)

                   -- Note that these are hidden by deafult by semantic-ui ... the
                   -- class of the "form" controls whether they are shown.
                   , requestStatus
                   , div
                        [ class "ui error message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.ValidationErrors ]
                        , List.map (viewFormError language) errors
                            |> ul []
                        ]
                   ]
    in
    div
        [ class "page-person-create" ]
        [ header
        , div
            [ class "ui full segment blue" ]
            [ div
                [ class "content" ]
                [ div
                    [ class "registration-page form" ]
                    [ div
                        [ classList
                            [ ( "ui form registration", True )
                            , ( "error", Form.isSubmitted personForm && not (List.isEmpty errors) )
                            , ( "success", RemoteData.isSuccess request )
                            , ( "warning", RemoteData.isFailure request )
                            ]
                        ]
                        formContent
                    ]
                ]
            ]
        ]


viewTextInput : Language -> TranslationId -> String -> Bool -> Form e a -> Html Form.Msg
viewTextInput language labelId fieldName isRequired form =
    let
        field =
            Form.getFieldAsString fieldName form
    in
    div [ class "ui grid" ]
        [ div
            [ classList
                [ ( "six wide column", True )
                , ( "required", isRequired )
                ]
            ]
            [ text <| translate language labelId ++ ":" ]
        , div
            [ class "ten wide column" ]
            [ Form.Input.textInput field
                [ classList
                    [ ( "error", isJust field.liveError )
                    , ( "field", True )
                    ]
                , field.value
                    |> Maybe.withDefault ""
                    |> value
                ]
            ]
        ]


viewSelectInput :
    Language
    -> TranslationId
    -> List ( String, String )
    -> String
    -> String
    -> String
    -> Bool
    -> Form e a
    -> Html Form.Msg
viewSelectInput language labelId options fieldName width inputClass isRequired form =
    let
        field =
            Form.getFieldAsString fieldName form
    in
    div [ class "ui grid" ]
        [ div
            [ classList
                [ ( "six wide column", True )
                , ( "required", isRequired )
                ]
            ]
            [ text <| translate language labelId ++ ":" ]
        , div
            [ class <| width ++ " wide column" ]
            [ Form.Input.selectInput options
                field
                [ classList
                    [ ( inputClass, True )
                    , ( "field", True )
                    , ( "error", isJust field.liveError )
                    ]
                ]
            ]
        ]
