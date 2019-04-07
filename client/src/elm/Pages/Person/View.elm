module Pages.Person.View exposing (view, viewCreateForm)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Encoder exposing (encodeEducationLevel, encodeMaritalStatus, encodeUbudehe)
import Backend.Person.Form exposing (PersonForm)
import Backend.Person.Model exposing (Gender(..), Person, allEducationLevels, allMaritalStatuses, allUbudehes)
import Backend.Person.Utils exposing (ageInYears)
import Backend.Relationship.Model exposing (Relationship)
import Backend.Relationship.Utils exposing (getRelatedTo, toMyRelationship)
import EveryDict
import EveryDictList
import Form exposing (Form)
import Form.Input
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Set
import Translate exposing (Language, TranslationId, translate)
import Utils.Form exposing (dateInput, getValueAsInt, isFormFieldSet, viewFormError)
import Utils.GeoLocation exposing (GeoInfo, geoInfo)
import Utils.Html exposing (script, thumbnailImage, viewLoading)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    div
        [ class "wrap wrap-alt-2" ]
        [ viewHeader language
        , div
            [ class "ui full segment blue" ]
            [ EveryDict.get id db.people
                |> Maybe.withDefault NotAsked
                |> viewWebData language (viewParticipantDetailsForm language currentDate db id) identity
            ]
        ]


viewHeader : Language -> Html App.Model.Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.People ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewRelationship : Language -> NominalDate -> ModelIndexedDb -> PersonId -> RelationshipId -> Relationship -> Html App.Model.Msg
viewRelationship language currentDate db personId relationshipId relationship =
    let
        viewMyRelationship myRelationship =
            let
                relatedToId =
                    getRelatedTo myRelationship

                relatedTo =
                    EveryDict.get relatedToId db.people
                        |> Maybe.withDefault NotAsked
            in
            div []
                [ h4
                    [ class "ui header" ]
                    [ text <| translate language <| Translate.MyRelationship myRelationship ]
                , div
                    [ class "ui unstackable items" ]
                    [ viewWebData language (viewParticipant language currentDate relatedToId) identity relatedTo ]
                ]
    in
    toMyRelationship personId relationship
        |> Maybe.map viewMyRelationship
        |> showMaybe


viewParticipantDetailsForm : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html App.Model.Msg
viewParticipantDetailsForm language currentDate db id person =
    let
        viewFamilyMembers relationships =
            relationships
                |> EveryDictList.map (viewRelationship language currentDate db id)
                |> EveryDictList.values
                |> div []

        familyMembers =
            EveryDict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> viewWebData language viewFamilyMembers identity
    in
    div [ class "wrap-list registration-page view" ]
        [ h3
            [ class "ui header" ]
            [ text <| translate language Translate.DemographicInformation ++ ": " ]
        , div
            [ class "ui unstackable items" ]
            [ viewParticipant language currentDate id person ]
        , div [ class "separator-line" ] []
        , h3
            [ class "ui header" ]
            [ text <| translate language Translate.FamilyMembers ++ ": " ]
        , familyMembers
        ]


viewParticipant : Language -> NominalDate -> PersonId -> Person -> Html App.Model.Msg
viewParticipant language currentDate id person =
    let
        typeForThumbnail =
            ageInYears currentDate person
                |> Maybe.map
                    (\age ->
                        if age > 12 then
                            "mother"

                        else
                            "child"
                    )
                |> Maybe.withDefault "mother"

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text <| person.name ]
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


viewCreateForm : Language -> NominalDate -> PersonForm -> WebData PersonId -> Html Msg
viewCreateForm language currentDate personForm request =
    let
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

        birthDateInput =
            div [ class "ui grid" ]
                [ div
                    [ classList
                        [ ( "six wide column", True )
                        , ( "required", True )
                        ]
                    ]
                    [ text <| translate language Translate.DateOfBirth ++ ":" ]
                , div
                    [ class "ten wide column" ]
                    [ dateInput birthDateField
                        [ classList
                            [ ( "error", isJust birthDateField.liveError )
                            , ( "field", True )
                            ]
                        , birthDateField.value
                            |> Maybe.withDefault ""
                            |> value
                        ]
                    ]
                ]

        birthDateEstimatedField =
            Form.getFieldAsBool Backend.Person.Form.birthDateEstimated personForm

        birthDateEstimatedInput =
            div [ class "ui grid" ]
                [ div
                    [ classList
                        [ ( "six wide column", True )
                        , ( "required", True )
                        ]
                    ]
                    [ text <| translate language Translate.Estimated ++ ":" ]
                , div
                    [ class "ten wide column" ]
                    [ Form.Input.checkboxInput birthDateEstimatedField
                        [ classList
                            [ ( "error", isJust birthDateEstimatedField.liveError )
                            , ( "field", True )
                            ]
                        ]
                    ]
                ]

        genderField =
            Form.getFieldAsString Backend.Person.Form.gender personForm

        genderInput =
            div [ class "ui grid" ]
                [ div
                    [ class "six wide column" ]
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
                    |> keyed "dropzone"

                -- This runs the function from our `app.js` at the precise moment this gets
                -- written to the DOM. Indeed very convenient.
                , script "bindDropZone()"
                    |> keyed "script"
                ]

        demographicFields =
            viewPhoto
                :: List.map (Html.map MsgForm)
                    [ viewTextInput language Translate.FirstName Backend.Person.Form.firstName True personForm
                    , viewTextInput language Translate.MiddleName Backend.Person.Form.middleName False personForm
                    , viewTextInput language Translate.SecondName Backend.Person.Form.secondName True personForm
                    , viewTextInput language Translate.NationalIdNumber Backend.Person.Form.nationalIdNumber False personForm
                    , birthDateInput
                    , birthDateEstimatedInput
                    , genderInput
                    , viewSelectInput language Translate.LevelOfEducationLabel educationLevelOptions Backend.Person.Form.educationLevel "ten" "select-input" True personForm
                    , viewTextInput language Translate.Profession Backend.Person.Form.profession False personForm
                    , viewSelectInput language Translate.MaritalStatusLabel maritalStatusOptions Backend.Person.Form.maritalStatus "ten" "select-input" True personForm
                    ]

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

        contactInformationFields =
            [ viewTextInput language Translate.TelephoneNumber Backend.Person.Form.phoneNumber False personForm
            ]

        submitButton =
            button
                [ classList
                    [ ( "ui button primary", True )
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
                |> Html.map MsgForm
            , h3
                [ class "ui header" ]
                [ text <| translate language Translate.AddressInformation ++ ":" ]
            , addressFields
                |> fieldset [ class "registration-form address-info" ]
                |> Html.map MsgForm
            , h3
                [ class "ui header" ]
                [ text <| translate language Translate.ContactInformation ++ ":" ]
            , contactInformationFields
                |> fieldset [ class "registration-form address-info" ]
                |> Html.map MsgForm
            , submitButton
                |> Html.map MsgForm

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
        [ class "wrap wrap-alt-2" ]
        [ header
        , div
            [ class "ui full segment blue" ]
            [ div
                [ class "content" ]
                [ div
                    [ class "wrap-list registration-page form" ]
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
