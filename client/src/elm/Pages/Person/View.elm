module Pages.Person.View exposing (view, viewCreateEditForm)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Encoder
    exposing
        ( encodeEducationLevel
        , encodeHivStatus
        , encodeMaritalStatus
        , encodeModeOfDelivery
        , encodeUbudehe
        )
import Backend.Person.Form exposing (PersonForm, applyDefaultValues, expectedAgeByForm, validatePerson)
import Backend.Person.Model exposing (ExpectedAge(..), Gender(..), ParticipantDirectoryOperation(..), Person, allEducationLevels, allHivStatuses, allMaritalStatuses, allModesOfDelivery, allUbudehes)
import Backend.Person.Utils exposing (expectedAgeByPerson, isAdult, isPersonAnAdult)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Backend.Village.Utils exposing (getVillageById)
import Date exposing (Unit(..))
import DateSelector.SelectorDropdown
import Form exposing (Form)
import Form.Field
import Form.Input
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityId)
import Set
import Translate exposing (Language, TranslationId, translate)
import Utils.Form exposing (getValueAsInt, isFormFieldSet, viewFormError)
import Utils.GeoLocation exposing (GeoInfo, geoInfo)
import Utils.Html exposing (thumbnailImage, viewLoading)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


view : Language -> NominalDate -> Bool -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate isChw id db =
    let
        person =
            Dict.get id db.people
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
            [ viewWebData language (viewParticipantDetailsForm language currentDate isChw db id) identity person
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


{-| We want to show other people related to this person, either because they
have a `Relationship` or because they are paired in a group. So, we have this
custom type to track those other persons.
-}
type alias OtherPerson =
    { relationship : Maybe ( RelationshipId, MyRelationship )
    , groups : List ( PmtctParticipantId, PmtctParticipant )
    }


viewParticipantDetailsForm : Language -> NominalDate -> Bool -> ModelIndexedDb -> PersonId -> Person -> Html App.Model.Msg
viewParticipantDetailsForm language currentDate isChw db id person =
    let
        -- We re-organize our data about relatoinships and group participations
        -- so that we have one record per `OtherPerson`.
        relationshipsData =
            Dict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked

        participationsData =
            Dict.get id db.participantsByPerson
                |> Maybe.withDefault NotAsked

        addRelationshipToOtherPeople : RelationshipId -> MyRelationship -> Dict PersonId OtherPerson -> Dict PersonId OtherPerson
        addRelationshipToOtherPeople relationshipId myRelationship accum =
            Dict.update myRelationship.relatedTo
                (\existing ->
                    Just
                        { relationship = Just ( relationshipId, myRelationship )
                        , groups =
                            Maybe.map .groups existing
                                |> Maybe.withDefault []
                        }
                )
                accum

        addParticipantToOtherPeople : PmtctParticipantId -> PmtctParticipant -> Dict PersonId OtherPerson -> Dict PersonId OtherPerson
        addParticipantToOtherPeople pmtctParticipantId pmtctParticipant accum =
            let
                otherParticipantId =
                    if pmtctParticipant.child == id then
                        pmtctParticipant.adult

                    else
                        pmtctParticipant.child
            in
            Dict.update otherParticipantId
                (\existing ->
                    Just
                        { relationship = Maybe.andThen .relationship existing
                        , groups =
                            Maybe.map .groups existing
                                |> Maybe.withDefault []
                                |> (::) ( pmtctParticipantId, pmtctParticipant )
                        }
                )
                accum

        otherPeople : WebData (Dict PersonId OtherPerson)
        otherPeople =
            RemoteData.append relationshipsData participationsData
                |> RemoteData.map
                    (\( relationships, participations ) ->
                        let
                            withParticipants =
                                Dict.foldl addParticipantToOtherPeople Dict.empty participations
                        in
                        Dict.foldl addRelationshipToOtherPeople withParticipants relationships
                    )

        viewOtherPeople people =
            people
                |> Dict.map
                    (\otherPersonId otherPerson ->
                        Dict.get otherPersonId db.people
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.append db.clinics
                            |> viewWebData language (viewOtherPerson language currentDate isChw id ( otherPersonId, otherPerson )) identity
                    )
                |> Dict.values
                |> div [ class "ui unstackable items participants-list" ]

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
                            , style "height" "120px"
                            , style "width" "120px"
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
                                , onClick <| App.Model.SetActivePage <| UserPage <| PersonsPage (Just id)
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
            [ viewPerson language currentDate id person ]
        , h3
            [ class "ui header" ]
            [ text <| translate language Translate.FamilyMembers ++ ": " ]
        , viewWebData language viewOtherPeople identity otherPeople
        , p [] []
        , addFamilyMember
        ]


viewPerson : Language -> NominalDate -> PersonId -> Person -> Html App.Model.Msg
viewPerson language currentDate id person =
    let
        typeForThumbnail =
            case isPersonAnAdult currentDate person of
                Just True ->
                    "mother"

                Just False ->
                    "child"

                Nothing ->
                    "mother"

        action =
            div
                [ class "action" ]
                [ div
                    [ class "action-icon-wrapper" ]
                    [ span
                        [ class "action-icon edit"
                        , onClick <| App.Model.SetActivePage <| UserPage <| EditPersonPage id
                        ]
                        []
                    ]
                ]

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text person.name ]
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
                , action
                ]
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage typeForThumbnail person.avatarUrl person.name 120 120 ]
        , content
        ]


viewOtherPerson : Language -> NominalDate -> Bool -> PersonId -> ( PersonId, OtherPerson ) -> ( Dict ClinicId Clinic, Person ) -> Html App.Model.Msg
viewOtherPerson language currentDate isChw relationMainId ( otherPersonId, otherPerson ) ( clinics, person ) =
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
            otherPerson.relationship
                |> Maybe.map
                    (\( _, relationship ) ->
                        span
                            [ class "relationship" ]
                            [ text " ("
                            , text <| translate language <| Translate.MyRelatedBy relationship.relatedBy
                            , text ")"
                            ]
                    )
                |> Maybe.withDefault emptyNode

        groupNames =
            otherPerson.groups
                |> List.map (\( _, group ) -> Dict.get group.clinic clinics)
                |> List.filterMap (Maybe.map .name)
                |> String.join ", "

        groups =
            if isChw then
                emptyNode

            else
                p
                    []
                    [ label [] [ text <| translate language Translate.Groups ++ ": " ]
                    , span [] [ text groupNames ]
                    ]

        action =
            if isChw then
                emptyNode

            else
                div
                    [ class "action" ]
                    [ div
                        [ class "action-icon-wrapper" ]
                        [ span
                            [ class "action-icon forward"
                            , onClick <| App.Model.SetActivePage <| UserPage <| RelationshipPage relationMainId otherPersonId
                            ]
                            []
                        ]
                    ]

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
                    , groups
                    ]
                , action
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
            , class "ui small image rotate-90"
            ]
            []
        ]


viewCreateEditForm : Language -> NominalDate -> Maybe VillageId -> Bool -> ParticipantDirectoryOperation -> Model -> ModelIndexedDb -> Html Msg
viewCreateEditForm language currentDate maybeVillageId isChw operation model db =
    let
        formBeforeDefaults =
            model.form

        personId =
            case operation of
                CreatePerson maybePersonId ->
                    maybePersonId

                EditPerson personId_ ->
                    Just personId_

        -- When we create new person, this is a person that we want to associate
        -- new person with.
        -- When editing, this is the person that is being edited.
        maybeRelatedPerson =
            personId
                |> Maybe.andThen (\id -> Dict.get id db.people)
                |> Maybe.andThen RemoteData.toMaybe

        maybeVillage =
            maybeVillageId
                |> Maybe.andThen (getVillageById db)

        personForm =
            applyDefaultValues currentDate maybeVillage isChw maybeRelatedPerson operation formBeforeDefaults

        request =
            db.postPerson

        emptyOption =
            ( "", "" )

        goBackAction =
            case operation of
                CreatePerson _ ->
                    SetActivePage <| UserPage <| PersonsPage personId

                EditPerson _ ->
                    personId
                        |> Maybe.map
                            (\personId_ ->
                                SetActivePage <| UserPage <| PersonPage personId_
                            )
                        |> Maybe.withDefault (SetActivePage PinCodePage)

        header =
            div [ class "ui basic segment head" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Translate.People ]
                , a
                    [ class "link-back"
                    , onClick goBackAction
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

        expectedAge =
            maybeRelatedPerson
                |> Maybe.map
                    (\related -> expectedAgeByPerson currentDate related operation)
                -- If we don't have a related person, or don't know whether
                -- that person is an adult, then we check whether a birthdate
                -- has been entered into the form so far.
                |> Maybe.withDefault (expectedAgeByForm currentDate personForm operation)

        birthDateEstimatedField =
            Form.getFieldAsBool Backend.Person.Form.birthDateEstimated personForm

        selectedBirthDate =
            Form.getFieldAsString Backend.Person.Form.birthDate personForm
                |> .value
                |> Maybe.andThen (Date.fromIsoString >> Result.toMaybe)

        ( birthDateSelectorFrom, birthDateSelectorTo ) =
            case operation of
                -- When creating without relation, allow full dates range.
                CreatePerson Nothing ->
                    ( Date.add Years -60 currentDate, currentDate )

                _ ->
                    case expectedAge of
                        ExpectChild ->
                            ( Date.add Years -13 currentDate, currentDate )

                        ExpectAdult ->
                            ( Date.add Years -60 currentDate, Date.add Years -13 currentDate )

                        ExpectAdultOrChild ->
                            ( Date.add Years -60 currentDate, currentDate )

        birthDateInput =
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
                        (DateSelected operation)
                        model.isDateSelectorOpen
                        birthDateSelectorFrom
                        birthDateSelectorTo
                        selectedBirthDate
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
                        |> Html.map (MsgForm operation)
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
                        ( Debug.toString (encodeEducationLevel level)
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

        modeOfDeliveryOptions =
            allModesOfDelivery
                |> List.map
                    (\mode ->
                        ( encodeModeOfDelivery mode
                        , translate language <| Translate.ModeOfDelivery mode
                        )
                    )
                |> (::) emptyOption

        hivStatusOptions =
            allHivStatuses
                |> List.map
                    (\status ->
                        ( encodeHivStatus status
                        , translate language <| Translate.HIVStatus status
                        )
                    )
                |> (::) emptyOption

        hmisNumberOptions =
            List.repeat 15 ""
                |> List.indexedMap
                    (\index _ ->
                        let
                            order =
                                index + 1

                            orderAsString =
                                if order < 10 then
                                    "0" ++ String.fromInt order

                                else
                                    String.fromInt order
                        in
                        ( orderAsString, orderAsString )
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
                    , on "dropzonecomplete" (Json.Decode.map (DropZoneComplete operation) decodeDropZoneFile)
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
                ]

        levelOfEducationInput =
            viewSelectInput language Translate.LevelOfEducationLabel educationLevelOptions Backend.Person.Form.educationLevel "ten" "select-input" True personForm

        maritalStatusInput =
            viewSelectInput language Translate.MaritalStatusLabel maritalStatusOptions Backend.Person.Form.maritalStatus "ten" "select-input" True personForm

        modeOfDeliveryInput =
            viewSelectInput language Translate.ModeOfDeliveryLabel modeOfDeliveryOptions Backend.Person.Form.modeOfDelivery "ten" "select-input" True personForm

        hivStatusInput =
            viewSelectInput language Translate.HIVStatusLabel hivStatusOptions Backend.Person.Form.hivStatus "ten" "select-input" True personForm

        numberOfChildrenUnder5Input =
            let
                options =
                    emptyOption
                        :: (List.repeat 5 "."
                                |> List.indexedMap (\index _ -> ( Debug.toString index, Debug.toString index ))
                           )
            in
            viewSelectInput language Translate.NumberOfChildrenUnder5 options Backend.Person.Form.numberOfChildren "ten" "select-input" False personForm

        hmisNumberInput =
            viewSelectInput language Translate.ChildHmisNumber hmisNumberOptions Backend.Person.Form.hmisNumber "ten" "select-input" False personForm

        demographicFields =
            viewPhoto
                :: (List.map (Html.map (MsgForm operation)) <|
                        [ viewTextInput language Translate.FirstName Backend.Person.Form.firstName False personForm
                        , viewTextInput language Translate.SecondName Backend.Person.Form.secondName True personForm
                        , viewNumberInput language Translate.NationalIdNumber Backend.Person.Form.nationalIdNumber False personForm
                        ]
                   )
                ++ [ birthDateInput ]
                ++ (List.map (Html.map (MsgForm operation)) <|
                        case expectedAge of
                            ExpectAdult ->
                                [ genderInput
                                , hivStatusInput
                                , levelOfEducationInput
                                , maritalStatusInput
                                , numberOfChildrenUnder5Input
                                ]

                            ExpectChild ->
                                [ hmisNumberInput
                                , genderInput
                                , hivStatusInput
                                , modeOfDeliveryInput
                                ]

                            ExpectAdultOrChild ->
                                [ hmisNumberInput
                                , genderInput
                                , hivStatusInput
                                , levelOfEducationInput
                                , maritalStatusInput
                                , modeOfDeliveryInput
                                , numberOfChildrenUnder5Input
                                ]
                   )

        ubudeheOptions =
            allUbudehes
                |> List.map
                    (\ubudehe ->
                        ( Debug.toString (encodeUbudehe ubudehe)
                        , Debug.toString (encodeUbudehe ubudehe)
                        )
                    )
                |> (::) emptyOption

        familyInformationFields =
            [ viewSelectInput language Translate.FamilyUbudehe ubudeheOptions Backend.Person.Form.ubudehe "ten" "select-input" True personForm
            ]

        geoLocationDictToOptions dict =
            dict
                |> Dict.toList
                |> List.map
                    (\( id, geoLocation ) ->
                        ( Debug.toString <| fromEntityId id, geoLocation.name )
                    )

        filterGeoLocationDictByParent parentId dict =
            dict
                |> Dict.filter
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

        isEditOperation =
            case operation of
                CreatePerson _ ->
                    False

                EditPerson _ ->
                    True

        viewVillage =
            let
                disabled =
                    isEditOperation
                        && (isAdult currentDate selectedBirthDate
                                |> Maybe.map not
                                |> Maybe.withDefault False
                           )
                        && isFormFieldSet village

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
                (geoLocationInputClass disabled)
                True
                personForm

        addressFields =
            [ viewProvince
            , viewDistrict
            , viewSector
            , viewCell
            , viewVillage
            ]

        addressSection =
            if isChw then
                []

            else
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.AddressInformation ++ ":" ]
                , addressFields
                    |> fieldset [ class "registration-form address-info" ]
                    |> Html.map (MsgForm operation)
                ]

        contactInformationSection =
            if expectedAge /= ExpectChild then
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.ContactInformation ++ ":" ]
                , [ viewTextInput language Translate.TelephoneNumber Backend.Person.Form.phoneNumber False personForm ]
                    |> fieldset [ class "registration-form address-info" ]
                    |> Html.map (MsgForm operation)
                ]

            else
                []

        healthCenter =
            Form.getFieldAsString Backend.Person.Form.healthCenter personForm

        healthCenterSection =
            if isChw then
                []

            else
                let
                    inputClass =
                        "select-input"
                            ++ (if isEditOperation && isFormFieldSet healthCenter then
                                    " disabled"

                                else
                                    ""
                               )

                    options =
                        emptyOption
                            :: (db.healthCenters
                                    |> RemoteData.map
                                        (\dict ->
                                            dict
                                                |> Dict.toList
                                                |> List.map
                                                    (\( id, healthCenter_ ) ->
                                                        ( fromEntityUuid id
                                                        , healthCenter_.name
                                                        )
                                                    )
                                                |> List.sortBy (\( id, name ) -> name)
                                        )
                                    |> RemoteData.withDefault []
                               )
                in
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.RegistratingHealthCenter ++ ":" ]
                , [ viewSelectInput language Translate.HealthCenter options Backend.Person.Form.healthCenter "ten" inputClass True personForm ]
                    |> fieldset [ class "registration-form health-center" ]
                    |> Html.map (MsgForm operation)
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
                |> Html.map (MsgForm operation)
            ]
                ++ addressSection
                ++ contactInformationSection
                ++ healthCenterSection
                ++ [ p [] []
                   , submitButton
                        |> Html.map (MsgForm operation)

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
    viewFormInput language labelId Form.Input.textInput fieldName isRequired form


viewNumberInput : Language -> TranslationId -> String -> Bool -> Form e a -> Html Form.Msg
viewNumberInput language labelId fieldName isRequired form =
    viewFormInput language labelId (Form.Input.baseInput "number" Form.Field.String Form.Text) fieldName isRequired form


viewFormInput : Language -> TranslationId -> Form.Input.Input e String -> String -> Bool -> Form e a -> Html Form.Msg
viewFormInput language labelId formInput fieldName isRequired form =
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
            [ formInput field
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
