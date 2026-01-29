module Pages.Person.View exposing (view, viewCreateEditForm, viewSelectInput, viewTextInput)

import App.Model exposing (GPSCoordinates)
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (Clinic, ClinicType(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form exposing (applyDefaultValuesForPerson, expectedAgeByForm)
import Backend.Person.Model
    exposing
        ( ExpectedAge(..)
        , ExpectedGender(..)
        , Initiator(..)
        , ParticipantDirectoryOperation(..)
        , Person
        , allEducationLevels
        , allHivStatuses
        , allMaritalStatuses
        , allModesOfDelivery
        , allUbudehes
        )
import Backend.Person.Utils
    exposing
        ( defaultIconForPerson
        , educationLevelToInt
        , expectedAgeByPerson
        , generateFullName
        , graduatingAgeInMonth
        , hivStatusToString
        , isAdult
        , isPersonAnAdult
        , maritalStatusToString
        , modeOfDeliveryToString
        , ubudeheToInt
        )
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.PrenatalActivity.Model
import Backend.Relationship.Model exposing (MyRelationship)
import Backend.Session.Utils exposing (getSession)
import Backend.Utils exposing (gpsCoordinatesEnabled)
import Backend.Village.Utils exposing (getVillageById)
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Form exposing (Form)
import Form.Field
import Form.Input
import GeoLocation.Model exposing (GeoInfo, ReverseGeoInfo)
import GeoLocation.Utils exposing (filterGeoLocationDictByParent, geoLocationDictToOptions, resolveGeoSructureLabelLevel1, resolveGeoSructureLabelLevel2, resolveGeoSructureLabelLevel3, resolveGeoSructureLabelLevel4, resolveGeoSructureLabelLevel5)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffMonths, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model exposing (Model, Msg(..))
import Pages.Utils exposing (viewConfirmationDialog)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid)
import Set
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Form exposing (getValueAsInt, isFormFieldSet, viewFormError)
import Utils.Html exposing (thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


view : Language -> NominalDate -> Bool -> Initiator -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate isChw initiator id db =
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
        [ viewHeader language initiator headerName
        , div
            [ class "ui full segment blue" ]
            [ viewWebData language (viewParticipantDetailsForm language currentDate isChw initiator db id) identity person
            ]
        ]


viewHeader : Language -> Initiator -> String -> Html App.Model.Msg
viewHeader language initiator name =
    let
        goBackPage =
            UserPage (PersonsPage Nothing initiator)
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text name ]
        , span
            [ class "link-back"
            , onClick <| App.Model.SetActivePage goBackPage
            ]
            [ span [ class "icon-back" ] []
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


viewParticipantDetailsForm : Language -> NominalDate -> Bool -> Initiator -> ModelIndexedDb -> PersonId -> Person -> Html App.Model.Msg
viewParticipantDetailsForm language currentDate isChw initiator db id person =
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
                            |> viewWebData language (viewOtherPerson language currentDate isChw initiator db id ( otherPersonId, otherPerson )) identity
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
                                , onClick <| App.Model.SetActivePage <| UserPage <| PersonsPage (Just id) initiator
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
            [ viewPerson language currentDate initiator db id person ]
        , h3
            [ class "ui header" ]
            [ text <| translate language Translate.FamilyMembers ++ ": " ]
        , viewWebData language viewOtherPeople identity otherPeople
        , p [] []
        , addFamilyMember
        ]


viewPerson : Language -> NominalDate -> Initiator -> ModelIndexedDb -> PersonId -> Person -> Html App.Model.Msg
viewPerson language currentDate initiator db id person =
    let
        typeForThumbnail =
            defaultIconForPerson currentDate person

        action =
            if initiator == ParticipantDirectoryOrigin then
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

            else
                emptyNode

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
                        , span [] [ text <| Maybe.withDefault "" person.village ]
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


viewOtherPerson : Language -> NominalDate -> Bool -> Initiator -> ModelIndexedDb -> PersonId -> ( PersonId, OtherPerson ) -> ( Dict ClinicId Clinic, Person ) -> Html App.Model.Msg
viewOtherPerson language currentDate isChw initiator db relationMainId ( otherPersonId, otherPerson ) ( clinics, person ) =
    let
        typeForThumbnail =
            defaultIconForPerson currentDate person

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

        ( groups, action ) =
            if isChw then
                ( emptyNode, emptyNode )

            else
                let
                    groupNames =
                        otherPerson.groups
                            |> List.map (\( _, group ) -> Dict.get group.clinic clinics)
                            |> List.filterMap (Maybe.map .name)
                            |> String.join ", "

                    viewGoToRelationshipPageArrow =
                        div
                            [ class "action" ]
                            [ div
                                [ class "action-icon-wrapper" ]
                                [ span
                                    [ class "action-icon forward"
                                    , onClick <| App.Model.SetActivePage <| UserPage <| RelationshipPage relationMainId otherPersonId initiator
                                    ]
                                    []
                                ]
                            ]
                in
                ( p []
                    [ label [] [ text <| translate language Translate.Groups ++ ": " ]
                    , span [] [ text groupNames ]
                    ]
                , case initiator of
                    ParticipantDirectoryOrigin ->
                        viewGoToRelationshipPageArrow

                    IndividualEncounterOrigin _ ->
                        -- For now, we do not use this page for individual encounters.
                        -- Those got their own dedicated page.
                        emptyNode

                    GroupEncounterOrigin sessionId ->
                        -- We show the arrow only when person does not yet
                        -- a participant of clinic to which initating session belongs.
                        -- Also, when person is a child, we examine clinic type and
                        -- age, to determine whether or not to show the arrow.
                        getSession sessionId db
                            |> Maybe.map
                                (\session ->
                                    let
                                        -- Allow any adult. Allow any child, when clinic type is
                                        -- Sorwathe/Achi. When clinic type is other, allow child
                                        -- with age lower than 26 month.
                                        isAdult =
                                            isPersonAnAdult currentDate person
                                                |> Maybe.withDefault True

                                        qualifiesByAge =
                                            isAdult || List.member session.clinicType [ Sorwathe, Achi ] || childAgeCondition

                                        -- When clinic type is not Sorwathe or Achi, we expect child age
                                        -- to be less than 26 month.
                                        childAgeCondition =
                                            person.birthDate
                                                |> Maybe.map (\birthDate -> diffMonths birthDate currentDate < graduatingAgeInMonth)
                                                |> Maybe.withDefault False

                                        alreadyParticipates =
                                            otherPerson.groups
                                                |> List.map (Tuple.second >> .clinic)
                                                |> List.member session.clinicId
                                    in
                                    if alreadyParticipates || not qualifiesByAge then
                                        emptyNode

                                    else
                                        viewGoToRelationshipPageArrow
                                )
                            |> Maybe.withDefault emptyNode

                    PrenatalNextStepsNewbornEnrolmentOrigin _ _ ->
                        -- We do not allow this actions when registering newborn child.
                        emptyNode

                    AcuteIllnessContactsTracingActivityOrigin _ ->
                        -- Not in use, as at Acute Ilness patient is created
                        -- from a dedicated form.
                        emptyNode
                )

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
                        , span [] [ text <| Maybe.withDefault "" person.village ]
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
            , class "ui small image orientation"
            ]
            []
        ]


viewCreateEditForm :
    Language
    -> NominalDate
    -> Maybe GPSCoordinates
    -> Site
    -> EverySet SiteFeature
    -> GeoInfo
    -> ReverseGeoInfo
    -> Maybe VillageId
    -> Bool
    -> ParticipantDirectoryOperation
    -> Initiator
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewCreateEditForm language currentDate coordinates site features geoInfo reverseGeoInfo maybeVillageId isChw operation initiator model db =
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
            Maybe.andThen (\id -> Dict.get id db.people) personId
                |> Maybe.andThen RemoteData.toMaybe

        maybeVillage =
            Maybe.andThen (getVillageById db) maybeVillageId

        today =
            currentDate

        personForm =
            applyDefaultValuesForPerson currentDate
                site
                reverseGeoInfo
                maybeVillage
                isChw
                maybeRelatedPerson
                operation
                initiator
                formBeforeDefaults

        request =
            db.postPerson

        emptyOption =
            ( "", "" )

        originBasedSettings =
            case initiator of
                ParticipantDirectoryOrigin ->
                    let
                        goBackPage =
                            case operation of
                                CreatePerson _ ->
                                    UserPage <| PersonsPage personId initiator

                                EditPerson _ ->
                                    personId
                                        |> Maybe.map
                                            (\personId_ ->
                                                UserPage <| PersonPage personId_ initiator
                                            )
                                        |> Maybe.withDefault PinCodePage

                        expectedAge =
                            maybeRelatedPerson
                                |> Maybe.map
                                    (\related -> expectedAgeByPerson currentDate related operation)
                                -- If we don't have a related person, or don't know whether
                                -- that person is an adult, then we check whether a birthdate
                                -- has been entered into the form so far.
                                |> Maybe.withDefault (expectedAgeByForm currentDate personForm operation)

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
                    in
                    { goBackPage = goBackPage
                    , expectedAge = expectedAge
                    , expectedGender = ExpectMaleOrFemale
                    , birthDateSelectorFrom = birthDateSelectorFrom
                    , birthDateSelectorTo = birthDateSelectorTo
                    , title = Translate.People
                    }

                IndividualEncounterOrigin encounterType ->
                    case encounterType of
                        AcuteIllnessEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage AcuteIllnessEncounter)
                            , expectedAge = expectedAgeByForm currentDate personForm operation
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -120 today
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                        AntenatalEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage AntenatalEncounter)
                            , expectedAge = ExpectAdult
                            , expectedGender = ExpectFemale
                            , birthDateSelectorFrom = Date.add Years -120 today
                            , birthDateSelectorTo = Date.add Years -13 today
                            , title = Translate.People
                            }

                        ChildScoreboardEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage ChildScoreboardEncounter)
                            , expectedAge = ExpectChild
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -2 today
                            , birthDateSelectorTo = Date.add Days -1 today
                            , title = Translate.People
                            }

                        HIVEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage HIVEncounter)
                            , expectedAge = expectedAgeByForm currentDate personForm operation
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -120 today
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                        -- We do not have a direct access to Home Visit
                        -- encounter, since it resides under Nutrition menu.
                        -- Providing 'default' values, to satisfy compiler.
                        HomeVisitEncounter ->
                            { goBackPage = PinCodePage
                            , expectedAge = ExpectAdultOrChild
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -60 today
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                        NCDEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage NCDEncounter)
                            , expectedAge = expectedAgeByForm currentDate personForm operation
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -120 today
                            , birthDateSelectorTo = Date.add Years -12 today
                            , title = Translate.People
                            }

                        NutritionEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage NutritionEncounter)
                            , expectedAge = ExpectChild
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -13 today |> Date.add Days 1
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                        TuberculosisEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage TuberculosisEncounter)
                            , expectedAge = expectedAgeByForm currentDate personForm operation
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -120 today
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                        WellChildEncounter ->
                            { goBackPage = UserPage (IndividualEncounterParticipantsPage WellChildEncounter)
                            , expectedAge = ExpectChild
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -13 today
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                        -- Note yet implemented. Providing 'default'
                        -- values, to satisfy compiler.
                        InmmunizationEncounter ->
                            { goBackPage = PinCodePage
                            , expectedAge = ExpectAdultOrChild
                            , expectedGender = ExpectMaleOrFemale
                            , birthDateSelectorFrom = Date.add Years -60 today
                            , birthDateSelectorTo = today
                            , title = Translate.People
                            }

                GroupEncounterOrigin sessionId ->
                    let
                        expectedAge =
                            maybeRelatedPerson
                                |> Maybe.map
                                    (\related -> expectedAgeByPerson currentDate related operation)
                                -- If we don't have a related person, or don't know whether
                                -- that person is an adult, then we check whether a birthdate
                                -- has been entered into the form so far.
                                |> Maybe.withDefault (expectedAgeByForm currentDate personForm operation)

                        ( birthDateSelectorFrom, birthDateSelectorTo ) =
                            case operation of
                                -- When creating with relation, limit child age
                                -- according to type of clinic to which session belongs.
                                CreatePerson (Just _) ->
                                    case expectedAge of
                                        ExpectChild ->
                                            let
                                                defaultMaximalAge =
                                                    Date.add Years -13 currentDate

                                                maximalAge =
                                                    getSession sessionId db
                                                        |> Maybe.map
                                                            (\session ->
                                                                if List.member session.clinicType [ Sorwathe, Achi ] then
                                                                    defaultMaximalAge

                                                                else
                                                                    Date.add Months (-1 * graduatingAgeInMonth) currentDate
                                                            )
                                                        |> Maybe.withDefault defaultMaximalAge
                                            in
                                            ( maximalAge, currentDate )

                                        ExpectAdult ->
                                            ( Date.add Years -60 currentDate, Date.add Years -13 currentDate )

                                        ExpectAdultOrChild ->
                                            ( Date.add Years -60 currentDate, currentDate )

                                _ ->
                                    ( Date.add Years -60 currentDate, currentDate )
                    in
                    { goBackPage = UserPage (PersonsPage personId initiator)
                    , expectedAge = expectedAge
                    , expectedGender = ExpectMaleOrFemale
                    , birthDateSelectorFrom = birthDateSelectorFrom
                    , birthDateSelectorTo = birthDateSelectorTo
                    , title = Translate.People
                    }

                PrenatalNextStepsNewbornEnrolmentOrigin _ encounterId ->
                    { goBackPage = UserPage (PrenatalActivityPage encounterId Backend.PrenatalActivity.Model.NextSteps)
                    , expectedAge = ExpectChild
                    , expectedGender = ExpectMaleOrFemale
                    , birthDateSelectorFrom = Date.add Years -3 today
                    , birthDateSelectorTo = today
                    , title = Translate.EnrolNewborn
                    }

                AcuteIllnessContactsTracingActivityOrigin _ ->
                    -- Not in use, as at Acute Ilness patient is created
                    -- from a dedicated form.
                    { goBackPage = PinCodePage
                    , expectedAge = ExpectAdultOrChild
                    , expectedGender = ExpectMaleOrFemale
                    , birthDateSelectorFrom = today
                    , birthDateSelectorTo = today
                    , title = Translate.People
                    }

        header =
            div [ class "ui basic segment head" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language originBasedSettings.title ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage originBasedSettings.goBackPage
                    ]
                    [ span [ class "icon-back" ] []
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

        birthDateEstimatedField =
            Form.getFieldAsBool Backend.Person.Form.birthDateEstimated personForm

        selectedBirthDate =
            Form.getFieldAsString Backend.Person.Form.birthDate personForm
                |> .value
                |> Maybe.andThen (Date.fromIsoString >> Result.toMaybe)

        birthDateForView =
            Maybe.map formatDDMMYYYY selectedBirthDate
                |> Maybe.withDefault ""

        dateSelectorConfig =
            { select = DateSelected operation initiator
            , close = SetDateSelectorState Nothing
            , dateFrom = originBasedSettings.birthDateSelectorFrom
            , dateTo = originBasedSettings.birthDateSelectorTo
            , dateDefault = Nothing
            }

        birthDateInput =
            div [ class "ui grid" ]
                [ div
                    [ class "six wide column" ]
                    []
                , div
                    [ class "seven wide column required" ]
                    [ text <| translate language Translate.DateOfBirth ++ ":"
                    , br [] []
                    , div
                        [ class "date-input field"
                        , onClick <| SetDateSelectorState (Just dateSelectorConfig)
                        ]
                        [ text birthDateForView ]
                    , viewModal <| viewCalendarPopup language model.dateSelectorPopupState selectedBirthDate
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
                        |> Html.map (MsgForm operation initiator)
                    ]
                ]

        genderInput =
            let
                genderField =
                    Form.getFieldAsString Backend.Person.Form.gender personForm

                label =
                    div [ class "six wide column required" ]
                        [ text <| translate language Translate.GenderLabel ++ ":" ]

                femaleOption =
                    [ Form.Input.radioInput "female"
                        genderField
                        [ class "one wide column gender-input" ]
                    , div
                        [ class "three wide column" ]
                        [ text <| translate language (Translate.Gender Female) ]
                    ]

                options =
                    case originBasedSettings.expectedGender of
                        ExpectFemale ->
                            femaleOption

                        ExpectMaleOrFemale ->
                            let
                                maleOption =
                                    [ Form.Input.radioInput "male"
                                        genderField
                                        [ class "one wide column gender-input" ]
                                    , div
                                        [ class "three wide column" ]
                                        [ text <| translate language (Translate.Gender Male) ]
                                    ]
                            in
                            maleOption ++ femaleOption
            in
            div [ class "ui grid" ] <|
                (label :: options)

        educationLevelOptions =
            allEducationLevels
                |> List.map
                    (\level ->
                        ( String.fromInt (educationLevelToInt level)
                        , translate language (Translate.LevelOfEducation level)
                        )
                    )
                |> (::) emptyOption

        maritalStatusOptions =
            allMaritalStatuses
                |> List.map
                    (\status ->
                        ( maritalStatusToString status
                        , translate language <| Translate.MaritalStatus status
                        )
                    )
                |> (::) emptyOption

        modeOfDeliveryOptions =
            allModesOfDelivery
                |> List.map
                    (\mode ->
                        ( modeOfDeliveryToString mode
                        , translate language <| Translate.ModeOfDelivery mode
                        )
                    )
                |> (::) emptyOption

        hivStatusOptions =
            allHivStatuses
                |> List.map
                    (\status ->
                        ( hivStatusToString status
                        , translate language <| Translate.HIVStatus status
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
                    , on "dropzonecomplete" (Json.Decode.map (DropZoneComplete operation initiator) decodeDropZoneFile)
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
            viewSelectInput language Translate.HIVStatusLabel hivStatusOptions Backend.Person.Form.hivStatus "ten" "select-input" False personForm

        -- Not in use anymore - not displayed on form.
        numberOfChildrenUnder5Input =
            let
                options =
                    emptyOption
                        :: (List.repeat 5 "."
                                |> List.indexedMap (\index _ -> ( String.fromInt index, String.fromInt index ))
                           )
            in
            viewSelectInput language Translate.NumberOfChildrenUnder5 options Backend.Person.Form.numberOfChildren "ten" "select-input" False personForm

        -- Used only on Rwanda site.
        hmisNumberInput =
            case site of
                SiteRwanda ->
                    let
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
                    in
                    viewSelectInput language Translate.ChildHmisNumber hmisNumberOptions Backend.Person.Form.hmisNumber "ten" "select-input" False personForm

                _ ->
                    emptyNode

        firstNameInput =
            viewTextInput language Translate.FirstName Backend.Person.Form.firstName False personForm

        secondNameInput =
            viewTextInput language Translate.SecondName Backend.Person.Form.secondName True personForm

        nationalIdNumberInput =
            case site of
                SiteBurundi ->
                    viewTextInput language Translate.NationalIdNumber Backend.Person.Form.nationalIdNumber False personForm

                _ ->
                    viewNumberInput language Translate.NationalIdNumber Backend.Person.Form.nationalIdNumber False personForm

        demographicFields =
            viewPhoto
                :: (List.map (Html.map (MsgForm operation initiator)) <|
                        case site of
                            SiteBurundi ->
                                [ secondNameInput
                                , firstNameInput
                                , nationalIdNumberInput
                                ]

                            _ ->
                                [ firstNameInput
                                , secondNameInput
                                , nationalIdNumberInput
                                ]
                   )
                ++ [ birthDateInput ]
                ++ (List.map (Html.map (MsgForm operation initiator)) <|
                        case originBasedSettings.expectedAge of
                            ExpectAdult ->
                                [ genderInput
                                , hivStatusInput
                                , levelOfEducationInput
                                , maritalStatusInput
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
                                ]
                   )

        demographicSection =
            [ h3
                [ class "ui header" ]
                [ text <| translate language Translate.ParticipantDemographicInformation ++ ":" ]
            , demographicFields
                |> fieldset [ class "registration-form" ]
            ]

        -- Only field here is Ubudehe, and it's Rwanda specific.
        familyInformationSection =
            if site == SiteRwanda then
                let
                    familyInformationFields =
                        [ viewSelectInput language
                            Translate.FamilyUbudehe
                            ubudeheOptions
                            Backend.Person.Form.ubudehe
                            "ten"
                            "select-input"
                            False
                            personForm
                        ]

                    ubudeheOptions =
                        allUbudehes
                            |> List.map
                                (\ubudehe ->
                                    ( String.fromInt (ubudeheToInt ubudehe)
                                    , String.fromInt (ubudeheToInt ubudehe)
                                    )
                                )
                            |> (::) emptyOption
                in
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.FamilyInformation ++ ":" ]
                , familyInformationFields
                    |> fieldset [ class "registration-form family-info" ]
                    |> Html.map (MsgForm operation initiator)
                ]

            else
                []

        geoLocationInputClass isDisabled =
            "select-input"
                ++ (if isDisabled then
                        " disabled"

                    else
                        ""
                   )

        isEditOperation =
            case operation of
                CreatePerson _ ->
                    False

                EditPerson _ ->
                    True

        addressSection =
            if isChw then
                []

            else
                let
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
                            (resolveGeoSructureLabelLevel1 site)
                            options
                            Backend.Person.Form.province
                            "ten"
                            (geoLocationInputClass disabled)
                            True
                            personForm

                    viewDistrict =
                        let
                            province =
                                Form.getFieldAsString Backend.Person.Form.province personForm

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
                            (resolveGeoSructureLabelLevel2 site)
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
                            (resolveGeoSructureLabelLevel3 site)
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
                            (resolveGeoSructureLabelLevel4 site)
                            options
                            Backend.Person.Form.cell
                            "ten"
                            (geoLocationInputClass disabled)
                            True
                            personForm

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
                            (resolveGeoSructureLabelLevel5 site)
                            options
                            Backend.Person.Form.village
                            "ten"
                            (geoLocationInputClass disabled)
                            True
                            personForm
                in
                [ h3 [ class "ui header" ]
                    [ text <| translate language Translate.AddressInformation ++ ":" ]
                , fieldset [ class "registration-form address-info" ]
                    [ viewProvince
                    , viewDistrict
                    , viewSector
                    , viewCell
                    , viewVillage
                    ]
                    |> Html.map (MsgForm operation initiator)
                ]

        gpsInfoSection =
            if gpsCoordinatesEnabled features then
                let
                    sectionContent =
                        Maybe.map
                            (\coords ->
                                let
                                    saveGPSLocationField =
                                        Form.getFieldAsBool Backend.Person.Form.saveGPSLocation personForm
                                in
                                [ div [ class "ui grid" ]
                                    [ div [ class "six wide column" ]
                                        [ text <| translate language Translate.GPSLocation ++ ":" ]
                                    , div
                                        [ class "ten wide column" ]
                                        [ text <| String.fromFloat coords.latitude ++ " , " ++ String.fromFloat coords.longitude
                                        ]
                                    ]
                                , div [ class "ui grid" ]
                                    [ div [ class "eight wide column" ]
                                        [ text <| translate language Translate.GPSLocationSaveLabel ++ ":" ]
                                    , div
                                        [ class "three wide column" ]
                                        [ Form.Input.checkboxInput saveGPSLocationField
                                            [ class "field" ]
                                            |> Html.map (MsgForm operation initiator)
                                        ]
                                    ]
                                ]
                            )
                            coordinates
                            |> Maybe.withDefault []
                in
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.GPSInfo ++ ":" ]
                , fieldset [ class "registration-form gps-info" ]
                    sectionContent
                ]

            else
                []

        contactInformationSection =
            let
                content =
                    if originBasedSettings.expectedAge == ExpectChild then
                        [ div [ class "ui header secondary" ]
                            [ text <| translate language Translate.NextOfKin ++ ":" ]
                        , viewTextInput language Translate.Name Backend.Person.Form.nextOfKinName False personForm
                        , viewTextInput language Translate.TelephoneNumber Backend.Person.Form.nextOfKinPhoneNumber False personForm
                        ]

                    else
                        [ viewTextInput language Translate.TelephoneNumber Backend.Person.Form.phoneNumber False personForm
                        , div [ class "ui header secondary" ]
                            [ text <| translate language Translate.SpousePartner ++ ":" ]
                        , viewTextInput language Translate.Name Backend.Person.Form.spouseName False personForm
                        , viewTextInput language Translate.TelephoneNumber Backend.Person.Form.spousePhoneNumber False personForm
                        , div [ class "ui header secondary" ]
                            [ text <| translate language Translate.NextOfKin ++ ":" ]
                        , viewTextInput language Translate.Name Backend.Person.Form.nextOfKinName False personForm
                        , viewTextInput language Translate.TelephoneNumber Backend.Person.Form.nextOfKinPhoneNumber False personForm
                        ]
            in
            [ h3 [ class "ui header" ]
                [ text <| translate language Translate.ContactInformation ++ ":" ]
            , fieldset [ class "registration-form contact-info" ]
                content
                |> Html.map (MsgForm operation initiator)
            ]

        healthCenterSection =
            if isChw then
                []

            else
                let
                    healthCenter =
                        Form.getFieldAsString Backend.Person.Form.healthCenter personForm

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
                                                |> List.sortBy (\( _, name ) -> name)
                                        )
                                    |> RemoteData.withDefault []
                               )
                in
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Translate.RegistratingHealthCenter ++ ":" ]
                , [ viewSelectInput language Translate.HealthCenter options Backend.Person.Form.healthCenter "ten" inputClass True personForm ]
                    |> fieldset [ class "registration-form health-center" ]
                    |> Html.map (MsgForm operation initiator)
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
            demographicSection
                ++ familyInformationSection
                ++ addressSection
                ++ gpsInfoSection
                ++ contactInformationSection
                ++ healthCenterSection
                ++ [ p [] []
                   , submitButton
                        |> Html.map (MsgForm operation initiator)

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

        duplicateSuspectDialog =
            Maybe.andThen
                (\( duplicateSuspect, confirmationMsg ) ->
                    Maybe.map
                        (\nationalIdNumber ->
                            let
                                name =
                                    generateFullName duplicateSuspect.firstName duplicateSuspect.secondName
                            in
                            viewConfirmationDialog language
                                Translate.Warning
                                (Translate.DuplicateSuspectMessage name nationalIdNumber)
                                confirmationMsg
                                (SetDialogState Nothing)
                        )
                        duplicateSuspect.nationalIdNumber
                )
                model.dialogState
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
        , viewModal duplicateSuspectDialog
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
