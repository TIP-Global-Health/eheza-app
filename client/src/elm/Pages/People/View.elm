module Pages.People.View exposing (view)

import AssocList as Dict
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (ExpectedAge(..), Initiator(..), Person)
import Backend.Person.Utils exposing (defaultIconForPerson, graduatingAgeInMonth, isPersonAnAdult)
import Backend.PrenatalActivity.Model
import Backend.Session.Utils exposing (getSession)
import Backend.Village.Utils exposing (personLivesInVillage)
import Components.PatientsSearchForm.Utils exposing (..)
import Components.PatientsSearchForm.View
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isNothing, unwrap)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.People.Model exposing (..)
import Pages.Utils exposing (viewBySyncStatus)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Model
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewWebData)


{-| Shows a form which can be used to search people.

  - `searchString` is the string we're currently searching for, which is encoded
    in the URL
  - `relation` is the ID of a person who we're wanting to add a relationship for ...
    that is, we're searching in the context of trying to find (or create) a new
    family member for that person, either child, parent, etc.

-}
view :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Initiator
    -> Maybe PersonId
    -> SyncManager.Model.Model
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate healthCenterId maybeVillageId isChw initiator relation syncManager db model =
    let
        title =
            case relation of
                Just relationId ->
                    Dict.get relationId db.people
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map .name
                        |> RemoteData.withDefault (fromEntityUuid relationId)
                        |> (\name -> translate language (Translate.AddFamilyMemberFor name))

                Nothing ->
                    translate language Translate.People

        content =
            div
                [ class "search-wrapper" ]
                [ div [ class "ui full segment" ]
                    [ viewSearchForm language currentDate maybeVillageId isChw initiator relation model db ]
                ]
                |> viewBySyncStatus language healthCenterId syncManager.syncInfoAuthorities
    in
    div
        [ class "page-people" ]
        [ viewHeader initiator relation title
        , content
        ]


viewHeader : Initiator -> Maybe PersonId -> String -> Html Msg
viewHeader initiator relation title =
    let
        goBackPage =
            case initiator of
                ParticipantDirectoryOrigin ->
                    relation
                        |> Maybe.map (\personId -> UserPage (PersonPage personId initiator))
                        |> Maybe.withDefault PinCodePage

                IndividualEncounterOrigin _ ->
                    -- For now, we do not use this page for individual encounters.
                    -- Those got their own dedicated page.
                    -- Therefore, we default to Pincode page.
                    PinCodePage

                GroupEncounterOrigin sessionId ->
                    relation
                        |> Maybe.map (\personId -> UserPage (PersonPage personId initiator))
                        |> Maybe.withDefault (UserPage (SessionPage sessionId AttendancePage))

                PrenatalNextStepsNewbornEnrolmentOrigin _ encounterId ->
                    UserPage (PrenatalActivityPage encounterId Backend.PrenatalActivity.Model.NextSteps)

                AcuteIllnessContactsTracingActivityOrigin _ ->
                    -- Not in use, as at Acute Ilness patient is created
                    -- from a dedicated form.
                    PinCodePage
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text title ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage goBackPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewSearchForm : Language -> NominalDate -> Maybe VillageId -> Bool -> Initiator -> Maybe PersonId -> Model -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate maybeVillageId isChw initiator relation model db =
    let
        searchForm =
            Components.PatientsSearchForm.View.view language model
                |> Html.map Pages.People.Model.MsgPatientsSearchForm

        relatedPerson =
            Maybe.andThen (\id -> Dict.get id db.people) relation
                |> Maybe.andThen RemoteData.toMaybe

        expectedAge =
            relatedPerson
                |> Maybe.andThen (isPersonAnAdult currentDate)
                |> (\isAdult ->
                        case isAdult of
                            Just True ->
                                ExpectChild

                            Just False ->
                                ExpectAdult

                            Nothing ->
                                ExpectAdultOrChild
                   )

        searchValue =
            Components.PatientsSearchForm.Utils.getSearchValue model

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                let
                    -- When relation person is provided, we need to make sure
                    -- that at search result, we don't present:
                    -- 1. Relation person himself.
                    -- 2. People of same type as relation person. If relation person
                    --    is an adult, related person must be a child, and vice versa.
                    -- 3. People already related to relation person.
                    personTypeCondition filteredPerson =
                        case isPersonAnAdult currentDate filteredPerson of
                            Just True ->
                                -- We'll show adults unless we're expecting children.
                                expectedAge /= ExpectChild

                            Just False ->
                                -- We'll show children unless we're expecting adults.
                                expectedAge /= ExpectAdult && childAgeCondition filteredPerson

                            Nothing ->
                                -- If we don't know, then show it.
                                True

                    childAgeCondition filteredPerson =
                        case initiator of
                            GroupEncounterOrigin sessionId ->
                                Maybe.map2
                                    (\session birthDate ->
                                        if List.member session.clinicType [ Sorwathe, Achi ] then
                                            True

                                        else
                                            diffMonths birthDate currentDate < graduatingAgeInMonth
                                    )
                                    (getSession sessionId db)
                                    filteredPerson.birthDate
                                    |> Maybe.withDefault True

                            _ ->
                                True

                    personRelationCondition filteredPersonId =
                        case relation of
                            Nothing ->
                                True

                            Just personId ->
                                Dict.get personId db.relationshipsByPerson
                                    |> Maybe.andThen RemoteData.toMaybe
                                    |> unwrap
                                        True
                                        (\relatedPersionRelationships ->
                                            relatedPersionRelationships
                                                |> Dict.values
                                                |> List.all
                                                    (\relationship ->
                                                        relationship.relatedTo /= filteredPersonId
                                                    )
                                        )

                    -- For CHW nurse, we present people only from the village that was selected.
                    chwCondition filteredPerson =
                        if isChw then
                            maybeVillageId
                                |> Maybe.map (personLivesInVillage filteredPerson db)
                                |> Maybe.withDefault False

                        else
                            True
                in
                Components.PatientsSearchForm.Utils.getSearchResults db model
                    |> RemoteData.map
                        (Dict.filter
                            (\filteredPersonId filteredPerson ->
                                -- Applying conditions explained above.
                                (relation /= Just filteredPersonId)
                                    && personTypeCondition filteredPerson
                                    && personRelationCondition filteredPersonId
                                    && chwCondition filteredPerson
                            )
                        )
                    |> Just

        summary =
            Maybe.map (viewWebData language viewSummary identity) results
                |> Maybe.withDefault emptyNode

        viewSummary data =
            Dict.size data
                |> Translate.ReportResultsOfParticipantsSearch
                |> translate language
                |> text

        searchResultsParticipants =
            Maybe.withDefault (Success Dict.empty) results
                |> RemoteData.withDefault Dict.empty
                |> Dict.map (viewParticipant language currentDate initiator relation db)
                |> Dict.values

        searchHelper =
            case relation of
                Just _ ->
                    Translate.SearchHelperFamilyMember

                Nothing ->
                    Translate.SearchHelper
    in
    div [ class "registration-page search" ]
        [ div
            [ class "search-top" ]
            [ p
                [ class "search-helper" ]
                [ text <| translate language searchHelper ]
            , searchForm
            ]
        , div
            [ class "search-middle" ]
            [ div
                [ class "results-summary" ]
                [ summary ]
            , div
                [ class "ui unstackable items participants-list" ]
                searchResultsParticipants
            ]
        , div
            [ class "search-bottom" ]
            [ div
                [ class "register-helper" ]
                [ text <| translate language Translate.RegisterParticipantHelper ]
            , div
                [ class "register-actions" ]
                [ button
                    [ class "ui primary button fluid"
                    , onClick <| SetActivePage <| UserPage <| CreatePersonPage relation initiator
                    ]
                    [ text <| translate language Translate.RegisterNewParticipant ]
                ]
            ]
        ]


viewParticipant : Language -> NominalDate -> Initiator -> Maybe PersonId -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewParticipant language currentDate initiator relation db id person =
    let
        typeForThumbnail =
            defaultIconForPerson currentDate person

        nextPage =
            case relation of
                Just relationId ->
                    RelationshipPage relationId id initiator

                Nothing ->
                    PersonPage id initiator

        action =
            div [ class "action" ]
                [ showIf (isNothing relation) <|
                    span
                        [ class "patient-record"
                        , onClick <| SetActivePage <| UserPage <| PatientRecordPage InitiatorParticipantDirectory id
                        ]
                        []
                , div [ class "action-icon-wrapper blue" ]
                    [ span
                        [ class "action-icon forward"
                        , onClick <| SetActivePage <| UserPage <| nextPage
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
