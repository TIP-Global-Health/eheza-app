module Pages.FamilyEncounter.Participant.View exposing (view)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (defaultIconForPerson, isPersonAnAdult)
import Backend.Village.Utils exposing (personLivesInVillage)
import Components.PatientsSearchForm.Utils
import Components.PatientsSearchForm.View
import GeoLocation.Model exposing (ReverseGeoInfo)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.FamilyEncounter.Participant.Model exposing (Model, Msg(..), RegistrationMode(..), MotherSelection(..), maxChildren)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.View
import Pages.Utils exposing (viewLabel)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> Site
    -> ReverseGeoInfo
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> IndividualParticipantInitiator
    -> Model
    -> ModelIndexedDb
    -> Html Msg
view language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw initiator model db =
    div
        [ class "wrap wrap-alt-2 page-participant individual family" ]
        [ viewHeader language isChw initiator
        , div
            [ class "ui full segment" ]
            [ viewContent language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw model db ]
        ]


viewHeader : Language -> Bool -> IndividualParticipantInitiator -> Html Msg
viewHeader language isChw initiator =
    let
        goBackPage =
            case initiator of
                Backend.IndividualEncounterParticipant.Model.InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.FamilyEncounter

                Backend.IndividualEncounterParticipant.Model.InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.FamilyEncounter isChw ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent :
    Language
    -> NominalDate
    -> Site
    -> ReverseGeoInfo
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewContent language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw model db =
    case model.registrationMode of
        SearchMode ->
            viewSearchMode language currentDate selectedHealthCenter maybeVillageId isChw model db

        RegisterMotherMode ->
            viewRegisterMotherMode language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw model db

        RegisterChildMode ->
            viewRegisterChildMode language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw model db


viewSearchMode :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewSearchMode language currentDate selectedHealthCenter maybeVillageId isChw model db =
    div [ class "family-encounter-search" ]
        [ viewSelectedMother language currentDate model db
        , viewSelectedChildren language currentDate model db
        , viewMotherSearch language currentDate selectedHealthCenter maybeVillageId isChw model db
        , viewStartEncounterButton language model
        ]


viewSelectedMother : Language -> NominalDate -> Model -> ModelIndexedDb -> Html Msg
viewSelectedMother language currentDate model db =
    case model.selectedMother of
        Just (ExistingMother personId person) ->
            div [ class "selected-mother section" ]
                [ h3 [] [ text <| translate language Translate.SelectedMother ]
                , viewPersonCard language currentDate personId person True
                ]

        Just NewMother ->
            div [ class "selected-mother section" ]
                [ h3 [] [ text <| translate language Translate.SelectedMother ]
                , div [ class "person-card new" ]
                    [ text <| translate language Translate.RegisteringNewMother ]
                ]

        Nothing ->
            emptyNode


viewSelectedChildren : Language -> NominalDate -> Model -> ModelIndexedDb -> Html Msg
viewSelectedChildren language currentDate model db =
    if List.isEmpty model.selectedChildren then
        emptyNode

    else
        let
            childCards =
                model.selectedChildren
                    |> List.filterMap
                        (\childId ->
                            Dict.get childId db.people
                                |> Maybe.andThen RemoteData.toMaybe
                                |> Maybe.map (\child -> viewPersonCard language currentDate childId child False)
                        )
        in
        div [ class "selected-children section" ]
            [ h3 [] [ text <| translate language Translate.SelectedChildren ]
            , div [ class "children-list" ] childCards
            ]


viewPersonCard : Language -> NominalDate -> PersonId -> Person -> Bool -> Html Msg
viewPersonCard language currentDate personId person isMother =
    let
        defaultIcon =
            defaultIconForPerson currentDate person

        removeAction =
            if isMother then
                onClick DeselectMother

            else
                onClick (RemoveChild personId)
    in
    div [ class "person-card" ]
        [ div [ class "person-image" ]
            [ thumbnailImage defaultIcon person.photo person.name 110 110 ]
        , div [ class "person-details" ]
            [ h4 [] [ text person.name ]
            , p []
                [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                , person.birthDate
                    |> Maybe.map (renderDate language >> text)
                    |> showMaybe
                ]
            , p []
                [ label [] [ text <| translate language Translate.Village ++ ": " ]
                , text <| Maybe.withDefault "" person.village
                ]
            ]
        , button
            [ class "ui button basic"
            , removeAction
            ]
            [ text <| translate language Translate.Remove ]
        ]


viewMotherSearch :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewMotherSearch language currentDate selectedHealthCenter maybeVillageId isChw model db =
    let
        searchForm =
            Components.PatientsSearchForm.View.view language model.searchForm
                |> Html.map MsgPatientsSearchForm

        searchValue =
            Components.PatientsSearchForm.Utils.getSearchValue model.searchForm

        motherSelected =
            model.selectedMother /= Nothing

        chwCondition person =
            if isChw then
                maybeVillageId
                    |> Maybe.map (personLivesInVillage person db)
                    |> Maybe.withDefault False

            else
                True

        adultCondition person =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Components.PatientsSearchForm.Utils.getSearchResults db model.searchForm
                    |> RemoteData.map
                        (Dict.filter
                            (\_ filteredPerson ->
                                (filteredPerson.healthCenterId == Just selectedHealthCenter)
                                    && adultCondition filteredPerson
                                    && chwCondition filteredPerson
                            )
                        )
                    |> Just

        summary =
            results
                |> Maybe.map (viewWebData language viewSummary identity)
                |> Maybe.withDefault emptyNode

        viewSummary data =
            Dict.size data
                |> Translate.ReportResultsOfParticipantsSearch
                |> translate language
                |> text

        searchResultsParticipants =
            results
                |> Maybe.withDefault (Success Dict.empty)
                |> RemoteData.withDefault Dict.empty
                |> Dict.map (viewSearchResultPerson language currentDate db)
                |> Dict.values
    in
    if motherSelected then
        div [ class "mother-search section" ]
            [ h3 [] [ text <| translate language Translate.AddChildren ]
            , p [ class "helper-text" ]
                [ text <| translate language Translate.AddChildrenHelper ]
            , div [ class "search-actions" ]
                [ button
                    [ class "ui primary button"
                    , onClick RegisterNewChild
                    , disabled (List.length model.selectedChildren >= maxChildren)
                    ]
                    [ text <| translate language Translate.RegisterNewChild ]
                ]
            ]

    else
        div [ class "mother-search section registration-page search" ]
            [ h3 [] [ text <| translate language Translate.SearchForMother ]
            , div [ class "search-top" ]
                [ p [ class "search-helper" ]
                    [ text <| translate language Translate.SearchHelper ]
                , searchForm
                ]
            , div [ class "search-middle" ]
                [ div [ class "results-summary" ]
                    [ summary ]
                , div [ class "ui unstackable items participants-list" ]
                    searchResultsParticipants
                ]
            , div [ class "search-bottom" ]
                [ div [ class "register-helper" ]
                    [ text <| translate language Translate.RegisterMotherHelper ]
                , div [ class "register-actions" ]
                    [ button
                        [ class "ui primary button fluid"
                        , onClick RegisterNewMother
                        ]
                        [ text <| translate language Translate.RegisterNewMother ]
                    ]
                ]
            ]


viewSearchResultPerson : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewSearchResultPerson language currentDate db id person =
    let
        action =
            onClick (SelectMother id person)

        viewAction =
            div [ class "action" ]
                [ div [ class "action-icon-wrapper" ]
                    [ span
                        [ class "action-icon forward"
                        , action
                        ]
                        []
                    ]
                ]

        viewContent =
            div [ class "content" ]
                [ div [ class "details" ]
                    [ h2 [ class "ui header" ]
                        [ text person.name ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , person.birthDate
                            |> Maybe.map (renderDate language >> text)
                            |> showMaybe
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , text <| Maybe.withDefault "" person.village
                        ]
                    ]
                , viewAction
                ]

        defaultIcon =
            defaultIconForPerson currentDate person
    in
    div [ class "item participant-view" ]
        [ div [ class "ui image" ]
            [ thumbnailImage defaultIcon person.photo person.name 120 120 ]
        , viewContent
        ]


viewRegisterMotherMode :
    Language
    -> NominalDate
    -> Site
    -> ReverseGeoInfo
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewRegisterMotherMode language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw model db =
    case model.motherForm of
        Just formData ->
            div [ class "registration-form" ]
                [ h3 [] [ text <| translate language Translate.RegisterNewMother ]
                , Pages.Person.View.viewCreateEditForm
                    language
                    currentDate
                    site
                    reverseGeoInfo
                    selectedHealthCenter
                    maybeVillageId
                    isChw
                    (Backend.Person.Model.CreatePerson Nothing)
                    (Backend.Person.Model.IndividualEncounterOrigin Backend.IndividualEncounterParticipant.Model.FamilyEncounter)
                    formData.form
                    formData.dateSelectorPopupState
                    Nothing
                    db
                    MsgMotherForm
                    DropZoneCompleteMotherPhoto
                    DateSelectedMother
                    SetMotherDateSelectorState
                , button
                    [ class "ui button basic"
                    , onClick BackToSearch
                    ]
                    [ text <| translate language Translate.Cancel ]
                ]

        Nothing ->
            emptyNode


viewRegisterChildMode :
    Language
    -> NominalDate
    -> Site
    -> ReverseGeoInfo
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewRegisterChildMode language currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw model db =
    case model.childForm of
        Just formData ->
            div [ class "registration-form" ]
                [ h3 [] [ text <| translate language Translate.RegisterNewChild ]
                , Pages.Person.View.viewCreateEditForm
                    language
                    currentDate
                    site
                    reverseGeoInfo
                    selectedHealthCenter
                    maybeVillageId
                    isChw
                    (Backend.Person.Model.CreatePerson Nothing)
                    (Backend.Person.Model.IndividualEncounterOrigin Backend.IndividualEncounterParticipant.Model.FamilyEncounter)
                    formData.form
                    formData.dateSelectorPopupState
                    Nothing
                    db
                    MsgChildForm
                    DropZoneCompleteChildPhoto
                    DateSelectedChild
                    SetChildDateSelectorState
                , button
                    [ class "ui button basic"
                    , onClick BackToSearch
                    ]
                    [ text <| translate language Translate.Cancel ]
                ]

        Nothing ->
            emptyNode


viewStartEncounterButton : Language -> Model -> Html Msg
viewStartEncounterButton language model =
    let
        canStart =
            case model.selectedMother of
                Just (ExistingMother _ _) ->
                    True

                _ ->
                    False
    in
    if canStart then
        div [ class "actions" ]
            [ button
                [ class "ui primary button fluid"
                , onClick CreateEncounter
                ]
                [ text <| translate language Translate.StartEncounter ]
            ]

    else
        emptyNode
