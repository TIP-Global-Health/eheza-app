module Pages.IndividualEncounterParticipants.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (ExpectedAge(..), Person, RegistrationInitiator(..))
import Backend.Person.Utils exposing (ageInYears, isPersonAFertileWoman, isPersonAnAdult)
import Backend.SyncData.Model
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.IndividualEncounterParticipants.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
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
view : Language -> NominalDate -> HealthCenterId -> IndividualEncounterType -> Model -> ModelIndexedDb -> Html Msg
view language currentDate healthCenterId encounterType model db =
    let
        title =
            translate language Translate.SearchExistingParticipants
    in
    div
        [ class "wrap wrap-alt-2 page-prenatal-participants" ]
        [ viewHeader title
        , viewBody language currentDate healthCenterId encounterType model db
        ]


viewHeader : String -> Html Msg
viewHeader title =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text title ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage IndividualEncounterTypesPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewBody : Language -> NominalDate -> HealthCenterId -> IndividualEncounterType -> Model -> ModelIndexedDb -> Html Msg
viewBody language currentDate selectedHealthCenterId encounterType model db =
    let
        sync =
            db.syncData |> RemoteData.withDefault Dict.empty

        showWarningMessage header message =
            div [ class "ui basic segment" ]
                [ div
                    [ class "ui message warning" ]
                    [ div [ class "header" ] [ text <| translate language header ]
                    , text <| translate language message
                    ]
                ]
    in
    Dict.get selectedHealthCenterId sync
        |> unwrap
            (showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync)
            (\selectedHealthCenterSyncData ->
                let
                    isDownloading =
                        case selectedHealthCenterSyncData.attempt of
                            Backend.SyncData.Model.Downloading _ _ ->
                                True

                            _ ->
                                False

                    isUploading =
                        case selectedHealthCenterSyncData.attempt of
                            Backend.SyncData.Model.Uploading _ ->
                                True

                            _ ->
                                False
                in
                if isDownloading then
                    showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCDownloading

                else if isUploading then
                    showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCUploading

                else
                    div
                        [ class "search-wrapper" ]
                        [ div
                            [ class "ui full segment" ]
                            [ viewSearchForm language currentDate selectedHealthCenterId encounterType model db ]
                        ]
            )


viewSearchForm : Language -> NominalDate -> HealthCenterId -> IndividualEncounterType -> Model -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate selectedHealthCenterId encounterType model db =
    let
        searchForm =
            Html.form []
                [ div
                    [ class "ui search form" ]
                    [ div []
                        [ input
                            [ placeholder <| translate language Translate.PlaceholderEnterParticipantName
                            , type_ "text"
                            , class "search-input"
                            , onInput SetInput
                            , value model.input
                            , autofocus True
                            ]
                            []
                        ]
                    ]
                ]

        searchValue =
            model.search
                |> Maybe.withDefault ""

        participantsFilter id person =
            -- Show only participants that belong to selected health center
            (person.healthCenterId == Just selectedHealthCenterId)
                && (case encounterType of
                        AntenatalEncounter ->
                            isPersonAFertileWoman currentDate person

                        NutritionEncounter ->
                            isPersonAnAdult currentDate person
                                |> Maybe.map not
                                |> Maybe.withDefault False

                        _ ->
                            False
                   )

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.filter participantsFilter)
                    |> Just

        summary =
            results
                |> Maybe.map (viewWebData language viewSummary identity)
                |> Maybe.withDefault emptyNode

        viewSummary data =
            Dict.size data
                |> Translate.ReportResultsOfSearch
                |> translate language
                |> text

        searchResultsParticipants =
            results
                |> Maybe.withDefault (Success Dict.empty)
                |> RemoteData.withDefault Dict.empty
                |> Dict.map (viewParticipant language currentDate encounterType db)
                |> Dict.values

        searchHelper =
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
                [ text <| translate language Translate.RegisterHelper ]
            , div
                [ class "register-actions" ]
                [ button
                    [ class "ui primary button fluid"
                    , onClick <| SetActivePage <| UserPage <| CreatePersonPage Nothing (IndividualEncounterOrigin encounterType)
                    ]
                    [ text <| translate language Translate.RegisterNewParticipant ]
                ]
            ]
        ]


viewParticipant : Language -> NominalDate -> IndividualEncounterType -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewParticipant language currentDate encounterType db id person =
    let
        action =
            case encounterType of
                AntenatalEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage id ]

                NutritionEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| NutritionParticipantPage id ]

                _ ->
                    []

        viewAction =
            div [ class "action" ]
                [ div [ class "action-icon-wrapper" ]
                    [ span
                        (class "action-icon forward" :: action)
                        []
                    ]
                ]

        viewContent =
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
                , viewAction
                ]
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage "mother" person.avatarUrl person.name 120 120 ]
        , viewContent
        ]
