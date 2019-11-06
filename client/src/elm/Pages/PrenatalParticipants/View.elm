module Pages.PrenatalParticipants.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form exposing (ExpectedAge(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAFertileWoman)
import Dict
import EveryDict
import EveryDictList
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalParticipants.Model exposing (..)
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
view : Language -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language currentDate healthCenterId model db =
    let
        title =
            translate language Translate.PrenatalParticipants
    in
    div
        [ class "wrap wrap-alt-2 page-prenatal-participants" ]
        [ viewHeader title
        , viewContent language currentDate healthCenterId model db
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
            , onClick <| SetActivePage <| UserPage ClinicalPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
viewContent language currentDate selectedHealthCenterId model db =
    let
        sync =
            db.syncData |> RemoteData.withDefault EveryDictList.empty

        showWarningMessage header message =
            div [ class "ui basic segment" ]
                [ div
                    [ class "ui message warning" ]
                    [ div [ class "header" ] [ text <| translate language header ]
                    , text <| translate language message
                    ]
                ]
    in
    EveryDictList.get selectedHealthCenterId sync
        |> unwrap
            (showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync)
            (\selectedHealthCenterSyncData ->
                let
                    isDownloading =
                        selectedHealthCenterSyncData.downloadStatus
                            |> Maybe.map (\status -> status.remaining > 0)
                            |> Maybe.withDefault True

                    isUploading =
                        selectedHealthCenterSyncData.uploadStatus
                            |> Maybe.map (\status -> status.remaining > 0)
                            |> Maybe.withDefault False
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
                            [ viewSearchForm language currentDate selectedHealthCenterId model db ]
                        ]
            )


viewSearchForm : Language -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate selectedHealthCenterId model db =
    let
        searchForm =
            Html.form []
                [ div
                    [ class "ui search form" ]
                    [ div []
                        [ input
                            [ placeholder <| translate language Translate.PlaceholderEnterParticipantName
                            , type_ "text"
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

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (EveryDictList.filter
                            (\_ person ->
                                (isPersonAFertileWoman currentDate person |> Maybe.withDefault False)
                                    -- Show only mothers that belong to selected health center
                                    && (person.healthCenterId == Just selectedHealthCenterId)
                            )
                        )
                    |> Just

        summary =
            results
                |> Maybe.map (viewWebData language viewSummary identity)
                |> Maybe.withDefault emptyNode

        viewSummary data =
            EveryDictList.length data
                |> Translate.ReportResultsOfSearch
                |> translate language
                |> text

        searchResultsParticipants =
            results
                |> Maybe.withDefault (Success EveryDictList.empty)
                |> RemoteData.withDefault EveryDictList.empty
                |> EveryDictList.map (viewParticipant language currentDate db)
                |> EveryDictList.values

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
        ]


viewParticipant : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewParticipant language currentDate db id person =
    let
        action =
            div [ class "action" ]
                [ div [ class "action-icon-wrapper" ]
                    [ span
                        [ class "action-icon forward"
                        , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage id
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
            [ thumbnailImage "mother" person.avatarUrl person.name 120 120 ]
        , content
        ]
