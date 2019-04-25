module Pages.People.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Dict
import EveryDictList
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
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
view : Language -> NominalDate -> Maybe String -> Maybe PersonId -> ModelIndexedDb -> Html Msg
view language currentDate searchString relation db =
    div
        [ class "page-people" ]
        [ viewHeader language
        , div
            [ class "search-wrapper" ]
            [ div
                [ class "ui full segment blue" ]
                [ viewSearchForm language currentDate searchString relation db ]
            ]
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
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


viewSearchForm : Language -> NominalDate -> Maybe String -> Maybe PersonId -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate searchString relation db =
    let
        searchValue =
            searchString
                |> Maybe.map String.trim
                |> Maybe.withDefault ""

        -- Calculates the page which corresponds to what the user types.
        getPage typing =
            let
                trimmed =
                    String.trim typing
            in
            if String.isEmpty trimmed then
                UserPage <| PersonsPage Nothing relation

            else
                UserPage <| PersonsPage (Just trimmed) relation

        searchForm =
            Html.form []
                [ div
                    [ class "ui search form" ]
                    [ div []
                        [ input
                            [ placeholder <| translate language Translate.PlaceholderEnterParticipantName
                            , type_ "text"
                            , onInput <| SetActivePage << getPage
                            , value searchValue
                            , autofocus True
                            ]
                            []
                        ]
                    ]
                ]

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
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
                |> EveryDictList.map (viewParticipant language currentDate relation db)
                |> EveryDictList.values
    in
    div [ class "registration-page search" ]
        [ div
            [ class "search-top" ]
            [ p
                [ class "search-helper" ]
                [ text <| translate language Translate.SearchHelper ]
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
                [ class "actions" ]
                [ button
                    [ class "ui primary button"
                    , onClick <| SetActivePage <| UserPage <| CreatePersonPage relation
                    ]
                    [ text <| translate language Translate.RegisterNewParticipant ]
                ]
            ]
        ]


viewParticipant : Language -> NominalDate -> Maybe PersonId -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewParticipant language currentDate relation db id person =
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

        action =
            div [ class "action" ]
                [ div [ class "action-icon-wrapper" ]
                    [ span
                        [ class "action-icon forward"
                        , onClick <| SetActivePage <| UserPage <| PersonPage id relation
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
