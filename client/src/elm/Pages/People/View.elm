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
-}
view : Language -> NominalDate -> Maybe String -> ModelIndexedDb -> Html Msg
view language currentDate searchString db =
    div
        [ class "wrap wrap-alt-2" ]
        [ viewHeader language
        , div
            [ class "ui full segment blue" ]
            [ viewSearchForm language currentDate searchString db
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


viewSearchForm : Language -> NominalDate -> Maybe String -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate searchString db =
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
                UserPage <| PersonsPage Nothing

            else
                UserPage <| PersonsPage <| Just trimmed

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
                |> EveryDictList.map (viewParticipant language currentDate db)
                |> EveryDictList.values
    in
    div [ class "wrap-list registration-page search" ]
        [ h3
            [ class "ui header" ]
            [ text <| translate language Translate.ParticipantInformation ++ ": " ]
        , span
            [ class "search-helper" ]
            [ text <| translate language Translate.SearchHelper ]
        , h3
            [ class "ui header" ]
            [ text <| translate language Translate.ParticipantDirectory ++ ": " ]
        , searchForm
        , div
            [ class "results-summary" ]
            [ summary ]
        , div
            [ class "ui unstackable items participants-list" ]
            searchResultsParticipants
        , div
            [ class "register-helper" ]
            [ text <| translate language Translate.RegisterHelper ]
        , div
            [ class "actions" ]
            [ button
                [ class "ui primary button"
                , onClick <| SetActivePage <| UserPage <| CreatePersonPage
                ]
                [ text <| translate language Translate.RegisterNewParticipant ]
            ]
        ]


viewParticipant : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewParticipant language currentDate db id person =
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
                        , onClick <| SetActivePage <| UserPage <| PersonPage id
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
