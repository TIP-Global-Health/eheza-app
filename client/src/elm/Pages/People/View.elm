module Pages.People.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
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
view : Language -> NominalDate -> Maybe String -> Maybe PersonId -> ModelIndexedDb -> Html Msg
view language currentDate searchString relation db =
    let
        title =
            case relation of
                Just relationId ->
                    EveryDict.get relationId db.people
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map .name
                        |> RemoteData.withDefault (fromEntityUuid relationId)
                        |> (\name -> translate language (Translate.AddFamilyMemberFor name))

                Nothing ->
                    translate language Translate.People
    in
    div
        [ class "page-people" ]
        [ viewHeader title
        , div
            [ class "search-wrapper" ]
            [ div
                [ class "ui full segment blue" ]
                [ viewSearchForm language currentDate searchString relation db ]
            ]
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

        -- When related person is provided, we need to:
        -- 1. Make sure that we create a person of other type.
        --    If related person is an adult, created person must be a children
        --    and vice versa.
        relatedPersonConditions filteredPerson =
            case relation of
                Nothing ->
                    True

                Just personId ->
                    EveryDict.get personId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                        |> unwrap
                            True
                            (\relatedPerson ->
                                isPersonAnAdult currentDate filteredPerson == (not <| isPersonAnAdult currentDate relatedPerson)
                            )

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                -- If we're adding a family member, we filter out the person
                -- we're adding the famnily member to.
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (EveryDictList.filter
                            (\k v ->
                                not (relation == Just k) && relatedPersonConditions v
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
                |> EveryDictList.map (viewParticipant language currentDate relation db)
                |> EveryDictList.values

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
            if isPersonAnAdult currentDate person then
                "mother"

            else
                "child"

        nextPage =
            case relation of
                Just relationId ->
                    RelationshipPage relationId id

                Nothing ->
                    PersonPage id

        action =
            div [ class "action" ]
                [ div [ class "action-icon-wrapper" ]
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
