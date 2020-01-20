module Pages.People.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form exposing (ExpectedAge(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.People.Model exposing (..)
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
view : Language -> NominalDate -> Maybe PersonId -> Model -> ModelIndexedDb -> Html Msg
view language currentDate relation model db =
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
    in
    div
        [ class "page-people" ]
        [ viewHeader title
        , div
            [ class "search-wrapper" ]
            [ div
                [ class "ui full segment" ]
                [ viewSearchForm language currentDate relation model db ]
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


viewSearchForm : Language -> NominalDate -> Maybe PersonId -> Model -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate relation model db =
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

        relatedPerson =
            relation
                |> Maybe.andThen (\id -> Dict.get id db.people)
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
            model.search
                |> Maybe.withDefault ""

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
                                -- We'll show adults unless we're expecting children
                                expectedAge /= ExpectChild

                            Just False ->
                                -- We''ll show children unless we're expecting adults.
                                expectedAge /= ExpectAdult

                            Nothing ->
                                -- If we don't know, then show it.
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
                in
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.filter
                            (\k v ->
                                -- Applying 3 conditionms explained above
                                not (relation == Just k) && personTypeCondition v && personRelationCondition k
                            )
                        )
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
                |> Dict.map (viewParticipant language currentDate relation db)
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
                [ text <| translate language Translate.RegisterHelper ]
            , div
                [ class "register-actions" ]
                [ button
                    [ class "ui primary button fluid"
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
            case isPersonAnAdult currentDate person of
                Just True ->
                    "mother"

                Just False ->
                    "child"

                Nothing ->
                    "mother"

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
