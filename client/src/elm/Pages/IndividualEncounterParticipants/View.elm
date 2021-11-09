module Pages.IndividualEncounterParticipants.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (ExpectedAge(..), Initiator(..), Person)
import Backend.Person.Utils exposing (ageInYears, defaultIconForPerson, isNewborn, isPersonAFertileWoman, isPersonAnAdult)
import Backend.Village.Utils exposing (personLivesInVillage)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.IndividualEncounterParticipants.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
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
view : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> IndividualEncounterType -> Model -> ModelIndexedDb -> Html Msg
view language currentDate ( healthCenterId, maybeVillageId ) isChw encounterType model db =
    let
        title =
            translate language Translate.SearchExistingParticipants
    in
    div
        [ class "wrap wrap-alt-2 page-participants" ]
        [ viewHeader title
        , viewBody language currentDate ( healthCenterId, maybeVillageId ) isChw encounterType model db
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


viewBody : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> IndividualEncounterType -> Model -> ModelIndexedDb -> Html Msg
viewBody language currentDate ( healthCenterId, maybeVillageId ) isChw encounterType model db =
    div
        [ class "search-wrapper" ]
        [ div
            [ class "ui full segment" ]
            [ viewSearchForm language currentDate ( healthCenterId, maybeVillageId ) isChw encounterType model db ]
        ]


viewSearchForm : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> IndividualEncounterType -> Model -> ModelIndexedDb -> Html Msg
viewSearchForm language currentDate ( healthCenterId, maybeVillageId ) isChw encounterType model db =
    let
        searchForm =
            Pages.Utils.viewSearchForm language model.input Translate.PlaceholderEnterParticipantName SetInput

        searchValue =
            model.search
                |> Maybe.withDefault ""

        encounterCondition person =
            case encounterType of
                AcuteIllnessEncounter ->
                    True

                AntenatalEncounter ->
                    isPersonAFertileWoman currentDate person

                NutritionEncounter ->
                    isPersonAnAdult currentDate person
                        |> Maybe.map not
                        |> Maybe.withDefault False

                WellChildEncounter ->
                    if isChw then
                        -- CHW can run only Newborn exam, which is
                        -- performed for children up to 2 months old.
                        isNewborn currentDate person
                            |> Maybe.withDefault False

                    else
                        isPersonAnAdult currentDate person
                            |> Maybe.map not
                            |> Maybe.withDefault False

                _ ->
                    False

        -- For CHW nurse, we present people only from the village that was selected.
        chwCondition person =
            if isChw then
                maybeVillageId
                    |> Maybe.map (personLivesInVillage person db)
                    |> Maybe.withDefault False

            else
                True

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.filter
                            (\filteredPersonId filteredPerson ->
                                -- Show only participants that belong to selected health center.
                                -- Todo: check if this really required.
                                (filteredPerson.healthCenterId == Just healthCenterId)
                                    && encounterCondition filteredPerson
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
                [ text <| translate language Translate.RegisterParticipantHelper ]
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
                AcuteIllnessEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage id ]

                AntenatalEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage id ]

                NutritionEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| NutritionParticipantPage id ]

                WellChildEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| WellChildParticipantPage id ]

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

        defaultIcon =
            defaultIconForPerson currentDate person
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage defaultIcon person.avatarUrl person.name 120 120 ]
        , viewContent
        ]
