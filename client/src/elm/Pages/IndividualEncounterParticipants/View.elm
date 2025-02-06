module Pages.IndividualEncounterParticipants.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (defaultIconForPerson, isChildUnderAgeOf2, isPersonAFertileWoman, isPersonAnAdult)
import Backend.Village.Utils exposing (personLivesInVillage)
import Components.PatientsSearchForm.Utils exposing (..)
import Components.PatientsSearchForm.View
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffYears)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.IndividualEncounterParticipants.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
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
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage IndividualEncounterTypesPage
            ]
            [ span [ class "icon-back" ] []
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
            Components.PatientsSearchForm.View.view language model
                |> Html.map Pages.IndividualEncounterParticipants.Model.MsgPatientsSearchForm

        searchValue =
            Components.PatientsSearchForm.Utils.getSearchValue model

        encounterCondition person =
            case encounterType of
                AcuteIllnessEncounter ->
                    True

                AntenatalEncounter ->
                    isPersonAFertileWoman currentDate person

                ChildScoreboardEncounter ->
                    isChw && isChildUnderAgeOf2 currentDate person

                HIVEncounter ->
                    isChw

                HomeVisitEncounter ->
                    -- We do not have direct access to Home Visit encounter.
                    False

                NCDEncounter ->
                    -- Patient is 12 years old or above.
                    Maybe.map (\birthDate -> diffYears birthDate currentDate >= 12)
                        person.birthDate
                        |> Maybe.withDefault False

                NutritionEncounter ->
                    isPersonAnAdult currentDate person
                        |> Maybe.map not
                        |> Maybe.withDefault False

                TuberculosisEncounter ->
                    isChw

                WellChildEncounter ->
                    isPersonAnAdult currentDate person
                        |> Maybe.map not
                        |> Maybe.withDefault False

                InmmunizationEncounter ->
                    -- Not in use (possibly future development).
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
                Components.PatientsSearchForm.Utils.getSearchResults db model
                    |> RemoteData.map
                        (Dict.filter
                            (\_ filteredPerson ->
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
                |> Translate.ReportResultsOfParticipantsSearch
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
                    [ onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage InitiatorParticipantsPage id ]

                AntenatalEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage InitiatorParticipantsPage id ]

                ChildScoreboardEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| ChildScoreboardParticipantPage id ]

                HIVEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| HIVParticipantPage id ]

                HomeVisitEncounter ->
                    -- We do not have direct access to Home Visit encounter.
                    []

                NCDEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| NCDParticipantPage InitiatorParticipantsPage id ]

                NutritionEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| NutritionParticipantPage InitiatorParticipantsPage id ]

                TuberculosisEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| TuberculosisParticipantPage id ]

                WellChildEncounter ->
                    [ onClick <| SetActivePage <| UserPage <| WellChildParticipantPage InitiatorParticipantsPage id ]

                InmmunizationEncounter ->
                    -- Not implemented (possibly future development).
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
                        , span [] [ text <| Maybe.withDefault "" person.village ]
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
