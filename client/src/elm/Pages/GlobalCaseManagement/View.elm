module Pages.GlobalCaseManagement.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements, NutritionAssesment(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Date exposing (Month, Unit(..), isBetween, numberToMonth)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> Bool -> Model -> ModelIndexedDb -> Html Msg
view language currentDate healthCenterId isChw model db =
    let
        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.CaseManagement ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage PinCodePage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]

        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.withDefault NotAsked

        content =
            viewWebData language (viewContent language currentDate healthCenterId isChw model db) identity followUps
    in
    div [ class "wrap wrap-alt-2 page-case-management" ]
        [ header
        , content
        , viewModal <|
            viewStartFollowUpEncounterDialog language
                model.dialogState
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> Bool -> Model -> ModelIndexedDb -> FollowUpMeasurements -> Html Msg
viewContent language currentDate healthCenterId isChw model db followUps =
    let
        nutritionFollowUps =
            generateNutritionFollowUps currentDate healthCenterId followUps

        acuteIllnessFollowUps =
            Dict.empty

        panes =
            [ ( AcuteIllnessEncounter, acuteIllnessFollowUps ), ( HomeVisitEncounter, nutritionFollowUps ) ]
                |> List.filterMap
                    (\( type_, followUps_ ) ->
                        if isNothing model.encounterTypeFilter || model.encounterTypeFilter == Just type_ then
                            Just <| viewEncounterTypePane language currentDate type_ followUps_ db model

                        else
                            Nothing
                    )
    in
    div [ class "ui unstackable items" ] <|
        viewFilters language model
            :: panes


viewStartFollowUpEncounterDialog : Language -> Maybe FollowUpEncounterData -> Maybe (Html Msg)
viewStartFollowUpEncounterDialog language dialogState =
    dialogState
        |> Maybe.map
            (\data ->
                div [ class "ui tiny active modal" ]
                    [ div
                        [ class "content" ]
                        [ text <| translate language <| Translate.EncounterTypeFollowUpQuestion data.encounterType
                        , text " "
                        , span [ class "person-name " ] [ text data.personName ]
                        , text "?"
                        ]
                    , div
                        [ class "actions" ]
                        [ div [ class "two ui buttons" ]
                            [ button
                                [ class "ui primary fluid button"
                                , onClick <| StartFollowUpEncounter data
                                ]
                                [ text <| translate language Translate.Yes ]
                            , button
                                [ class "ui fluid button"
                                , onClick <| SetDialogState Nothing
                                ]
                                [ text <| translate language Translate.No ]
                            ]
                        ]
                    ]
            )


viewFilters : Language -> Model -> Html Msg
viewFilters language model =
    let
        filters =
            allEncounterTypes
                |> List.map Just
                |> List.append [ Nothing ]

        renderButton maybeFilter =
            let
                label =
                    Maybe.map Translate.EncounterTypeFileterLabel maybeFilter
                        |> Maybe.withDefault Translate.All
            in
            button
                [ classList
                    [ ( "active", model.encounterTypeFilter == maybeFilter )
                    , ( "primary ui button", True )
                    ]
                , onClick <| SetEncounterTypeFilter maybeFilter
                ]
                [ translateText language label ]
    in
    div [ class "ui segment filters" ] <|
        List.map renderButton filters


viewEncounterTypePane : Language -> NominalDate -> IndividualEncounterType -> Dict PersonId FollowUpItem -> ModelIndexedDb -> Model -> Html Msg
viewEncounterTypePane language currentDate encounterType itemsDict db model =
    let
        content =
            if Dict.isEmpty itemsDict then
                [ translateText language Translate.NoMatchesFound ]

            else
                Dict.map (viewFollowUpItem language currentDate db) itemsDict
                    |> Dict.values
    in
    div [ class "pane" ]
        [ viewItemHeading language encounterType
        , div [ class "pane-content" ]
            content
        ]


viewItemHeading : Language -> IndividualEncounterType -> Html Msg
viewItemHeading language encounterType =
    div [ class "pane-heading" ]
        [ text <| translate language <| Translate.EncounterTypeFollowUpLabel encounterType ]


viewFollowUpItem : Language -> NominalDate -> ModelIndexedDb -> PersonId -> FollowUpItem -> Html Msg
viewFollowUpItem language currentDate db personId item =
    let
        lastHomeVisitEncounter =
            resolveIndividualParticipantForPerson personId HomeVisitEncounter db
                |> Maybe.map
                    (\participantId ->
                        Dict.get participantId db.homeVisitEncountersByParticipant
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map Dict.values
                            |> Maybe.withDefault []
                    )
                |> Maybe.withDefault []
                -- Sort DESC
                |> List.sortWith (\e1 e2 -> Date.compare e2.startDate e1.startDate)
                |> List.head
    in
    lastHomeVisitEncounter
        |> Maybe.map
            (\encounter ->
                -- Last Home Visitit encounter occurred before follow up was scheduled.
                if Date.compare encounter.startDate item.dateMeasured == LT then
                    viewFollowUpEntry language currentDate db personId item

                else
                    emptyNode
            )
        |> -- No Home Visitit encounter found.
           Maybe.withDefault (viewFollowUpEntry language currentDate db personId item)


viewFollowUpEntry : Language -> NominalDate -> ModelIndexedDb -> PersonId -> FollowUpItem -> Html Msg
viewFollowUpEntry language currentDate db personId item =
    Dict.get personId db.people
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\person ->
                let
                    dueOption =
                        followUpDueOptionByDate currentDate item.dateMeasured item.value

                    dueLabel =
                        Translate.FollowUpDueOption dueOption
                            |> translateText language

                    dueClass =
                        "due "
                            ++ (case dueOption of
                                    OverDue ->
                                        "overdue"

                                    DueToday ->
                                        "today"

                                    DueThisWeek ->
                                        "this-week"

                                    DueThisMonth ->
                                        "this-month"
                               )

                    assessments =
                        EverySet.toList item.value.assesment
                            |> List.reverse
                            |> List.map (\assessment -> p [] [ translateAssement assessment ])

                    translateAssement assessment =
                        case assessment of
                            AssesmentMalnutritionSigns signs ->
                                let
                                    translatedSigns =
                                        List.map (Translate.ChildNutritionSignLabel >> translate language) signs
                                            |> String.join ", "
                                in
                                text <| translate language (Translate.NutritionAssesment assessment) ++ ": " ++ translatedSigns

                            _ ->
                                text <| translate language <| Translate.NutritionAssesment assessment
                in
                div [ class "follow-up-entry" ]
                    [ div [ class "name" ] [ text person.name ]
                    , div [ class dueClass ] [ dueLabel ]
                    , div [ class "assesment" ] assessments
                    , div
                        [ class "icon-forward"
                        , onClick <| SetDialogState <| Just <| FollowUpEncounterData HomeVisitEncounter personId person.name
                        ]
                        []
                    ]
            )
        |> Maybe.withDefault emptyNode
