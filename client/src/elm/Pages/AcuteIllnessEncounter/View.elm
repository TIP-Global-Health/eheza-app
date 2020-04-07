module Pages.AcuteIllnessEncounter.View exposing (view)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AcuteIllnessActivity.Utils exposing (getActivityIcon, getAllActivities)
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.AcuteIllnessEncounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        encounter =
            Dict.get id db.acuteIllnessEncounters
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        measurements =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked

        personWithMeasurements =
            RemoteData.map (\a b -> ( a, b )) person
                |> RemoteData.andMap measurements

        header =
            viewWebData language (viewHeader language) identity participant

        content =
            viewWebData language (viewContent language currentDate id model) identity personWithMeasurements
    in
    div [ class "page-encounter acute-illness" ] <|
        [ header
        , content
        ]


viewHeader : Language -> IndividualEncounterParticipant -> Html Msg
viewHeader language participant =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Model -> ( Person, AcuteIllnessMeasurements ) -> Html Msg
viewContent language currentDate id model ( person, measurements ) =
    ((viewPersonDetails language currentDate person |> div [ class "item" ])
        :: viewMainPageContent language currentDate id measurements model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessMeasurements -> Model -> List (Html Msg)
viewMainPageContent language currentDate id measurements model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities
                |> List.partition
                    (\activity ->
                        case activity of
                            AcuteIllnessSymptoms ->
                                isJust measurements.symptomsGeneral
                                    && isJust measurements.symptomsRespiratory
                                    && isJust measurements.symptomsGI

                            AcuteIllnessPhysicalExam ->
                                isJust measurements.vitals

                            AcuteIllnessLaboratory ->
                                False

                            AcuteIllnessExposure ->
                                False
                    )

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        viewCard activity =
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetActivePage <| UserPage <| AcuteIllnessActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.AcuteIllnessActivityTitle activity
                            |> translate language
                            |> String.toUpper
                            |> text
                        ]
                    ]
                ]

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

        innerContent =
            div [ class "full content" ]
                [ div [ class "wrap-cards" ]
                    [ div [ class "ui four cards" ] <|
                        if List.isEmpty selectedActivities then
                            [ span [] [ text emptySectionMessage ] ]

                        else
                            List.map viewCard selectedActivities
                    ]
                ]

        allowEndEcounter =
            List.isEmpty pendingActivities

        endEcounterButtonAttributes =
            if allowEndEcounter then
                [ class "ui fluid primary button"
                , onClick <| CloseEncounter id
                ]

            else
                [ class "ui fluid primary button disabled" ]

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , div [ class "actions" ]
                    [ button
                        endEcounterButtonAttributes
                        [ text <| translate language Translate.EndEncounter ]
                    ]
                ]
    in
    [ tabs
    , content
    ]
