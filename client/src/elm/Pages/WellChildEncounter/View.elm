module Pages.WellChildEncounter.View exposing (view, viewPersonDetails)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (WellChildMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.WellChildEncounter.Model exposing (..)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    let
        assembeld =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id isChw db model) identity assembeld


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id isChw db model assembeld =
    let
        header =
            viewHeader language isChw assembeld

        content =
            viewContent language currentDate zscores id isChw db model assembeld
    in
    div [ class "page-encounter well-child" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                assembeld.participant.person
                id
                model.warningPopupState
        ]


viewHeader : Language -> Bool -> AssembledData -> Html Msg
viewHeader language isChw assembeld =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| WellChildParticipantPage assembeld.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw db model assembeld =
    ((viewPersonDetails language currentDate assembeld.person |> div [ class "item" ])
        :: viewMainPageContent language currentDate zscores id isChw db assembeld model
    )
        |> div [ class "ui unstackable items" ]


warningPopup : Language -> PersonId -> WellChildEncounterId -> Maybe WarningPopupType -> Maybe (Html Msg)
warningPopup language childId encounterId warningPopupState =
    warningPopupState
        |> Maybe.map
            (\popupType ->
                let
                    warningHeading =
                        [ img [ src "assets/images/exclamation-red.png" ] []
                        , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                        ]

                    actions =
                        case popupType of
                            PopupDangerSigns ->
                                div [ class "two ui buttons" ]
                                    [ button
                                        [ class "ui fluid button"
                                        , onClick <| SetWarningPopupState Nothing
                                        ]
                                        [ text <| translate language Translate.Cancel ]
                                    , button
                                        [ class "ui primary fluid button"
                                        , onClick <| TriggerAcuteIllnessEncounter childId encounterId
                                        ]
                                        [ text <| translate language Translate.CloseAndContinue ]
                                    ]

                            PopupECD _ ->
                                button
                                    [ class "ui fluid button"
                                    , onClick <| SetWarningPopupState Nothing
                                    ]
                                    [ text <| translate language Translate.Continue ]
                in
                div [ class "ui active modal danger-signs-popup" ]
                    [ div [ class "content" ]
                        [ div [ class "popup-heading-wrapper" ] warningHeading
                        , div [ class "popup-action" ] [ text <| translate language <| Translate.WellChildEncounterPopup popupType ]
                        ]
                    , div [ class "actions" ]
                        [ actions ]
                    ]
            )


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 140
    , height = 140
    }


viewPersonDetails : Language -> NominalDate -> Person -> List (Html any)
viewPersonDetails language currentDate person =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (Translate.YearsOld >> translate language)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )

        ( dateOfBirthEntry, ageEntry ) =
            Maybe.map
                (\birthDate ->
                    ( viewEntry Translate.DateOfBirth (formatDDMMYY birthDate)
                    , viewEntry Translate.AgeWord (renderAgeMonthsDays language birthDate currentDate)
                    )
                )
                person.birthDate
                |> Maybe.withDefault ( emptyNode, emptyNode )

        genderEntry =
            viewEntry Translate.GenderLabel (translate language <| Translate.Gender person.gender)

        villageEntry =
            Maybe.map (viewEntry Translate.Village) person.village
                |> Maybe.withDefault emptyNode

        viewEntry labelTransId content =
            p []
                [ span [ class "label" ] [ text <| translate language labelTransId ++ ": " ]
                , span [] [ text content ]
                ]
    in
    [ div [ class "ui image" ]
        [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
    , div [ class "details" ]
        [ h2 [ class "ui header" ]
            [ text person.name ]
        , ageEntry
        , dateOfBirthEntry
        , genderEntry
        , villageEntry
        ]
    ]


viewMainPageContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate zscores id isChw db assembeld model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities isChw
                |> List.filter (expectActivity currentDate zscores isChw assembeld db)
                |> List.partition (activityCompleted currentDate zscores isChw assembeld db)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.ProgressReport

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetActivePage (UserPage (WellChildProgressReportPage id)))
                ]

        viewCard activity =
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| NavigateToActivity id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.WellChildActivityTitle activity
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

                Reports ->
                    ( [], "" )

        viewReportLink labelTransId redirectPage =
            div
                [ class "report-wrapper"
                , onClick <| SetActivePage redirectPage
                ]
                [ div [ class "icon-progress-report" ] []
                , div [ class "report-text" ]
                    [ div [ class "report-label" ] [ text <| translate language labelTransId ]
                    , div [ class "report-link" ] [ text <| translate language Translate.View ]
                    ]
                ]

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
            case pendingActivities of
                [] ->
                    True

                [ WellChildPhoto ] ->
                    True

                _ ->
                    False

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
