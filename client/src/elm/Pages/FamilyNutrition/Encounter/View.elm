module Pages.FamilyNutrition.Encounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model exposing (FamilyParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.FamilyNutrition.Encounter.Model exposing (..)
import Pages.FamilyNutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (isAboveAgeOf2Years, viewConfirmationDialog, viewEndEncounterButton, viewReportLink, viewSkipNCDADialog)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (activityCard, tabItem, thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths, renderDate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> FamilyNutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate site zscores features id isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site zscores features id isChw db model) identity data


viewHeaderAndContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> FamilyNutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate site zscores features id isChw db model data =
    let
        header =
            viewHeader language isChw data

        content =
            viewContent language currentDate site zscores features id isChw db model data

        dialog =
            Maybe.map
                (\state ->
                    case state of
                        DialogEndEncounter ->
                            viewConfirmationDialog language
                                Translate.EndEncounterQuestion
                                Translate.OnceYouEndTheEncounter
                                (CloseEncounter id)
                                (SetDialogState Nothing)
                )
                model.dialogState
    in
    div [ class "page-encounter family-nutrition" ]
        [ header
        , content
        , viewModal dialog
        ]


viewHeader : Language -> Bool -> AssembledData -> Html Msg
viewHeader language isChw data =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.FamilyEncounterLabel
                        Backend.FamilyEncounterParticipant.Model.NutritionEncounter
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| FamilyNutritionParticipantPage InitiatorParticipantsPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> FamilyNutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate site zscores features id isChw db model data =
    let
        displayPerson =
            case model.selectedFamilyMember of
                MotherPage ->
                    data.person

                ChildPage childId ->
                    List.filter (\( cid, _ ) -> cid == childId) data.children
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault data.person

        isAdult =
            isPersonAnAdult currentDate displayPerson
                |> Maybe.withDefault True

        thumbnailClass =
            if isAdult then
                "mother"

            else
                "child"

        dateOfBirth =
            displayPerson.birthDate
                |> Maybe.map (renderDate language)
                |> Maybe.withDefault (translate language Translate.NotAvailable)
                |> Translate.ReportDOB
                |> translate language
                |> text

        age =
            displayPerson.birthDate
                |> Maybe.map
                    (\birthDate ->
                        let
                            renderAgeFunc =
                                if isAboveAgeOf2Years currentDate displayPerson then
                                    renderAgeYearsMonths

                                else
                                    renderAgeMonthsDays
                        in
                        renderAgeFunc language birthDate currentDate
                    )
                |> Maybe.withDefault (translate language Translate.NotAvailable)
                |> Translate.ReportAge
                |> translate language
                |> text

        gender =
            displayPerson.gender
                |> Translate.Gender
                |> translate language
                |> text
    in
    div [ class ("ui unstackable items participant-page " ++ thumbnailClass) ]
        [ div [ class "item" ]
            [ div [ class "ui image" ]
                [ thumbnailImage thumbnailClass displayPerson.avatarUrl displayPerson.name 222 222 ]
            , div [ class "content" ]
                [ h2 [ class "ui header" ]
                    [ text displayPerson.name ]
                , p []
                    [ dateOfBirth, br [] [], age, br [] [], gender ]
                , viewFamilyMemberLinks model data
                ]
            ]
        ]


viewFamilyMemberLinks : Model -> AssembledData -> Html Msg
viewFamilyMemberLinks model data =
    let
        motherMarkup =
            let
                isActive =
                    model.selectedFamilyMember == MotherPage

                attributes =
                    if isActive then
                        [ class "active" ]

                    else
                        [ onClick <| SetSelectedFamilyMember MotherPage ]
            in
            li attributes
                [ span [ class "icon" ]
                    [ span [ class "icon-mother" ] []
                    ]
                ]

        childrenMarkup =
            List.indexedMap viewChildMarkup data.children

        viewChildMarkup index ( childId, _ ) =
            let
                isActive =
                    model.selectedFamilyMember == ChildPage childId

                attributes =
                    if isActive then
                        [ class "active" ]

                    else
                        [ onClick <| SetSelectedFamilyMember (ChildPage childId) ]
            in
            li attributes
                [ span [ class "icon" ]
                    [ span [ class "icon-baby" ] []
                    , span [ class "count" ]
                        [ text <| String.fromInt (index + 1) ]
                    ]
                ]
    in
    ul [ class "links-body" ]
        (motherMarkup :: childrenMarkup)


viewMainPageContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> FamilyNutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> Model
    -> List (Html Msg)
viewMainPageContent language currentDate site zscores features id isChw db data model =
    []



--     let
--         ( completedActivities, pendingActivities ) =
--             partitionActivitiesConsideringSkipped currentDate site zscores features isChw db data model.skippedActivities
--
--         pendingTabTitle =
--             translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities
--
--         completedTabTitle =
--             translate language <| Translate.ActivitiesCompleted <| List.length completedActivities
--
--         reportsTabTitle =
--             translate language Translate.ProgressReport
--
--         tabs =
--             div [ class "ui tabular menu" ]
--                 [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
--                 , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
--                 , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
--                 ]
--
--         ( selectedActivities, emptySectionMessage ) =
--             case model.selectedTab of
--                 Pending ->
--                     ( pendingActivities, translate language Translate.NoActivitiesPending )
--
--                 Completed ->
--                     ( completedActivities, translate language Translate.NoActivitiesCompleted )
--
--                 Reports ->
--                     ( [], "" )
--
--         innerContent =
--             if model.selectedTab == Reports then
--                 div [ class "reports-wrapper" ]
--                     [ viewReportLink language
--                         Translate.ProgressReport
--                         (SetActivePage <|
--                             UserPage <|
--                                 FamilyNutritionProgressReportPage data.id
--                         )
--                     ]
--
--             else
--                 div [ class "full content" ]
--                     [ div [ class "wrap-cards" ]
--                         [ div [ class "ui four cards" ] <|
--                             if List.isEmpty selectedActivities then
--                                 [ span [] [ text emptySectionMessage ] ]
--
--                             else
--                                 List.map viewCard selectedActivities
--                         ]
--                     ]
--
--         viewCard activity =
--             let
--                 action =
--                     case activity of
--                         NCDA ->
--                             SetDialogState <| Just DialogSkipNCDA
--
--                         _ ->
--                             SetActivePage <| UserPage <| FamilyNutritionActivityPage id activity
--             in
--             activityCard language
--                 (Translate.FamilyNutritionActivityTitle activity)
--                 (getActivityIcon activity)
--                 action
--
--         allowEndEncounter =
--             allowEndingEncounter site isChw pendingActivities
--
--         content =
--             div [ class "ui full segment" ]
--                 [ innerContent
--                 , viewEndEncounterButton language allowEndEncounter (SetDialogState <| Just DialogEndEncounter)
--                 ]
--     in
--     [ tabs
--     , content
--     ]
--
--
-- partitionActivities :
--     NominalDate
--     -> Site
--     -> ZScore.Model.Model
--     -> EverySet SiteFeature
--     -> Bool
--     -> ModelIndexedDb
--     -> AssembledData
--     -> ( List FamilyNutritionActivity, List FamilyNutritionActivity )
-- partitionActivities currentDate site zscores features isChw db assembled =
--     partitionActivitiesConsideringSkipped currentDate site zscores features isChw db assembled EverySet.empty
--
--
-- partitionActivitiesConsideringSkipped :
--     NominalDate
--     -> Site
--     -> ZScore.Model.Model
--     -> EverySet SiteFeature
--     -> Bool
--     -> ModelIndexedDb
--     -> AssembledData
--     -> EverySet FamilyNutritionActivity
--     -> ( List FamilyNutritionActivity, List FamilyNutritionActivity )
-- partitionActivitiesConsideringSkipped currentDate site zscores features isChw db assembled skipped =
--     List.filter (\activity -> EverySet.member activity skipped |> not) allActivities
--         |> List.filter (expectActivity currentDate site zscores features isChw assembled db)
--         |> List.partition (activityCompleted currentDate site zscores features isChw assembled db)
--
--
-- allowEndingEncounter : Site -> Bool -> List FamilyNutritionActivity -> Bool
-- allowEndingEncounter site isChw pendingActivities =
--     let
--         mandatoryActivities =
--             allMandatoryActivities site isChw
--     in
--     List.all
--         (\activity ->
--             -- Not an activity that is required to make a decision
--             -- on next steps, and not the Next Steps activity itself.
--             not (List.member activity mandatoryActivities)
--                 && (activity /= NextSteps)
--         )
--         pendingActivities
