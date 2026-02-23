module Pages.FamilyNutrition.Encounter.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model exposing (FamilyParticipantInitiator(..))
import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity(..))
import Backend.FamilyNutritionActivity.Utils exposing (allActivities, getActivityIcon)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Measurement.Utils exposing (ahezaFormWithDefault, getInputConstraintsMuac, muacFormWithDefault, withinConstraints)
import Measurement.View
import Pages.FamilyNutrition.Encounter.Model exposing (..)
import Pages.FamilyNutrition.Encounter.Utils exposing (activitiesForFamilyMember, activityCompleted, generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (isAboveAgeOf2Years, maybeToBoolTask, resolveTasksCompletedFromTotal, viewConfirmationDialog, viewEndEncounterButtonCustomColor, viewLabel, viewMeasurementInput, viewSaveAction, viewSkipNCDADialog)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewModal)
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
                FamilyMemberMother ->
                    data.person

                FamilyMemberChild childId ->
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
    (div [ class ("ui unstackable items participant-page " ++ thumbnailClass) ]
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
        :: viewMainPageContent language currentDate site zscores features id isChw db data model
    )
        |> div [ class "ui items" ]


viewFamilyMemberLinks : Model -> AssembledData -> Html Msg
viewFamilyMemberLinks model data =
    let
        motherMarkup =
            let
                isActive =
                    model.selectedFamilyMember == FamilyMemberMother

                attributes =
                    if isActive then
                        [ class "active" ]

                    else
                        [ onClick <| SetSelectedFamilyMember FamilyMemberMother ]
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
                    model.selectedFamilyMember == FamilyMemberChild childId

                attributes =
                    if isActive then
                        [ class "active" ]

                    else
                        [ onClick <| SetSelectedFamilyMember (FamilyMemberChild childId) ]
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
    let
        applicableActivities =
            activitiesForFamilyMember currentDate model.selectedFamilyMember data.children

        ( completedActivities, pendingActivities ) =
            List.partition
                (activityCompleted model.selectedFamilyMember data.measurements)
                applicableActivities

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete (List.length pendingActivities)

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted (List.length completedActivities)

        reportsTabTitle =
            translate language Translate.ProgressReport

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle False "reports" (SetActivePage <| UserPage <| FamilyNutritionProgressReportPage data.id)
                ]

        displayedActivities =
            case model.selectedTab of
                Completed ->
                    completedActivities

                Pending ->
                    pendingActivities

                Reports ->
                    []

        selectedActivity =
            case model.selectedActivity of
                Just activity ->
                    if List.member activity displayedActivities then
                        Just activity

                    else
                        List.head displayedActivities

                Nothing ->
                    List.head displayedActivities

        emptySectionMessage =
            case model.selectedTab of
                Completed ->
                    translate language Translate.NoActivitiesCompleted

                Pending ->
                    translate language Translate.NoActivitiesPending

                Reports ->
                    ""

        viewActivityItem activity =
            let
                isActive =
                    selectedActivity == Just activity

                isCompleted =
                    activityCompleted model.selectedFamilyMember data.measurements activity
            in
            div [ class "column" ]
                [ a
                    (classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetSelectedActivity (Just activity) ]
                           )
                    )
                    [ span [ class <| "icon-activity-task icon-" ++ getActivityIcon activity ] []
                    , text <| translate language (Translate.FamilyNutritionActivityTitle activity)
                    ]
                ]

        activityForm =
            Maybe.map (viewActivityForm language currentDate site data model) selectedActivity
                |> Maybe.withDefault emptyNode

        innerContent =
            div [ class "ui task segment" ]
                [ div [ class "ui five column grid" ] <|
                    if List.isEmpty displayedActivities then
                        [ span [] [ text emptySectionMessage ] ]

                    else
                        List.map viewActivityItem displayedActivities
                ]

        -- Allow ending encounter once mother has completed all it's activities.
        allowEndEncounter =
            activitiesForFamilyMember currentDate FamilyMemberMother data.children
                |> List.all (activityCompleted FamilyMemberMother data.measurements)
    in
    [ tabs
    , innerContent
    , activityForm
    , div [ class "end-encounter-button-wrapper" ]
        [ viewEndEncounterButtonCustomColor language "green" allowEndEncounter (SetDialogState <| Just DialogEndEncounter) ]
    ]


viewActivityForm :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> Model
    -> FamilyNutritionActivity
    -> Html Msg
viewActivityForm language currentDate site data model activity =
    case activity of
        FamilyNutritionAheza ->
            viewAhezaForm language data model

        FamilyNutritionMuac ->
            viewMuacForm language currentDate site data model


viewAhezaForm :
    Language
    -> AssembledData
    -> Model
    -> Html Msg
viewAhezaForm language data model =
    let
        existingValue =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    getMeasurementValueFunc data.measurements.ahezaMother

                FamilyMemberChild childId ->
                    Dict.get childId data.measurements.ahezaChild
                        |> Maybe.map (Tuple.second >> .value)

        form =
            ahezaFormWithDefault model.ahezaData.form existingValue

        currentValue =
            form.aheza

        disabled =
            currentValue == Nothing

        saveMsg =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    SaveAhezaMother data.participant.person data.measurements.ahezaMother

                FamilyMemberChild childId ->
                    SaveAhezaChild childId (Dict.get childId data.measurements.ahezaChild)
    in
    div [ class "ui full segment aheza" ]
        [ div [ class "content" ]
            [ viewLabel language <| Translate.FamilyNutritionActivityTitle FamilyNutritionAheza
            , p [ class "activity-helper" ]
                [ text <| translate language Translate.AhezaActivityLabel ]
            , div [ class "ui grid" ]
                [ viewMeasurementInput
                    language
                    currentValue
                    SetAheza
                    "aheza"
                    Translate.KilogramShorthand
                ]
            , viewSaveAction language
                saveMsg
                disabled
            ]
        ]


viewMuacForm :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> Model
    -> Html Msg
viewMuacForm language currentDate site data model =
    let
        existingValue =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    getMeasurementValueFunc data.measurements.muacMother

                FamilyMemberChild childId ->
                    Dict.get childId data.measurements.muacChild
                        |> Maybe.map (Tuple.second >> .value)

        displayPerson =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    data.person

                FamilyMemberChild childId ->
                    List.filter (\( cid, _ ) -> cid == childId) data.children
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault data.person

        form =
            muacFormWithDefault model.muacData.form existingValue

        ( inputs, tasks ) =
            Measurement.View.muacFormInputsAndTasks language currentDate site displayPerson Nothing SetMuac form

        ( tasksCompleted, tasksTotal ) =
            resolveTasksCompletedFromTotal tasks

        constraints =
            getInputConstraintsMuac site

        currentValue =
            case site of
                SyncManager.Model.SiteBurundi ->
                    Maybe.map ((*) 10) form.muac

                _ ->
                    form.muac

        disabled =
            (tasksCompleted /= tasksTotal)
                || (Maybe.map (withinConstraints constraints >> not) currentValue
                        |> Maybe.withDefault True
                   )

        saveMsg =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    SaveMuacMother data.participant.person data.measurements.muacMother

                FamilyMemberChild childId ->
                    SaveMuacChild childId (Dict.get childId data.measurements.muacChild)
    in
    div [ class "ui full segment muac" ]
        [ div [ class "content" ]
            inputs
        , viewSaveAction language
            saveMsg
            disabled
        ]
