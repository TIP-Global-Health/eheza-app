module Pages.FamilyNutrition.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (isPersonAnAdult)
import Date
import Gizra.NominalDate exposing (NominalDate, diffCalendarYearsAndMonths, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.FamilyNutrition.Encounter.Model exposing (AssembledData, FamilyMember(..))
import Pages.FamilyNutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.FamilyNutrition.ProgressReport.Model exposing (..)
import Pages.FamilyNutrition.ProgressReport.Svg as Svg
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.View exposing (viewEntries)
import Pages.Utils exposing (isAboveAgeOf2Years)
import Pages.WellChild.ProgressReport.View exposing (viewPaneHeading)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths, renderDate, sortTuplesByDateDesc)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> Site
    -> FamilyNutritionEncounterId
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate site id db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site id model) identity data


viewHeaderAndContent :
    Language
    -> NominalDate
    -> Site
    -> FamilyNutritionEncounterId
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate site id model data =
    let
        header =
            viewHeader language id

        content =
            viewContent language currentDate site model data
    in
    div [ class "page-report family-nutrition" ]
        [ header
        , content
        ]


viewHeader : Language -> FamilyNutritionEncounterId -> Html Msg
viewHeader language id =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language Translate.ProgressReport
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| FamilyNutritionEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewContent :
    Language
    -> NominalDate
    -> Site
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate site model data =
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
    div [ class "ui report unstackable items" ]
        [ div [ class ("ui unstackable items participant-page " ++ thumbnailClass) ]
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
        , viewAhezaPane language model data
        , viewMuacPane language currentDate site model data
        ]


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


viewAhezaPane : Language -> Model -> AssembledData -> Html Msg
viewAhezaPane language model data =
    let
        resolveAhezaValue measurements =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    measurements.ahezaMother
                        |> Maybe.map (\( _, measurement ) -> ( measurement.dateMeasured, measurement.value ))

                FamilyMemberChild childId ->
                    Dict.get childId measurements.ahezaChild
                        |> Maybe.map (\( _, measurement ) -> ( measurement.dateMeasured, measurement.value ))

        currentEncounterEntry =
            resolveAhezaValue data.measurements

        previousEncounterEntries =
            List.filterMap
                (\( _, ( _, measurements ) ) -> resolveAhezaValue measurements)
                data.previousMeasurementsWithDates

        allEntries =
            Maybe.map (\entry -> entry :: previousEncounterEntries) currentEncounterEntry
                |> Maybe.withDefault previousEncounterEntries
                |> List.sortWith sortTuplesByDateDesc

        entriesHeading =
            div [ class "heading aheza" ]
                [ div [ class "date" ] [ text <| translate language Translate.Date ]
                , div [ class "amount" ] [ text <| translate language Translate.DistributedAmount ]
                ]

        entries =
            List.map
                (\( date, value ) ->
                    div [ class "entry aheza" ]
                        [ div [ class "cell date" ] [ text <| formatDDMMYYYY date ]
                        , div [ class "cell amount" ] [ text <| String.fromFloat value ++ " " ++ translate language Translate.KilogramShorthand ]
                        ]
                )
                allEntries

        ahezaTitle =
            case model.selectedFamilyMember of
                FamilyMemberChild _ ->
                    Translate.AhezaChild

                FamilyMemberMother ->
                    Translate.AhezaMother
    in
    div [ class "pane aheza" ]
        [ viewPaneHeading language ahezaTitle
        , div [ class "pane-content" ] <|
            entriesHeading
                :: viewEntries language entries
        ]


viewMuacPane : Language -> NominalDate -> Site -> Model -> AssembledData -> Html Msg
viewMuacPane language currentDate site model data =
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

        resolveMuacValue measurements =
            case model.selectedFamilyMember of
                FamilyMemberMother ->
                    measurements.muacMother
                        |> Maybe.map (\( _, measurement ) -> muacValueFunc measurement.value)

                FamilyMemberChild childId ->
                    Dict.get childId measurements.muacChild
                        |> Maybe.map (\( _, measurement ) -> muacValueFunc measurement.value)

        previousEncounterMuacValues =
            List.filterMap
                (\( date, ( _, measurements ) ) ->
                    resolveMuacValue measurements
                        |> Maybe.map (\value -> ( date, value ))
                )
                data.previousMeasurementsWithDates

        currentEncounterMuacValue =
            resolveMuacValue data.measurements
                |> Maybe.map (\value -> ( data.encounter.startDate, value ))

        allMuacValues =
            case currentEncounterMuacValue of
                Just point ->
                    point :: previousEncounterMuacValues

                Nothing ->
                    previousEncounterMuacValues

        earliestMuacDate =
            List.map Tuple.first allMuacValues
                |> List.sortWith Date.compare
                |> List.head

        anchorAge =
            displayPerson.birthDate
                |> Maybe.map
                    (\birthDate ->
                        diffCalendarYearsAndMonths birthDate
                            (earliestMuacDate |> Maybe.withDefault currentDate)
                    )
                |> Maybe.withDefault { years = 0, months = 0 }

        muacPoints =
            displayPerson.birthDate
                |> Maybe.map
                    (\birthDate ->
                        let
                            anchorTotalMonths =
                                toFloat (anchorAge.years * 12 + anchorAge.months)
                        in
                        List.map
                            (\( date, value ) ->
                                let
                                    ageInMonths =
                                        toFloat (diffDays birthDate date) / 30.4375

                                    monthOffset =
                                        ageInMonths - anchorTotalMonths
                                in
                                ( monthOffset, value )
                            )
                            allMuacValues
                    )
                |> Maybe.withDefault []
    in
    div [ class "pane muac" ]
        [ viewPaneHeading language Translate.MUAC
        , div [ class "pane-content" ]
            [ Svg.viewMuacChart language site isAdult anchorAge muacPoints ]
        ]
