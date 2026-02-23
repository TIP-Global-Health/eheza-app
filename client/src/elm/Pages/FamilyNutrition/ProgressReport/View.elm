module Pages.FamilyNutrition.ProgressReport.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (isPersonAnAdult)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.FamilyNutrition.Encounter.Model exposing (AssembledData, FamilyMember(..))
import Pages.FamilyNutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.FamilyNutrition.ProgressReport.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (isAboveAgeOf2Years)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths, renderDate)
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
            viewContent language currentDate model data
    in
    div [ class "page-encounter family-nutrition" ]
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
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate model data =
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
    div [ class "ui items" ]
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
