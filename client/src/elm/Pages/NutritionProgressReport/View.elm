module Pages.NutritionProgressReport.View exposing (view)

-- import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
-- import Backend.Measurement.Model exposing (PrenatalMeasurements)
-- import Backend.Person.Model exposing (Person)
-- import Backend.Person.Utils exposing (ageInYears)
-- import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
-- import Date exposing (Interval(..))
-- import Gizra.Html exposing (emptyNode, showMaybe)
-- import List.Extra exposing (greedyGroupsOf)
-- import Pages.ClinicalProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
-- import Pages.DemographicsReport.View exposing (viewHeader, viewItemHeading)
-- import Pages.PrenatalActivity.Utils exposing (calculateBmi)
-- import Pages.PrenatalEncounter.Model exposing (AssembledData)
-- import Pages.PrenatalEncounter.Utils exposing (..)
-- import Pages.Utils exposing (viewPhotoThumbFromPhotoUrl)
-- import PrenatalActivity.Model
--     exposing
--         ( PregnancyTrimester(..)
--         , allMedicalDiagnosis
--         , allObstetricalDiagnosis
--         , allRiskFactors
--         , allTrimesters
--         )
-- import PrenatalActivity.Utils
--     exposing
--         ( generateMedicalDiagnosisAlertData
--         , generateObstetricalDiagnosisAlertData
--         , generateRiskFactorAlertData
--         , getEncounterTrimesterData
--         )
-- import RemoteData exposing (RemoteData(..), WebData)
-- import Round

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.NutritionEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.ProgressReport.View exposing (viewChildInfo, viewNutritionSigns)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NutritionEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate id db =
    let
        data =
            generateAssembledData id db
    in
    div [ class "page-report nutrition" ] <|
        [ viewWebData language (viewContent language currentDate db) identity data ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> AssembledData -> Html Msg
viewContent language currentDate db data =
    let
        backIcon =
            a
                [ class "icon-back"
                , NutritionEncounterPage data.id
                    |> UserPage
                    |> App.Model.SetActivePage
                    |> onClick
                ]
                []

        title =
            h1
                [ class "ui report header" ]
                [ text <| translate language Translate.ParticipantSummary ]

        -- Do we have any kind of measurement taken at the session?
        hasMeasurement measurements =
            isJust measurements.height
                || isJust measurements.muac
                || isJust measurements.nutrition
                || isJust measurements.weight
                || isJust measurements.photo

        dateOfLastAssessment =
            if hasMeasurement data.measurements then
                currentDate

            else
                data.previousMeasurementsWithDates
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault currentDate

        subtitle =
            p
                [ class "date" ]
                [ text <| translate language Translate.DateOfLastAssessment
                , text ": "
                , text <| renderDate language dateOfLastAssessment
                ]

        maybeRelationship =
            Dict.get data.participant.person db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.andThen (Dict.values >> List.head)

        maybeMother =
            maybeRelationship
                |> Maybe.andThen
                    (\relationship ->
                        Dict.get relationship.relatedTo db.people
                            |> Maybe.andThen RemoteData.toMaybe
                    )

        relationText =
            maybeRelationship
                |> Maybe.map
                    (\relationship ->
                        case relationship.relatedBy of
                            MyParent ->
                                Translate.ChildOf

                            MyCaregiver ->
                                Translate.TakenCareOfBy

                            -- Other 2 options will never occur, as we deal with child here.
                            _ ->
                                Translate.ChildOf
                    )
                |> Maybe.withDefault Translate.ChildOf

        childInfo =
            viewChildInfo language data.person maybeMother relationText dateOfLastAssessment

        -- We're using nutrition value from the current session here, at
        -- least for now. So, we're ignoring any later sessions, and we're just
        -- leaving it blank if it wasn't entered in this session (rather than looking
        -- back to a previous session when it was entered).
        --
        -- See <https://github.com/Gizra/ihangane/issues/382#issuecomment-353273873>
        signs =
            data.measurements.nutrition
                |> Maybe.map (Tuple.second >> .value)
                |> Maybe.withDefault EverySet.empty

        nutritionSigns =
            viewNutritionSigns language data.person dateOfLastAssessment signs
    in
    div
        [ class "wrap-report" ]
        [ backIcon
        , title
        , subtitle
        , childInfo
        , nutritionSigns
        ]
