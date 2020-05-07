module Pages.NutritionProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), Height, HeightInCm(..), MuacInCm(..), MuacIndication(..), PhotoUrl(..), Weight, WeightInKg(..))
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.NutritionEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.ProgressReport.View exposing (..)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), Length(..), ZScore)
import ZScore.Utils exposing (zScoreLengthHeightForAge, zScoreWeightForAge)
import ZScore.View


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate zscores id db =
    let
        data =
            generateAssembledData id db
    in
    div [ class "page-report nutrition" ] <|
        [ viewWebData language (viewContent language currentDate zscores db) identity data ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> Html Msg
viewContent language currentDate zscores db data =
    let
        child =
            data.person

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
            viewChildInfo language child maybeMother relationText dateOfLastAssessment

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
            viewNutritionSigns language child dateOfLastAssessment signs

        heightWeightMuacTable =
            (( currentDate, data.measurements )
                :: data.previousMeasurementsWithDates
            )
                |> greedyGroupsOf 6
                |> List.map
                    (\groupOfSix ->
                        let
                            ages =
                                groupOfSix
                                    |> List.map
                                        (\( date, _ ) ->
                                            child.birthDate
                                                |> Maybe.map (\birthDate -> renderAgeMonthsDaysHtml language birthDate date)
                                                |> Maybe.withDefault []
                                                |> th
                                                    [ classList
                                                        [ ( "center", True )
                                                        , ( "bottom", True )
                                                        , ( "aligned", True )
                                                        , ( "last", date == dateOfLastAssessment )
                                                        , ( "date-header", True )
                                                        ]
                                                    ]
                                        )
                                    |> (::) (viewAgeCell language)
                                    |> tr []

                            heights =
                                groupOfSix
                                    |> List.map
                                        (\( _, measurements ) ->
                                            measurements.height
                                                |> Maybe.map (Tuple.second >> viewHeightWithIndication language child zscores)
                                                |> withDefaultTextInCell
                                        )
                                    |> (::) (viewHeightCell language)
                                    |> tr []

                            muacs =
                                groupOfSix
                                    |> List.map
                                        (\( _, measurements ) ->
                                            measurements.muac
                                                |> Maybe.map (Tuple.second >> viewMuactWithIndication language)
                                                |> withDefaultTextInCell
                                        )
                                    |> (::) (viewMuacCell language)
                                    |> tr []

                            weights =
                                groupOfSix
                                    |> List.map
                                        (\( _, measurements ) ->
                                            measurements.weight
                                                |> Maybe.map (Tuple.second >> viewWeightWithIndication language child zscores)
                                                |> withDefaultTextInCell
                                        )
                                    |> (::) (viewWeightCell language)
                                    |> tr []
                        in
                        [ ages
                        , heights
                        , weights
                        , muacs
                        ]
                    )
                |> List.concat
                |> tbody []
                |> List.singleton
                |> table [ class "ui collapsing celled table" ]
    in
    div
        [ class "wrap-report" ]
        [ backIcon
        , title
        , subtitle
        , childInfo
        , nutritionSigns
        , heightWeightMuacTable
        ]
