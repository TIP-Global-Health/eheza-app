module Pages.Reports.Utils exposing (..)

import App.Types exposing (Site(..))
import AssocList as Dict
import Backend.Reports.Model exposing (..)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.Reports.Model exposing (..)
import Pages.Utils exposing (unique)


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportDemographics ->
            "demographics"

        ReportNutrition ->
            "nutrition"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "demographics" ->
            Just ReportDemographics

        "nutrition" ->
            Just ReportNutrition

        _ ->
            Nothing


countTotalEncounetrs : PatientData -> Int
countTotalEncounetrs data =
    let
        countIndividualDataEncounters =
            Maybe.map (List.map List.length >> List.sum)
                >> Maybe.withDefault 0

        countGroupDataEncounters =
            Maybe.map List.length
                >> Maybe.withDefault 0
    in
    countIndividualDataEncounters data.acuteIllnessData
        + countIndividualDataEncounters data.prenatalData
        + countIndividualDataEncounters data.homeVisitData
        + countIndividualDataEncounters data.wellChildData
        + countIndividualDataEncounters data.individualNutritionData
        + countGroupDataEncounters data.groupNutritionPmtctData
        + countGroupDataEncounters data.groupNutritionFbfData
        + countGroupDataEncounters data.groupNutritionSorwatheData
        + countGroupDataEncounters data.groupNutritionChwData
        + countGroupDataEncounters data.groupNutritionAchiData



--
--
-- calcualteNutritionMetricsForPatient : PatientData -> NutritionMetrics
-- calcualteNutritionMetricsForPatient data =
--     [ Maybe.map
--         (List.map calcualteNutritionMetricsForEncounters >> sumNutritionMetrics)
--         data.wellChildData
--     , Maybe.map
--         (List.map calcualteNutritionMetricsForEncounters >> sumNutritionMetrics)
--         data.individualNutritionData
--     , Maybe.map calcualteNutritionMetricsForEncounters data.groupNutritionPmtctData
--     , Maybe.map calcualteNutritionMetricsForEncounters data.groupNutritionFbfData
--     , Maybe.map calcualteNutritionMetricsForEncounters data.groupNutritionSorwatheData
--     , Maybe.map calcualteNutritionMetricsForEncounters data.groupNutritionChwData
--     , Maybe.map calcualteNutritionMetricsForEncounters data.groupNutritionAchiData
--     ]
--         |> Maybe.Extra.values
--         |> sumNutritionMetrics
-- calcualteNutritionMetricsForEncounters : List NutritionEncounterData -> NutritionMetrics
-- calcualteNutritionMetricsForEncounters =
--     let
--         categorizeZScore =
--             Maybe.map
--                 (\score ->
--                     if score <= -3 then
--                         ( 0, 0, 1 )
--
--                     else if score <= -2 then
--                         ( 0, 1, 0 )
--
--                     else
--                         ( 1, 0, 0 )
--                 )
--                 >> Maybe.withDefault ( 0, 0, 0 )
--     in
--     List.map nutritionEncounterDataToNutritionMetrics
--         >> sumNutritionMetrics


sumNutritionMetrics : List NutritionMetrics -> NutritionMetrics
sumNutritionMetrics =
    List.foldl
        (\metrics accum ->
            { accum
                | stuntingNormal = accum.stuntingNormal ++ metrics.stuntingNormal
                , stuntingModerate = accum.stuntingModerate ++ metrics.stuntingModerate
                , stuntingSevere = accum.stuntingSevere ++ metrics.stuntingSevere
                , wastingNormal = accum.wastingNormal ++ metrics.wastingNormal
                , wastingModerate = accum.wastingModerate ++ metrics.wastingModerate
                , wastingSevere = accum.wastingSevere ++ metrics.wastingSevere
                , underweightNormal = accum.underweightNormal ++ metrics.underweightNormal
                , underweightModerate = accum.underweightModerate ++ metrics.underweightModerate
                , underweightSevere = accum.underweightSevere ++ metrics.underweightSevere
            }
        )
        emptyNutritionMetrics


nutritionEncounterDataToNutritionMetrics : PersonId -> NutritionEncounterData -> NutritionMetrics
nutritionEncounterDataToNutritionMetrics personId =
    .nutritionData
        >> Maybe.map
            (\data ->
                let
                    categorizeZScore =
                        Maybe.map
                            (\score ->
                                if score <= -3 then
                                    ( [], [], [ personId ] )

                                else if score <= -2 then
                                    ( [], [ personId ], [] )

                                else
                                    ( [ personId ], [], [] )
                            )
                            >> Maybe.withDefault ( [], [], [] )

                    ( stuntingNormal, stuntingModerate, stuntingSevere ) =
                        categorizeZScore data.stunting

                    ( wastingNormal, wastingModerate, wastingSevere ) =
                        categorizeZScore data.wasting

                    ( underweightNormal, underweightModerate, underweightSevere ) =
                        categorizeZScore data.underweight
                in
                { stuntingNormal = stuntingNormal
                , stuntingModerate = stuntingModerate
                , stuntingSevere = stuntingSevere
                , wastingNormal = wastingNormal
                , wastingModerate = wastingModerate
                , wastingSevere = wastingSevere
                , underweightNormal = underweightNormal
                , underweightModerate = underweightModerate
                , underweightSevere = underweightSevere
                }
            )
        >> Maybe.withDefault emptyNutritionMetrics


nutritionMetricsToNutritionPrevalence : NutritionMetrics -> NutritionPrevalence
nutritionMetricsToNutritionPrevalence metrics =
    let
        calcualtePercentage nominator total =
            if List.isEmpty total then
                0

            else
                (toFloat (List.length nominator) / toFloat (List.length total)) * 100

        stuntingTotal =
            metrics.stuntingModerate
                ++ metrics.stuntingSevere
                ++ metrics.stuntingNormal
                |> unique

        wastingTotal =
            metrics.wastingModerate
                ++ metrics.wastingSevere
                ++ metrics.wastingNormal
                |> unique

        underweightTotal =
            metrics.underweightModerate
                ++ metrics.underweightSevere
                ++ metrics.underweightNormal
                |> unique
    in
    { stuntingModerate = calcualtePercentage metrics.stuntingModerate stuntingTotal
    , stuntingSevere = calcualtePercentage metrics.stuntingSevere stuntingTotal
    , wastingModerate = calcualtePercentage metrics.wastingModerate wastingTotal
    , wastingSevere = calcualtePercentage metrics.wastingSevere wastingTotal
    , underweightModerate = calcualtePercentage metrics.underweightModerate underweightTotal
    , underweightSevere = calcualtePercentage metrics.underweightSevere underweightTotal
    }
