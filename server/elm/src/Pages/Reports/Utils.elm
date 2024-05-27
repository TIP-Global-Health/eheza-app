module Pages.Reports.Utils exposing (..)

import App.Types exposing (Site(..))
import AssocList as Dict
import Backend.Reports.Model exposing (..)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Model exposing (..)


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportDemographics ->
            "demographics"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "demographics" ->
            Just ReportDemographics

        _ ->
            Nothing


countTotalEncounetrs : PatientData -> Int
countTotalEncounetrs data =
    let
        countIndividualDataEncounters =
            Maybe.map (List.map countEncountersDataVisits >> List.sum)
                >> Maybe.withDefault 0

        countGroupDataEncounters =
            Maybe.map countEncountersDataVisits
                >> Maybe.withDefault 0
    in
    countIndividualDataEncounters data.acuteIllnessData
        + countIndividualDataEncounters data.prenatalData
        + countIndividualDataEncounters data.homeVistData
        + countIndividualDataEncounters data.wellChildData
        + countIndividualDataEncounters data.individualNutritionData
        + countGroupDataEncounters data.groupNutritionPmtctData
        + countGroupDataEncounters data.groupNutritionFbfData
        + countGroupDataEncounters data.groupNutritionSorwatheData
        + countGroupDataEncounters data.groupNutritionChwData
        + countGroupDataEncounters data.groupNutritionAchiData


countEncountersDataVisits : EncountersData -> Int
countEncountersDataVisits =
    List.length
