module Backend.Scoreboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (..)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra


vaccineDoseToComparable : VaccineDose -> Int
vaccineDoseToComparable dose =
    case dose of
        VaccineDoseFirst ->
            1

        VaccineDoseSecond ->
            2

        VaccineDoseThird ->
            3

        VaccineDoseFourth ->
            4

        VaccineDoseFifth ->
            5


vaccineDoseFromOrder : Int -> Maybe VaccineDose
vaccineDoseFromOrder order =
    case order of
        0 ->
            Just VaccineDoseFirst

        1 ->
            Just VaccineDoseSecond

        2 ->
            Just VaccineDoseThird

        3 ->
            Just VaccineDoseFourth

        4 ->
            Just VaccineDoseFifth

        _ ->
            Nothing


generateVaccinationProgressForVaccine : EverySet NominalDate -> Dict VaccineDose NominalDate
generateVaccinationProgressForVaccine dates =
    EverySet.toList dates
        |> List.sortWith Date.compare
        |> List.indexedMap
            (\index date ->
                vaccineDoseFromOrder index
                    |> Maybe.map (\dose -> ( dose, date ))
            )
        |> Maybe.Extra.values
        |> Dict.fromList
