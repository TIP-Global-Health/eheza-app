module Backend.Scoreboard.Utils exposing (vaccineDoseFromOrder, vaccineDoseToComparable)

import Backend.Scoreboard.Model exposing (..)


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
