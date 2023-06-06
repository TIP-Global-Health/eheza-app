module Pages.Scoreboard.Utils exposing (generateFutureVaccinationsData, valuesByViewMode)

import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (..)
import Backend.Scoreboard.Utils exposing (vaccineDoseToComparable)
import Date exposing (Unit(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Pages.Scoreboard.Model exposing (..)
import Round


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData :
    NominalDate
    -> VaccinationProgressDict
    -> List ( VaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData birthDate vaccinationProgress =
    let
        initialOpvAdministered =
            wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress
    in
    List.map
        (\vaccineType ->
            let
                nextVaccinationData =
                    case latestVaccinationDataForVaccine vaccinationProgress vaccineType of
                        Just ( lastDoseAdministered, lastDoseDate ) ->
                            nextVaccinationDataForVaccine vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered

                        Nothing ->
                            let
                                vaccinationDate =
                                    initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, VaccineDoseFirst )
                            in
                            Just ( VaccineDoseFirst, vaccinationDate )
            in
            -- Getting Nothing at nextVaccinationData indicates that
            -- vacination cycle is completed for this vaccine.
            ( vaccineType, nextVaccinationData )
        )
        allVaccineTypes


wasInitialOpvAdministeredByVaccinationProgress : NominalDate -> VaccinationProgressDict -> Bool
wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress =
    Dict.get VaccineOPV vaccinationProgress
        |> Maybe.andThen (Dict.get VaccineDoseFirst)
        |> Maybe.map
            (\adminstrationDate ->
                Date.diff Days birthDate adminstrationDate < 14
            )
        |> Maybe.withDefault False


latestVaccinationDataForVaccine : VaccinationProgressDict -> VaccineType -> Maybe ( VaccineDose, NominalDate )
latestVaccinationDataForVaccine vaccinationsData vaccineType =
    Dict.get vaccineType vaccinationsData
        |> Maybe.andThen
            (Dict.toList
                >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                >> List.reverse
                >> List.head
            )


nextVaccinationDataForVaccine : VaccineType -> Bool -> NominalDate -> VaccineDose -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered =
    if getLastDoseForVaccine initialOpvAdministered vaccineType == lastDoseAdministered then
        Nothing

    else
        getNextVaccineDose lastDoseAdministered
            |> Maybe.map
                (\dose ->
                    let
                        ( interval, unit ) =
                            getIntervalForVaccine vaccineType
                    in
                    ( dose, Date.add unit interval lastDoseDate )
                )


getLastDoseForVaccine : Bool -> VaccineType -> VaccineDose
getLastDoseForVaccine initialOpvAdministered vaccineType =
    case vaccineType of
        VaccineBCG ->
            VaccineDoseFirst

        VaccineOPV ->
            if initialOpvAdministered then
                VaccineDoseFourth

            else
                VaccineDoseThird

        VaccineDTP ->
            VaccineDoseThird

        VaccinePCV13 ->
            VaccineDoseThird

        VaccineRotarix ->
            VaccineDoseSecond

        VaccineIPV ->
            VaccineDoseFirst

        VaccineMR ->
            VaccineDoseSecond


getNextVaccineDose : VaccineDose -> Maybe VaccineDose
getNextVaccineDose dose =
    case dose of
        VaccineDoseFirst ->
            Just VaccineDoseSecond

        VaccineDoseSecond ->
            Just VaccineDoseThird

        VaccineDoseThird ->
            Just VaccineDoseFourth

        VaccineDoseFourth ->
            Just VaccineDoseFifth

        VaccineDoseFifth ->
            Nothing


getIntervalForVaccine : VaccineType -> ( Int, Unit )
getIntervalForVaccine vaccineType =
    case vaccineType of
        VaccineBCG ->
            ( 0, Days )

        VaccineOPV ->
            ( 4, Weeks )

        VaccineDTP ->
            ( 4, Weeks )

        VaccinePCV13 ->
            ( 4, Weeks )

        VaccineRotarix ->
            ( 4, Weeks )

        VaccineIPV ->
            ( 0, Days )

        VaccineMR ->
            ( 6, Months )


initialVaccinationDateByBirthDate : NominalDate -> Bool -> ( VaccineType, VaccineDose ) -> NominalDate
initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, vaccineDose ) =
    let
        dosesInterval =
            vaccineDoseToComparable vaccineDose - 1

        ( interval, unit ) =
            getIntervalForVaccine vaccineType
    in
    case vaccineType of
        VaccineBCG ->
            birthDate

        VaccineOPV ->
            case vaccineDose of
                VaccineDoseFirst ->
                    birthDate

                _ ->
                    if initialOpvAdministered then
                        -- Second dose is given starting from age of 6 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit ((dosesInterval - 1) * interval)

                    else
                        -- Second dose is given starting from age of 10 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit (dosesInterval * interval)

        VaccineDTP ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccinePCV13 ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineRotarix ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineIPV ->
            Date.add Weeks 14 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineMR ->
            Date.add Weeks 36 birthDate
                |> Date.add unit (dosesInterval * interval)


allVaccineTypes : List VaccineType
allVaccineTypes =
    [ VaccineBCG
    , VaccineOPV
    , VaccineDTP
    , VaccinePCV13
    , VaccineRotarix
    , VaccineIPV
    , VaccineMR
    ]


valuesByViewMode : ViewMode -> List Int -> List Int -> List String
valuesByViewMode viewMode denominators nominators =
    case viewMode of
        ModePercentages ->
            List.map2
                (\nominator denominator ->
                    if denominator == 0 then
                        "0.0"

                    else
                        (toFloat nominator / toFloat denominator)
                            |> (*) 100
                            |> Round.round 1
                )
                nominators
                denominators

        ModeValues ->
            List.map String.fromInt nominators
