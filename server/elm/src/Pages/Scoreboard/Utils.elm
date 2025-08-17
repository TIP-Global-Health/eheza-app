module Pages.Scoreboard.Utils exposing (generateFutureVaccinationsData, valuesByViewMode, viewPercentage)

import App.Types exposing (Site(..))
import AssocList as Dict
import Backend.Scoreboard.Model exposing (..)
import Backend.Scoreboard.Utils exposing (vaccineDoseToComparable)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Scoreboard.Model exposing (..)
import Round


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData :
    Site
    -> NominalDate
    -> VaccinationProgressDict
    -> List ( VaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData site birthDate vaccinationProgress =
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
                            nextVaccinationDataForVaccine site vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered

                        Nothing ->
                            let
                                vaccinationDate =
                                    initialVaccinationDateByBirthDate site
                                        birthDate
                                        initialOpvAdministered
                                        vaccinationProgress
                                        ( vaccineType, VaccineDoseFirst )
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


nextVaccinationDataForVaccine : Site -> VaccineType -> Bool -> NominalDate -> VaccineDose -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine site vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered =
    if getLastDoseForVaccine initialOpvAdministered vaccineType == lastDoseAdministered then
        Nothing

    else
        getNextVaccineDose lastDoseAdministered
            |> Maybe.map
                (\dose ->
                    let
                        ( interval, unit ) =
                            getIntervalForVaccine site vaccineType
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

        VaccineDTPStandalone ->
            VaccineDoseFirst

        VaccinePCV13 ->
            VaccineDoseThird

        VaccineRotarix ->
            VaccineDoseSecond

        VaccineIPV ->
            VaccineDoseFirst

        VaccineMR ->
            VaccineDoseSecond

        VaccineHPV ->
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


getIntervalForVaccine : Site -> VaccineType -> ( Int, Unit )
getIntervalForVaccine site vaccineType =
    case vaccineType of
        VaccineBCG ->
            ( 0, Days )

        VaccineOPV ->
            ( 4, Weeks )

        VaccineDTP ->
            ( 4, Weeks )

        VaccineDTPStandalone ->
            ( 0, Days )

        VaccinePCV13 ->
            ( 4, Weeks )

        VaccineRotarix ->
            ( 4, Weeks )

        VaccineIPV ->
            ( 0, Days )

        VaccineMR ->
            case site of
                SiteBurundi ->
                    ( 9, Months )

                _ ->
                    ( 6, Months )

        VaccineHPV ->
            ( 6, Months )


initialVaccinationDateByBirthDate : Site -> NominalDate -> Bool -> VaccinationProgressDict -> ( VaccineType, VaccineDose ) -> NominalDate
initialVaccinationDateByBirthDate site birthDate initialOpvAdministered vaccinationProgress ( vaccineType, vaccineDose ) =
    let
        dosesInterval =
            vaccineDoseToComparable vaccineDose - 1

        ( interval, unit ) =
            getIntervalForVaccine site vaccineType
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

        VaccineDTPStandalone ->
            -- All 3 dosed of DTP were given, it has passed
            -- at least 28 days since third dose, and, child
            -- is at last 18 months old.
            Dict.get VaccineOPV vaccinationProgress
                |> Maybe.andThen (Dict.get VaccineDoseThird)
                |> Maybe.map
                    (\thirdDoseDate ->
                        let
                            fourWeeksAfterThirdDTPDose =
                                Date.add Days 28 thirdDoseDate

                            dateWhen18MonthsOld =
                                Date.add Months 18 birthDate
                        in
                        if Date.compare fourWeeksAfterThirdDTPDose dateWhen18MonthsOld == GT then
                            fourWeeksAfterThirdDTPDose

                        else
                            dateWhen18MonthsOld
                    )
                |> Maybe.withDefault
                    -- In other words, never.
                    (Date.add Years 999 birthDate)

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

        VaccineHPV ->
            Date.add Years 12 birthDate
                |> Date.add unit (dosesInterval * interval)


{-| We don't include VaccineHPV, since it's given only at
age of 12 years.
-}
allVaccineTypes : List VaccineType
allVaccineTypes =
    [ VaccineBCG
    , VaccineOPV
    , VaccineDTP
    , VaccineDTPStandalone
    , VaccinePCV13
    , VaccineRotarix
    , VaccineIPV
    , VaccineMR
    ]


valuesByViewMode : ViewMode -> List Int -> List Int -> List String
valuesByViewMode viewMode denominators nominators =
    case viewMode of
        ModePercentages ->
            List.map2 viewPercentage nominators denominators

        ModeValues ->
            List.map String.fromInt nominators


viewPercentage : Int -> Int -> String
viewPercentage nominator denominator =
    if denominator == 0 then
        "0.0%"

    else
        (toFloat nominator / toFloat denominator)
            |> (*) 100
            |> Round.round 1
            |> (\number -> number ++ "%")
