module Pages.Scoreboard.Utils exposing (allVaccineTypes, generateFutureVaccinationsData, valuesByViewMode, viewPercentage)

import App.Types exposing (Site(..))
import AssocList as Dict
import Backend.Scoreboard.Model exposing (VaccinationProgressDict, VaccineDose(..), VaccineType(..))
import Backend.Scoreboard.Utils exposing (vaccineDoseToComparable)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Scoreboard.Model exposing (ViewMode(..))
import Round


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData :
    Site
    -> NominalDate
    -> VaccinationProgressDict
    -> List VaccineType
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
                            nextVaccinationDataForVaccine site birthDate vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered

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


wasInitialOpvAdministeredByVaccinationProgress : NominalDate -> VaccinationProgressDict -> Bool
wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress =
    Dict.get VaccineOPV vaccinationProgress
        |> Maybe.andThen (Dict.get VaccineDoseFirst)
        |> Maybe.map
            (\administrationDate ->
                Date.diff Days birthDate administrationDate < 14
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


nextVaccinationDataForVaccine : Site -> NominalDate -> VaccineType -> Bool -> NominalDate -> VaccineDose -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine site birthDate vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered =
    if vaccineDoseToComparable (getLastDoseForVaccine site initialOpvAdministered vaccineType) <= vaccineDoseToComparable lastDoseAdministered then
        -- The course is over once the doses reach the last the site expects. A
        -- child holding more than that has no dose left to receive either.
        Nothing

    else
        getNextVaccineDose lastDoseAdministered
            |> Maybe.map
                (\dose ->
                    let
                        ( interval, unit ) =
                            getIntervalForVaccine site vaccineType

                        byInterval =
                            Date.add unit interval lastDoseDate
                    in
                    if vaccineType == VaccineOPV && initialOpvAdministered && dose == VaccineDoseSecond then
                        -- The initial OPV dose is given before the child is two
                        -- weeks old, and the second at six weeks, so the interval
                        -- alone does not say when it is due.
                        ( dose, laterOf byInterval (Date.add Weeks 6 birthDate) )

                    else if site == SiteRwanda && vaccineType == VaccineIPV && dose == VaccineDoseSecond then
                        -- The interval for IPV is zero, since for most sites
                        -- there is only one dose. Rwanda's second dose is due on
                        -- the later of 36 weeks of age and 28 days after the
                        -- first, which is what says when it is due here.
                        ( dose, laterOf (Date.add Days 28 lastDoseDate) (Date.add Weeks 36 birthDate) )

                    else
                        ( dose, byInterval )
                )


laterOf : NominalDate -> NominalDate -> NominalDate
laterOf first second =
    if Date.compare first second == GT then
        first

    else
        second


getLastDoseForVaccine : Site -> Bool -> VaccineType -> VaccineDose
getLastDoseForVaccine site initialOpvAdministered vaccineType =
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
            case site of
                SiteRwanda ->
                    VaccineDoseSecond

                _ ->
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

        -- So far, there was only single IPV dose.
        -- Since https://github.com/TIP-Global-Health/eheza-app/issues/1426,
        -- at Rwanda site, we got second dose scheduled on the latter
        -- between age of 36 weeks, and 4 weeks after first dose was administered.
        -- This requirement is not reflected here. Instead, it's defined as
        -- special case at appropriate spots in code (which use getIntervalForVaccine).
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
            -- Burundi calls the DTP combo Pentavalent, and issue #926 asks for
            -- this dose after three of them - not after three doses of polio,
            -- which is a different vaccine with a fourth dose of its own.
            Dict.get VaccineDTP vaccinationProgress
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
            -- Nothing here asks for the second dose today: the one caller
            -- resolves a date for a child who has had none, and so asks only
            -- for the first. It is answered anyway, for symmetry with the
            -- client's own copy of this function, which is reached with any
            -- dose - and because leaving it out would answer the second dose
            -- with 14 weeks and an interval of none, which is the wrong date
            -- rather than no date. Whoever asks for it next gets the right
            -- answer.
            case ( site, vaccineDose ) of
                ( SiteRwanda, VaccineDoseSecond ) ->
                    -- The later of 36 weeks of age and 28 days after the first
                    -- dose. Where the first dose is not known, 36 weeks is the
                    -- earliest the second is allowed.
                    Dict.get VaccineIPV vaccinationProgress
                        |> Maybe.andThen (Dict.get VaccineDoseFirst)
                        |> Maybe.map (\firstDoseDate -> laterOf (Date.add Days 28 firstDoseDate) (Date.add Weeks 36 birthDate))
                        |> Maybe.withDefault (Date.add Weeks 36 birthDate)

                _ ->
                    Date.add Weeks 14 birthDate
                        |> Date.add unit (dosesInterval * interval)

        VaccineMR ->
            Date.add Weeks 36 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineHPV ->
            Date.add Years 12 birthDate
                |> Date.add unit (dosesInterval * interval)


{-| We don't include VaccineHPV, since it's given only at
age of 12 years, and this report is for children up to 24 months.
That is what makes this list different from the one in
Pages.Reports.Utils, which covers all ages and does include it - they
are not two copies of the same list, and should not be made one.

Only Burundi schedules VaccineDTPStandalone. Listing it for a site that
does not schedule it leaves a dose that is never administered, and the
caller takes the earliest outstanding dose, so the child reads as off
track from the day it falls due.

-}
allVaccineTypes : Site -> List VaccineType
allVaccineTypes site =
    let
        common =
            [ VaccineBCG
            , VaccineOPV
            , VaccineDTP
            , VaccinePCV13
            , VaccineRotarix
            , VaccineIPV
            , VaccineMR
            ]
    in
    case site of
        SiteBurundi ->
            common ++ [ VaccineDTPStandalone ]

        _ ->
            common


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
