module Pages.Scoreboard.Utils exposing (allVaccineTypes, countChildrenUnder2, datesForMonth, foldRecordsByMonth, formatValues, generateFutureVaccinationsData, generateMonthsGap, isOnTrackForImmunisation, isPregnancyMonth, monthsSinceMonthOf, registeredBy, resolveTargetDateForMonth, valuesByViewMode, viewPercentage)

import App.Types exposing (Site(..))
import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (PatientData, VaccinationProgressDict, VaccineDose(..), VaccineType(..))
import Backend.Scoreboard.Utils exposing (vaccineDoseToComparable)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate, diffMonths, toLastDayOfMonth)
import Pages.Scoreboard.Model exposing (ViewMode(..))
import Round
import Utils.NominalDate exposing (equalByYearAndMonth)


{-| Whole calendar months from the month of the first date to the second.
For example, 25 October 2024 to 21 October 2026 is 24.
-}
monthsSinceMonthOf : NominalDate -> NominalDate -> Int
monthsSinceMonthOf from to =
    diffMonths (Date.floor Date.Month from) to


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
    let
        dosesGiven =
            vaccineDoseToComparable lastDoseAdministered

        dosesTheSiteExpects =
            vaccineDoseToComparable (getLastDoseForVaccine site initialOpvAdministered vaccineType)
    in
    if dosesGiven >= dosesTheSiteExpects then
        -- A child holding every dose the site expects, or more, has none left
        -- to receive.
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


{-| The date a month column is measured at: today for this month, else the
month's last day. An age measured against it is already the column's age.
-}
resolveTargetDateForMonth : Int -> NominalDate -> NominalDate
resolveTargetDateForMonth gapInMonths currentDate =
    if gapInMonths == 0 then
        currentDate

    else
        Date.add Months (-1 * gapInMonths) currentDate
            |> toLastDayOfMonth


{-| The dates on the list that fall within the month of the target date.
-}
datesForMonth : NominalDate -> List NominalDate -> List NominalDate
datesForMonth targetDate dates =
    List.filter (\date -> equalByYearAndMonth date targetDate) dates


{-| Folds patient records into a pane's 12 month columns, passing the given
function each column's date.
-}
foldRecordsByMonth : NominalDate -> Dict Int Int -> (NominalDate -> record -> accum -> accum) -> accum -> List record -> List accum
foldRecordsByMonth currentDate monthsGap updateForMonth emptyValue records =
    List.foldl
        (\record accum ->
            List.indexedMap
                (\index accumValue ->
                    Dict.get index monthsGap
                        |> Maybe.map
                            (\gapInMonths ->
                                updateForMonth (resolveTargetDateForMonth gapInMonths currentDate) record accumValue
                            )
                        |> Maybe.withDefault accumValue
                )
                accum
        )
        (List.repeat 12 emptyValue)
        records


generateMonthsGap : NominalDate -> Int -> Dict Int Int
generateMonthsGap currentDate yearSelectorGap =
    let
        currentMonthNumber =
            Date.monthNumber currentDate
    in
    List.range 1 12
        |> List.map (\monthNumber -> (-1 * 12 * yearSelectorGap) + (-1 * (monthNumber - currentMonthNumber)))
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


formatValues : NominalDate -> Int -> List String -> List String
formatValues currentDate yearSelectorGap =
    let
        currentMonthNumber =
            Date.monthNumber currentDate

        yearsGapFrom2023 =
            2023 - Date.year currentDate
    in
    List.indexedMap
        (\index value ->
            if yearSelectorGap == 0 then
                -- We're at current year.
                if index < currentMonthNumber then
                    value

                else
                    -- Not showing data of future months.
                    ""

            else if yearSelectorGap > yearsGapFrom2023 then
                -- We're in 2024, or above.
                value

            else if index >= 10 then
                -- We're in 2023. Showing data starting Nov 2023.
                -- For prior dates - dash for all rows.
                value

            else
                "-"
        )


{-| Whether the patient was registered by a month column's date, that date
included.
-}
registeredBy : NominalDate -> { a | created : NominalDate } -> Bool
registeredBy targetDateForMonth record =
    Date.compare record.created targetDateForMonth /= GT


{-| The children under two at each month column, counting only those
already registered by the column's date.
-}
countChildrenUnder2 : NominalDate -> Dict Int Int -> List PatientData -> List Int
countChildrenUnder2 currentDate monthsGap =
    foldRecordsByMonth currentDate
        monthsGap
        (\targetDateForMonth record accumValue ->
            let
                ageInMonths =
                    monthsSinceMonthOf record.birthDate targetDateForMonth

                existedDuringExaminationMonth =
                    registeredBy targetDateForMonth record
            in
            if
                existedDuringExaminationMonth
                    && (ageInMonths >= 0)
                    && (ageInMonths < 24)
            then
                accumValue + 1

            else
                accumValue
        )
        0


{-| Whether a pregnancy with this EDD is counted at the month column of the
target date.
-}
isPregnancyMonth : NominalDate -> NominalDate -> Bool
isPregnancyMonth eddDate targetDateForMonth =
    let
        ageInMonths =
            -- Using EDD date to properly resolve the month of
            -- pregnancy (as child may have been born premature).
            monthsSinceMonthOf eddDate targetDateForMonth

        monthsBeforeDelivery =
            -- Age is negative for months that precede the
            -- expected delivery date, which are the ones
            -- pregnancy is examined at.
            negate ageInMonths
    in
    (monthsBeforeDelivery > 0) && (monthsBeforeDelivery < 10)


{-| Whether a child under two had every vaccine that was due by the target
date: the last day of a past month, or today for the current month. The
caller checks the child was registered by then.
-}
isOnTrackForImmunisation : Site -> NominalDate -> PatientData -> Bool
isOnTrackForImmunisation site targetDateForMonth record =
    let
        ageInMonths =
            monthsSinceMonthOf record.birthDate targetDateForMonth
    in
    if (ageInMonths < 0) || (ageInMonths >= 24) then
        False

    else
        let
            referenceDate =
                -- A dose due after this date is not yet missed.
                targetDateForMonth

            -- Filter out vaccinations that were performed
            -- after the reference date.
            vaccinationProgressOnReferrenceDate =
                Dict.map
                    (\_ dosesDict ->
                        Dict.filter
                            (\_ administeredDate ->
                                Date.compare administeredDate referenceDate /= GT
                            )
                            dosesDict
                    )
                    record.ncda.universalIntervention.row1

            futureVaccinations =
                generateFutureVaccinationsData site
                    record.birthDate
                    vaccinationProgressOnReferrenceDate
                    (allVaccineTypes site)

            closestDateForVaccination =
                List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinations
                    |> List.sortWith Date.compare
                    |> List.head
        in
        Maybe.map
            (\closestDate ->
                -- The next dose is due after the reference date, so the
                -- child is on track. Otherwise the child is off track.
                Date.compare closestDate referenceDate == GT
            )
            closestDateForVaccination
            |> Maybe.withDefault
                -- No doses are left to give, so the child is on track.
                True
