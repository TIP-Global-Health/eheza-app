module Pages.Dashboard.Figures exposing (facilityFigures, programFigures)

{-| The dashboards' figures, worked out from the pregnancy records.
`docs/healthy-start-dashboards.md` says which figures HealthyStart has defined;
the others show a dash.
-}

import Backend.Components.Model exposing (HealthCenterId)
import Backend.Dashboard.Model
    exposing
        ( DeliveryLocation(..)
        , PregnancyOutcome(..)
        , PregnancyRecord
        , PrenatalIndicator(..)
        , PrenatalVisit
        )
import Date
import Dict exposing (Dict)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import List.Extra
import Maybe.Extra
import Pages.Dashboard.Model
    exposing
        ( Coverage
        , DashboardLabel(..)
        , Figures
        , Kpi
        , KpiSeries
        , KpiUnit(..)
        , Tile
        , TimePeriod(..)
        )
import Time exposing (Month(..))


{-| The days in a span, both ends included.
-}
type alias Span =
    { start : NominalDate
    , end : NominalDate
    }


{-| An indicator block. `value` works it out for the records over a span, or
gives Nothing when no pregnancy counts towards it.
-}
type alias Indicator =
    { id : String
    , label : DashboardLabel
    , unit : KpiUnit
    , target : String
    , targetValue : Maybe Float
    , value : Span -> List PregnancyRecord -> Maybe Float
    }


{-| A rate worked out for one site's records, and whether a value of it
crosses the threshold the Program dashboard counts sites by.
-}
type alias SiteThreshold =
    { rate : Span -> List PregnancyRecord -> Maybe Float
    , crosses : Float -> Bool
    }


facilityFigures : NominalDate -> Maybe NominalDate -> List PregnancyRecord -> Maybe HealthCenterId -> TimePeriod -> Figures
facilityFigures currentDate firstCohortDate records site period =
    let
        bySite =
            groupBySite records

        siteRecords =
            Maybe.andThen (\id -> Dict.get id bySite) site
                |> Maybe.withDefault []

        -- The inter site average leaves out the site on screen.
        otherSites =
            Maybe.Extra.unwrap bySite (\id -> Dict.remove id bySite) site
                |> Dict.values

        averageOf indicator overSpan =
            List.filterMap (indicator.value overSpan) otherSites
                |> mean

        span =
            currentSpan currentDate firstCohortDate period

        blocks =
            indicatorFigures currentDate span (previousSpan period span) siteRecords averageOf facilityIndicators
    in
    { tiles = tiles currentDate span (Tile TotalDeliveriesInstitutional) (.deliveryLocation >> (==) (Just FacilityDelivery)) siteRecords
    , kpis = blocks.kpis
    , coverage = coverage span siteRecords
    , alerts = facilityAlerts
    , series = blocks.series
    }


{-| The Program dashboard covers every site's records. Its alerts count the
sites over a threshold in the Time filter's span.
-}
programFigures : NominalDate -> Maybe NominalDate -> List PregnancyRecord -> TimePeriod -> Figures
programFigures currentDate firstCohortDate records period =
    let
        span =
            currentSpan currentDate firstCohortDate period

        blocks =
            indicatorFigures currentDate span (previousSpan period span) records (\_ _ -> Nothing) programIndicators

        alert toLabel threshold =
            let
                sites =
                    List.Extra.count threshold.crosses (siteRates threshold span records)
            in
            if sites > 0 then
                Just (toLabel sites)

            else
                Nothing
    in
    { tiles = tiles currentDate span (Tile TotalDeliveriesProgram) isBirth records
    , kpis = blocks.kpis
    , coverage = coverage span records
    , alerts =
        List.filterMap identity
            [ alert AlertSitesWithDelayedBirthWeightRecording delayedBirthWeightThreshold
            , alert AlertSitesWithPretermRate pretermThreshold
            ]
    , series = blocks.series
    }


{-| The indicator blocks over a span, with the change from the previous one,
and their monthly trend lines. `averageOf` gives the inter site average.
-}
indicatorFigures :
    NominalDate
    -> Span
    -> Span
    -> List PregnancyRecord
    -> (Indicator -> Span -> Maybe Float)
    -> List Indicator
    -> { kpis : List Kpi, series : Kpi -> KpiSeries }
indicatorFigures currentDate span previous records averageOf indicators =
    let
        kpiFor indicator =
            let
                current =
                    indicator.value span records
            in
            { id = indicator.id
            , label = indicator.label
            , unit = indicator.unit
            , current = current
            , target = indicator.target
            , average = averageOf indicator span
            , delta = Maybe.map2 (-) current (indicator.value previous records)
            }

        series kpi =
            let
                indicator =
                    List.Extra.find (\candidate -> candidate.id == kpi.id) indicators

                monthly valueOver year =
                    List.map
                        (\month ->
                            Maybe.map2 valueOver indicator (monthSpan currentDate year month)
                                |> Maybe.Extra.join
                        )
                        months
            in
            { performance = monthly (\found monthOfYear -> found.value monthOfYear records)
            , target = List.repeat 12 (Maybe.andThen .targetValue indicator)
            , average = monthly averageOf
            }
    in
    { kpis = List.map kpiFor indicators
    , series = series
    }


groupBySite : List PregnancyRecord -> Dict HealthCenterId (List PregnancyRecord)
groupBySite =
    List.foldl
        (\record -> Dict.update record.healthCenter (Maybe.withDefault [] >> (::) record >> Just))
        Dict.empty



-- PERIODS


{-| The Time filter's span that holds today, up to today. Weeks count from the
first cohort's enrolment date, or start on Monday without it.
-}
currentSpan : NominalDate -> Maybe NominalDate -> TimePeriod -> Span
currentSpan currentDate firstCohortDate period =
    let
        start =
            case period of
                PeriodMonth ->
                    Date.floor Date.Month currentDate

                PeriodQuarter ->
                    Date.floor Date.Quarter currentDate

                PeriodWeek ->
                    Maybe.map
                        (\cohortDate -> Date.add Date.Days (negate (modBy 7 (diffDays cohortDate currentDate))) currentDate)
                        firstCohortDate
                        |> Maybe.withDefault (Date.floor Date.Monday currentDate)

                PeriodYear ->
                    Date.floor Date.Year currentDate
    in
    Span start currentDate


{-| The whole span of the same kind just before `span`.
-}
previousSpan : TimePeriod -> Span -> Span
previousSpan period span =
    let
        ( unit, count ) =
            case period of
                PeriodMonth ->
                    ( Date.Months, 1 )

                PeriodQuarter ->
                    ( Date.Months, 3 )

                PeriodWeek ->
                    ( Date.Days, 7 )

                PeriodYear ->
                    ( Date.Years, 1 )
    in
    Span (Date.add unit -count span.start) (Date.add Date.Days -1 span.start)


{-| A calendar month, up to today. Nothing for a month still to come.
-}
monthSpan : NominalDate -> Int -> Month -> Maybe Span
monthSpan currentDate year month =
    let
        start =
            Date.fromCalendarDate year month 1
    in
    if Date.compare start currentDate == GT then
        Nothing

    else
        Just (Span start (Date.min currentDate (Date.add Date.Days -1 (Date.add Date.Months 1 start))))


months : List Month
months =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


inSpan : Span -> NominalDate -> Bool
inSpan span =
    Date.isBetween span.start span.end



-- SUMMARY TILES


{-| The summary tiles. `isDelivery` picks the pregnancies concluded in the
span that the deliveries tile counts.
-}
tiles : NominalDate -> Span -> (String -> Tile) -> (PregnancyRecord -> Bool) -> List PregnancyRecord -> List Tile
tiles currentDate span deliveriesTile isDelivery records =
    let
        -- Gestational age today of the pregnancies still in care: not
        -- concluded, and under 42 weeks.
        inCare =
            List.filterMap
                (\record ->
                    if Maybe.Extra.isJust record.dateConcluded then
                        Nothing

                    else
                        gestationalAgeDays record currentDate
                            |> Maybe.Extra.filter (\days -> days >= 0 && days < 294)
                )
                records

        countInCare isInTrimester =
            List.Extra.count isInTrimester inCare

        deliveries =
            List.Extra.count (\record -> concludedIn span record && isDelivery record) records

        childrenOnRoutineCare =
            List.Extra.count
                (\record -> Maybe.Extra.unwrap False (.visits >> List.any (inSpan span)) record.child)
                records

        count =
            String.fromInt
    in
    [ Tile WomenCurrentlyInAncCare (count (List.length inCare))
    , Tile PregnanciesFirstTrimester (count (countInCare (\days -> days < firstTrimesterEndDays)))
    , Tile PregnanciesSecondTrimester (count (countInCare (\days -> days >= firstTrimesterEndDays && days < secondTrimesterEndDays)))
    , Tile PregnanciesThirdTrimester (count (countInCare (\days -> days >= secondTrimesterEndDays)))
    , Tile AncMissRate notDefined
    , deliveriesTile (count deliveries)
    , Tile TotalChildrenOnRoutineCare (count childrenOnRoutineCare)
    , Tile GraduatedMotherChildPairs notDefined
    , Tile LostToFollowUp notDefined
    ]


{-| What a tile shows while HealthyStart has not defined its figure.
-}
notDefined : String
notDefined =
    "—"



-- INDICATOR BLOCKS


facilityIndicators : List Indicator
facilityIndicators =
    [ { id = "anc-booking"
      , label = EarlyAncBookingRate
      , unit = UnitPercent
      , target = "80%"
      , targetValue = Just 80
      , value = earlyBookingRate
      }
    , { id = "median-ga"
      , label = MedianGaAtFirstAnc
      , unit = UnitWeeks
      , target = "≤ 12 wk"
      , targetValue = Just 12
      , value =
            \span ->
                List.filter (bookedIn span)
                    >> List.filterMap (firstAntenatalAgeDays >> Maybe.map (\days -> toFloat days / 7))
                    >> median
      }
    , { id = "ultrasound"
      , label = EarlyUltrasoundCoverage
      , unit = UnitPercent
      , target = "85%"
      , targetValue = Just 85
      , value = earlyUltrasoundRate
      }
    , { id = "high-risk"
      , label = HighRiskPregnancyRate
      , unit = UnitPercent
      , target = "< 15%"
      , targetValue = Just 15
      , value = \span -> List.filter (seenIn span) >> rate .highRisk
      }
    , { id = "pph"
      , label = PostpartumHemorrhageRate
      , unit = UnitPercent
      , target = "< 5%"
      , targetValue = Just 5
      , value = \_ _ -> Nothing
      }
    , { id = "complete-anc"
      , label = CompleteAncAttendanceRate
      , unit = UnitPercent
      , target = "70%"
      , targetValue = Just 70
      , value =
            \span ->
                List.filter (concludedIn span)
                    >> rate (antenatalVisits >> List.length >> (\visits -> visits >= 8))
      }
    ]


{-| Each Program block is the percentage of sites with a rate in the span
whose rate crosses the threshold in the block's title. These blocks have no
target yet.
-}
programIndicators : List Indicator
programIndicators =
    let
        indicator id label threshold =
            { id = id
            , label = label
            , unit = UnitPercent
            , target = notDefined
            , targetValue = Nothing
            , value = \span -> siteRates threshold span >> rate threshold.crosses
            }
    in
    [ indicator "anc-cov80" EarlyAncCoverageFirstTrimester { rate = earlyBookingRate, crosses = \value -> value >= 80 }
    , indicator "ultrasound-p" EarlyUltrasoundCoverageSites { rate = earlyUltrasoundRate, crosses = \value -> value >= 85 }
    , indicator "inadequate-gwg" SitesWithInadequateGwg { rate = inadequateGwgRate, crosses = \value -> value > 10 }
    , indicator "delayed-bw" SitesWithDelayedBirthWeightRecording delayedBirthWeightThreshold
    , indicator "preterm-sga" SitesWithElevatedPretermOrSgaBirths pretermThreshold
    , indicator "poor-growth" SitesWithPoorChildGrowthOutcomes { rate = \_ _ -> Nothing, crosses = always False }
    ]


delayedBirthWeightThreshold : SiteThreshold
delayedBirthWeightThreshold =
    { rate = delayedBirthWeightRate, crosses = \value -> value > 5 }


{-| SGA births are not counted yet, so the rate holds preterm births only.
-}
pretermThreshold : SiteThreshold
pretermThreshold =
    { rate = pretermRate, crosses = \value -> value > 15 }


{-| The rate of every site that has one over the span.
-}
siteRates : SiteThreshold -> Span -> List PregnancyRecord -> List Float
siteRates threshold span =
    groupBySite
        >> Dict.values
        >> List.filterMap (threshold.rate span)



-- RATES


{-| Of the pregnancies first seen in the span, those seen under 13 weeks.
-}
earlyBookingRate : Span -> List PregnancyRecord -> Maybe Float
earlyBookingRate span =
    List.filter (bookedIn span)
        >> rate (firstAntenatalAgeDays >> Maybe.Extra.unwrap False (\days -> days < firstTrimesterEndDays))


{-| Of the pregnancies that reached 20 weeks in the span, while in care, those
scanned before then.
-}
earlyUltrasoundRate : Span -> List PregnancyRecord -> Maybe Float
earlyUltrasoundRate span =
    List.filter
        (\record ->
            Maybe.Extra.unwrap False
                (\weekTwenty -> inSpan span weekTwenty && List.any (.date >> isBefore (Date.add Date.Days 1 weekTwenty)) (antenatalVisits record))
                (weekTwentyDate record)
        )
        >> rate
            (\record ->
                Maybe.Extra.unwrap False
                    (\weekTwenty -> List.any (\date -> isBefore weekTwenty date) record.ultrasounds)
                    (weekTwentyDate record)
            )


{-| Of the pregnancies seen in the span whose weight gain was ever assessed,
those found inadequate at any visit.
-}
inadequateGwgRate : Span -> List PregnancyRecord -> Maybe Float
inadequateGwgRate span =
    List.filter (\record -> seenIn span record && (received IndicatorGwgAdequate record || received IndicatorGwgInadequate record))
        >> rate (received IndicatorGwgInadequate)


{-| Of the newborns born in the span, those whose birth weight is missing or
was recorded more than a day after birth.
-}
delayedBirthWeightRate : Span -> List PregnancyRecord -> Maybe Float
delayedBirthWeightRate span =
    List.filterMap .child
        >> List.filter (.birthDate >> Maybe.Extra.unwrap False (inSpan span))
        >> rate
            (\child ->
                Maybe.map2 (\birthDate examDate -> diffDays birthDate examDate > 1) child.birthDate child.birthWeightDate
                    |> Maybe.withDefault True
            )


{-| Of the births concluded in the span, live or stillborn, those preterm.
-}
pretermRate : Span -> List PregnancyRecord -> Maybe Float
pretermRate span =
    List.filter (\record -> concludedIn span record && isBirth record)
        >> rate (.outcome >> Maybe.Extra.unwrap False (\outcome -> List.member outcome [ OutcomeLivePreterm, OutcomeStillbirthPreterm ]))



-- COVERAGE BARS


coverage : Span -> List PregnancyRecord -> List Coverage
coverage span records =
    let
        seen =
            List.filter (seenIn span) records
    in
    [ Coverage Aspirin ProphylaxisCoverage (List.filter .preEclampsiaRisk seen |> rate (received IndicatorAspirin))
    , Coverage Calcium ProphylaxisCoverage (rate (received IndicatorCalcium) seen)
    , Coverage Sqlns SupplementCoverage Nothing
    ]



-- CRITICAL ALERTS


{-| Placeholders until the Facility alert rules are built.
-}
facilityAlerts : List DashboardLabel
facilityAlerts =
    [ AlertBirthsMissingBirthWeight 3
    , AlertPretermRateExceeded
    ]



-- PREGNANCIES


{-| The app's trimesters: the first ends at 13 weeks, the second at 28.
-}
firstTrimesterEndDays : Int
firstTrimesterEndDays =
    91


secondTrimesterEndDays : Int
secondTrimesterEndDays =
    196


antenatalVisits : PregnancyRecord -> List PrenatalVisit
antenatalVisits =
    .visits >> List.filter .antenatal


{-| Days since the last menstrual period, which is 280 days before the EDD.
-}
gestationalAgeDays : PregnancyRecord -> NominalDate -> Maybe Int
gestationalAgeDays record date =
    Maybe.map (\edd -> 280 - diffDays date edd) record.edd


firstAntenatalAgeDays : PregnancyRecord -> Maybe Int
firstAntenatalAgeDays record =
    List.head (antenatalVisits record)
        |> Maybe.andThen (.date >> gestationalAgeDays record)


weekTwentyDate : PregnancyRecord -> Maybe NominalDate
weekTwentyDate =
    .edd >> Maybe.map (Date.add Date.Days (140 - 280))


{-| The first antenatal visit fell in the span.
-}
bookedIn : Span -> PregnancyRecord -> Bool
bookedIn span =
    antenatalVisits >> List.head >> Maybe.Extra.unwrap False (.date >> inSpan span)


{-| An antenatal visit fell in the span.
-}
seenIn : Span -> PregnancyRecord -> Bool
seenIn span =
    antenatalVisits >> List.any (.date >> inSpan span)


concludedIn : Span -> PregnancyRecord -> Bool
concludedIn span =
    .dateConcluded >> Maybe.Extra.unwrap False (inSpan span)


{-| The pregnancy ended in a birth, live or stillborn.
-}
isBirth : PregnancyRecord -> Bool
isBirth =
    .outcome >> Maybe.Extra.unwrap False ((/=) OutcomeAbortion)


{-| A visit of the pregnancy carries the indicator.
-}
received : PrenatalIndicator -> PregnancyRecord -> Bool
received indicator =
    .visits >> List.any (.indicators >> List.member indicator)


{-| `isBefore limit date`: the date comes before the limit.
-}
isBefore : NominalDate -> NominalDate -> Bool
isBefore limit date =
    Date.compare date limit == LT



-- ARITHMETIC


{-| The percentage of items that meet a condition.
-}
rate : (a -> Bool) -> List a -> Maybe Float
rate isMet items =
    if List.isEmpty items then
        Nothing

    else
        Just (100 * toFloat (List.Extra.count isMet items) / toFloat (List.length items))


mean : List Float -> Maybe Float
mean values =
    if List.isEmpty values then
        Nothing

    else
        Just (List.sum values / toFloat (List.length values))


median : List Float -> Maybe Float
median values =
    let
        sorted =
            List.sort values

        middle =
            List.length sorted // 2
    in
    if modBy 2 (List.length sorted) == 1 then
        List.Extra.getAt middle sorted

    else
        Maybe.map2 (\low high -> (low + high) / 2)
            (List.Extra.getAt (middle - 1) sorted)
            (List.Extra.getAt middle sorted)
