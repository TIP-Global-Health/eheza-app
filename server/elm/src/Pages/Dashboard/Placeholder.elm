module Pages.Dashboard.Placeholder exposing (facilityAlerts, programFigures)

{-| Placeholder figures, not this installation's data: the whole Program
dashboard, and the Facility dashboard's critical alerts.
-}

import Pages.Dashboard.Model
    exposing
        ( Coverage
        , DashboardLabel(..)
        , Figures
        , Kpi
        , KpiSeries
        , KpiUnit(..)
        , Tile
        )


programFigures : Figures
programFigures =
    { tiles =
        [ Tile WomenCurrentlyInAncCare "2,418"
        , Tile PregnanciesFirstTrimester "646"
        , Tile PregnanciesSecondTrimester "921"
        , Tile PregnanciesThirdTrimester "851"
        , Tile AncMissRate "14%"
        , Tile TotalDeliveriesProgram "3,772"
        , Tile TotalChildrenOnRoutineCare "6,540"
        , Tile GraduatedMotherChildPairs "2,689"
        , Tile LostToFollowUp "402"
        ]
    , kpis = List.map .kpi programIndicators
    , coverage =
        [ Coverage Aspirin ProphylaxisCoverage (Just 79)
        , Coverage Calcium ProphylaxisCoverage (Just 71)
        , Coverage Sqlns SupplementCoverage (Just 65)
        ]
    , alerts =
        [ AlertSitesWithDelayedBirthWeightRecording 6
        , AlertSitesWithPretermRate 4
        ]
    , series = programSeries
    }


facilityAlerts : List DashboardLabel
facilityAlerts =
    [ AlertBirthsMissingBirthWeight 3
    , AlertPretermRateExceeded
    ]


{-| A Program indicator block, with what draws its placeholder curves. `base*`
centre each series, so every indicator draws its own curve.
-}
type alias PlaceholderIndicator =
    { kpi : Kpi
    , seed : Int
    , baseCurrent : Float
    , baseTarget : Float
    }


programIndicators : List PlaceholderIndicator
programIndicators =
    let
        indicator id label current target delta =
            PlaceholderIndicator (Kpi id label UnitPercent (Just current) target Nothing (Just delta))
    in
    [ indicator "anc-cov80" EarlyAncCoverageFirstTrimester 72 "80%" 3.5 71 72 86
    , indicator "ultrasound-p" EarlyUltrasoundCoverage 66 "80%" -1.5 83 68 88
    , indicator "inadequate-gwg" SitesWithInadequateGwg 18 "< 10%" 4.5 91 24 12
    , indicator "delayed-bw" SitesWithDelayedBirthWeightRecording 14 "< 5%" -1.5 97 16 8
    , indicator "preterm-sga" SitesWithElevatedPretermOrSgaBirths 20 "< 15%" 0.5 103 24 16
    , indicator "poor-growth" SitesWithPoorChildGrowthOutcomes 15 "< 10%" 1.2 109 18 12
    ]


programSeries : Kpi -> KpiSeries
programSeries kpi =
    let
        ( seed, baseCurrent, baseTarget ) =
            List.filter (\indicator -> indicator.kpi.id == kpi.id) programIndicators
                |> List.head
                |> Maybe.map (\indicator -> ( indicator.seed, indicator.baseCurrent, indicator.baseTarget ))
                |> Maybe.withDefault ( 0, 0, 0 )
    in
    { performance = monthly seed baseCurrent >> List.map Just

    -- Rises from a little below its base early in the year and flattens near
    -- it. The same every year.
    , target =
        List.map (\month -> Just <| clamp 0 100 (baseTarget - 8 + toFloat month / 11 * 8))
            (List.range 0 11)
    , average = always (List.repeat 12 Nothing)
    }



-- SERIES GENERATION


{-| A small hash into [0,1). Deterministic, so the same indicator and year
always draw the same curve.
-}
rnd : Int -> Float
rnd n =
    toFloat (modBy 233280 (abs n * 9301 + 49297)) / 233280


{-| Monthly line: wanders around `base` with a gentle upward drift across the
year plus stable noise.
-}
monthly : Int -> Float -> Int -> List Float
monthly seed base yearKey =
    List.map
        (\month ->
            let
                drift =
                    toFloat month * 0.9

                noise =
                    (rnd (seed * 131 + yearKey * 17 + month * 7) - 0.5) * 13
            in
            clamp 0 100 (base - 5 + drift + noise)
        )
        (List.range 0 11)
