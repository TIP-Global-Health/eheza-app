module Backend.PrenatalEncounter.Utils exposing (..)

import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounterType(..)
        , PrenatalProgressReportInitiator(..)
        , RecordPreganancyInitiator(..)
        )
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


recordPreganancyInitiatorToUrlFragment : RecordPreganancyInitiator -> String
recordPreganancyInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorParticipantPage ->
            "participant-page"

        InitiatorWarningPopup ->
            "warning-popup"

        InitiatorPostpartumEncounter encounterId ->
            "postpartum-encounter-" ++ fromEntityUuid encounterId


recordPreganancyInitiatorFromUrlFragment : String -> Maybe RecordPreganancyInitiator
recordPreganancyInitiatorFromUrlFragment s =
    case s of
        "participant-page" ->
            Just InitiatorParticipantPage

        "warning-popup" ->
            Just InitiatorWarningPopup

        _ ->
            if String.startsWith "postpartum-encounter-" s then
                String.dropLeft 21 s
                    |> toEntityUuid
                    |> InitiatorPostpartumEncounter
                    |> Just

            else
                Nothing


progressReportInitiatorToUrlFragment : PrenatalProgressReportInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorEncounterPage encounterId ->
            "encounter-page-" ++ fromEntityUuid encounterId

        InitiatorRecurrentEncounterPage encounterId ->
            "recurrent-encounter-page-" ++ fromEntityUuid encounterId

        InitiatorNewEncounter encounterId ->
            "encounter-" ++ fromEntityUuid encounterId

        InitiatorPatientRecord personId ->
            "patient-record-" ++ fromEntityUuid personId

        InitiatorCaseManagement encounterId ->
            "case-management-" ++ fromEntityUuid encounterId


progressReportInitiatorFromUrlFragment : String -> Maybe PrenatalProgressReportInitiator
progressReportInitiatorFromUrlFragment s =
    if String.startsWith "encounter-page-" s then
        String.dropLeft 15 s
            |> toEntityUuid
            |> InitiatorEncounterPage
            |> Just

    else if String.startsWith "recurrent-encounter-page-" s then
        String.dropLeft 25 s
            |> toEntityUuid
            |> InitiatorRecurrentEncounterPage
            |> Just

    else if String.startsWith "encounter-" s then
        String.dropLeft 10 s
            |> toEntityUuid
            |> InitiatorNewEncounter
            |> Just

    else if String.startsWith "patient-record-" s then
        String.dropLeft 15 s
            |> toEntityUuid
            |> InitiatorPatientRecord
            |> Just

    else if String.startsWith "case-management-" s then
        String.dropLeft 16 s
            |> toEntityUuid
            |> InitiatorCaseManagement
            |> Just

    else
        Nothing


{-| LMP date is considered to be the day on which pregnancy has started.
-EDD date is estimated delivery date - the day on which we expect pregnancy
-be concluded.
-Pregnancy lasts 280 days.
-}
lmpToEDDDate : NominalDate -> NominalDate
lmpToEDDDate lmpDate =
    Date.add Days 280 lmpDate


eddToLmpDate : NominalDate -> NominalDate
eddToLmpDate eddDate =
    Date.add Days -280 eddDate


isNurseEncounter : PrenatalEncounterType -> Bool
isNurseEncounter encounterType =
    List.member encounterType [ NurseEncounter, NursePostpartumEncounter, HealthyStartEncounter ]


{-| Gestational Weight Gain (GWG) and Adequate Weight Gain (AWG) Calculation Functions

These functions implement the method for calculating gestational weight gain and
determining whether weight gain at a visit is adequate for pregnant women.

## Overview

Gestational weight gain (GWG) is the amount of weight a pregnant woman gains between
two weight measurements taken during pregnancy. GWG is always assessed over a single
observation period (from previous visit to current visit).

Adequate weight gain (AWG) is determined by comparing actual weight gain to expected
weight gain for the observation period.

## Key Principles

1. GWG cannot be assessed at the first ANC visit (no previous weight to compare)
2. Expected weight gain is based on daily rates, not scheduled visit intervals
3. Nutrition status at booking permanently determines expected gain rate after 13 weeks
4. Adequate weight gain means meeting or exceeding the expected gain for time elapsed
5. Classification is made independently at each visit

## Calculation Steps

### Step 1: Determine nutrition status at booking (isSeverelyUndernourished)
At the first ANC visit (booking), classify the woman as severely undernourished if EITHER:
- BMI < 17.5 kg/m², OR
- MUAC < 21 cm
This classification is permanent and does not change later.

### Step 2: Identify observation period
For any visit, identify:
- Previous visit where weight was measured
- Current visit where weight is measured
Calculate the time between these visits in days.

### Step 3: Calculate actual GWG (calculateGestationalWeightGain)
Actual GWG = Current weight - Previous weight

### Step 4: Determine expected weight gain (calculateExpectedWeightGain)
Expected gain depends on:
- Whether woman was severely undernourished at booking
- Gestational age during observation period

#### Expected daily rates (expectedDailyWeightGain):
- Before 13 weeks of pregnancy: 23.5 grams/day (all women)
- After 13 weeks, not severely undernourished: 60 grams/day
- After 13 weeks, severely undernourished: 73 grams/day

#### For periods spanning 13 weeks:
- Apply 23.5 g/day to days before 13 weeks
- Apply appropriate rate (60 or 73 g/day) to days from 13 weeks onward

### Step 5: Classify AWG (isAdequateWeightGain)
Weight gain is adequate if: actual weight gain >= expected weight gain

-}
{-| Determine if a woman is severely undernourished at booking
A woman is severely undernourished if either:
- BMI is less than 17.5 kg/m², or
- MUAC is less than 21 cm
-}
isSeverelyUndernourished : Maybe Float -> Maybe Float -> Bool
isSeverelyUndernourished maybeBmi maybeMuac =
    let
        bmiCriteria =
            Maybe.map (\bmi -> bmi < 17.5) maybeBmi
                |> Maybe.withDefault False

        muacCriteria =
            Maybe.map (\muac -> muac < 21.0) maybeMuac
                |> Maybe.withDefault False
    in
    bmiCriteria || muacCriteria


{-| Calculate the expected daily weight gain in grams based on:
- Whether woman was severely undernourished at booking
- Whether we're before or after 13 weeks of gestation
-}
expectedDailyWeightGain : Bool -> Bool -> Float
expectedDailyWeightGain severelyUndernourished before13Weeks =
    if before13Weeks then
        -- Before 13 weeks: 23.5 grams per day for all women
        23.5

    else if severelyUndernourished then
        -- After 13 weeks, severely undernourished: 73 grams per day
        73.0

    else
        -- After 13 weeks, not severely undernourished: 60 grams per day
        60.0


{-| Calculate expected weight gain for an observation period
The observation period may span across the 13-week boundary, so we need to:
1. Calculate days before and after 13 weeks
2. Apply appropriate daily rates to each portion
3. Sum the total expected gain

Parameters:
- severelyUndernourished: Whether woman was undernourished at booking
- lmpDate: Last menstrual period date (start of pregnancy)
- previousVisitDate: Date of previous weight measurement
- currentVisitDate: Date of current weight measurement
-}
calculateExpectedWeightGain : Bool -> NominalDate -> NominalDate -> NominalDate -> Float
calculateExpectedWeightGain severelyUndernourished lmpDate previousVisitDate currentVisitDate =
    let
        -- 13 weeks is 91 days (13 * 7)
        thirteenWeeksDate =
            Date.add Days 91 lmpDate

        -- Calculate days in observation period
        totalDays =
            Date.diff Days previousVisitDate currentVisitDate
                |> toFloat

        -- Determine how many days fall before and after 13 weeks
        ( daysBefore13Weeks, daysAfter13Weeks ) =
            if Date.compare currentVisitDate thirteenWeeksDate == LT then
                -- Entire period is before 13 weeks
                ( totalDays, 0 )

            else if Date.compare previousVisitDate thirteenWeeksDate == GT || Date.compare previousVisitDate thirteenWeeksDate == EQ then
                -- Entire period is at or after 13 weeks
                ( 0, totalDays )

            else
                -- Period spans 13 weeks boundary
                let
                    daysBefore =
                        Date.diff Days previousVisitDate thirteenWeeksDate
                            |> toFloat

                    daysAfter =
                        totalDays - daysBefore
                in
                ( daysBefore, daysAfter )

        -- Calculate expected weight gain for each period
        expectedBefore =
            daysBefore13Weeks * expectedDailyWeightGain severelyUndernourished True

        expectedAfter =
            daysAfter13Weeks * expectedDailyWeightGain severelyUndernourished False
    in
    expectedBefore + expectedAfter


{-| Calculate actual gestational weight gain (GWG) between two visits
-}
calculateGestationalWeightGain : Float -> Float -> Float
calculateGestationalWeightGain previousWeight currentWeight =
    currentWeight - previousWeight


{-| Classify whether weight gain is adequate
Weight gain is adequate if actual gain >= expected gain
-}
isAdequateWeightGain : Float -> Float -> Bool
isAdequateWeightGain actualGain expectedGain =
    actualGain >= expectedGain
