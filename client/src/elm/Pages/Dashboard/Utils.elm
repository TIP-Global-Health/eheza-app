module Pages.Dashboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Dashboard.Model
    exposing
        ( AcuteIllnessDataItem
        , AcuteIllnessEncounterDataItem
        , AssembledData
        , CaseManagement
        , CaseNutrition
        , CaseNutritionTotal
        , ChildrenBeneficiariesStats
        , DashboardStats
        , DashboardStatsRaw
        , Nutrition
        , NutritionPageData
        , NutritionStatus(..)
        , NutritionValue
        , Periods
        , PersonIdentifier
        , PrenatalDataItem
        , ProgramType(..)
        , TotalBeneficiaries
        , TotalEncountersData
        , emptyTotalBeneficiaries
        )
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, IndividualEncounterParticipantOutcome(..), PregnancyOutcome(..))
import Backend.Measurement.Model
    exposing
        ( Call114Sign(..)
        , DangerSign(..)
        , FollowUpMeasurements
        , HCContactSign(..)
        , HCRecommendation(..)
        , IsolationSign(..)
        , Recommendation114(..)
        , SendToHCSign(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Date exposing (Month, Unit(..), isBetween, monthNumber, numberToMonth, year)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (filterVillageResidents, generateAcuteIllnessFollowUps, generateNutritionFollowUps, generatePrenatalFollowUps)
import Pages.GlobalCaseManagement.View exposing (generateAcuteIllnessFollowUpEntries, generateNutritionFollowUpEntries, generatePrenatalFollowUpEntries)
import RemoteData
import Translate exposing (Language)


filterProgramTypeToString : FilterProgramType -> String
filterProgramTypeToString filterProgramType =
    case filterProgramType of
        FilterAllPrograms ->
            "all"

        FilterProgramAchi ->
            "achi"

        FilterProgramFbf ->
            "fbf"

        FilterProgramPmtct ->
            "pmtct"

        FilterProgramSorwathe ->
            "sorwathe"

        FilterProgramCommunity ->
            "community"


filterProgramTypeFromString : String -> Maybe FilterProgramType
filterProgramTypeFromString string =
    case string of
        "all" ->
            Just FilterAllPrograms

        "achi" ->
            Just FilterProgramAchi

        "fbf" ->
            Just FilterProgramFbf

        "pmtct" ->
            Just FilterProgramPmtct

        "sorwathe" ->
            Just FilterProgramSorwathe

        "community" ->
            Just FilterProgramCommunity

        _ ->
            Nothing


generateAssembledData : NominalDate -> HealthCenterId -> DashboardStatsRaw -> ModelIndexedDb -> FilterProgramType -> Maybe VillageId -> AssembledData
generateAssembledData currentDate healthCenterId stats db programTypeFilter selectedVillageFilter =
    let
        filteredStats =
            generateFilteredDashboardStats stats programTypeFilter selectedVillageFilter
    in
    { stats = filteredStats
    , acuteIllnessData = generateFilteredAcuteIllnessData stats selectedVillageFilter
    , prenatalData = generateFilteredPrenatalData stats selectedVillageFilter
    , caseManagementData =
        Dict.get healthCenterId db.followUpMeasurements
            |> Maybe.andThen RemoteData.toMaybe
    , nutritionPageData = generateNutritionPageData currentDate filteredStats db programTypeFilter selectedVillageFilter
    }


generateFilteredDashboardStats : DashboardStatsRaw -> FilterProgramType -> Maybe VillageId -> DashboardStats
generateFilteredDashboardStats stats programTypeFilter selectedVillageFilter =
    { caseManagement =
        { thisYear =
            applyProgramTypeAndResidentsFilters stats.villagesWithResidents
                programTypeFilter
                selectedVillageFilter
                stats.caseManagement.thisYear
                |> caseManagementMergeDuplicates
        , lastYear =
            applyProgramTypeAndResidentsFilters
                stats.villagesWithResidents
                programTypeFilter
                selectedVillageFilter
                stats.caseManagement.lastYear
                |> caseManagementMergeDuplicates
        }
    , childrenBeneficiaries = applyProgramTypeAndResidentsFilters stats.villagesWithResidents programTypeFilter selectedVillageFilter stats.childrenBeneficiaries
    , completedPrograms = stats.completedPrograms
    , familyPlanning = stats.familyPlanning
    , missedSessions = stats.missedSessions
    , totalEncounters = stats.totalEncounters
    , villagesWithResidents = stats.villagesWithResidents
    , timestamp = stats.timestamp
    }


generateFilteredAcuteIllnessData : DashboardStatsRaw -> Maybe VillageId -> List AcuteIllnessDataItem
generateFilteredAcuteIllnessData stats selectedVillageFilter =
    selectedVillageFilter
        |> Maybe.andThen
            (\villageId -> Dict.get villageId stats.villagesWithResidents)
        |> Maybe.map
            (\residents -> List.filter (\item -> List.member item.identifier residents) stats.acuteIllnessData)
        |> Maybe.withDefault []


generateFilteredPrenatalData : DashboardStatsRaw -> Maybe VillageId -> List PrenatalDataItem
generateFilteredPrenatalData stats selectedVillageFilter =
    selectedVillageFilter
        |> Maybe.andThen
            (\villageId -> Dict.get villageId stats.villagesWithResidents)
        |> Maybe.map
            (\residents -> List.filter (\item -> List.member item.identifier residents) stats.prenatalData)
        |> Maybe.withDefault []


applyProgramTypeAndResidentsFilters :
    Dict VillageId (List PersonIdentifier)
    -> FilterProgramType
    -> Maybe VillageId
    -> Dict ProgramType (List { a | identifier : PersonIdentifier })
    -> List { a | identifier : PersonIdentifier }
applyProgramTypeAndResidentsFilters villagesWithResidents programTypeFilter selectedVillageFilter dict =
    case programTypeFilter of
        FilterAllPrograms ->
            let
                achi =
                    Dict.get ProgramAchi dict
                        |> Maybe.withDefault []

                fbf =
                    Dict.get ProgramFbf dict
                        |> Maybe.withDefault []

                pmtct =
                    Dict.get ProgramPmtct dict
                        |> Maybe.withDefault []

                sorwathe =
                    Dict.get ProgramSorwathe dict
                        |> Maybe.withDefault []

                chw =
                    Dict.get ProgramChw dict
                        |> Maybe.withDefault []

                individual =
                    Dict.get ProgramIndividual dict
                        |> Maybe.withDefault []
            in
            achi ++ fbf ++ pmtct ++ sorwathe ++ chw ++ individual

        FilterProgramAchi ->
            Dict.get ProgramAchi dict
                |> Maybe.withDefault []

        FilterProgramFbf ->
            Dict.get ProgramFbf dict
                |> Maybe.withDefault []

        FilterProgramPmtct ->
            Dict.get ProgramPmtct dict
                |> Maybe.withDefault []

        FilterProgramSorwathe ->
            Dict.get ProgramSorwathe dict
                |> Maybe.withDefault []

        FilterProgramCommunity ->
            selectedVillageFilter
                |> Maybe.map
                    (\selectedVillage ->
                        let
                            villageResidents =
                                Dict.get selectedVillage villagesWithResidents
                                    |> Maybe.withDefault []
                        in
                        applyProgramTypeAndResidentsFilters villagesWithResidents FilterAllPrograms selectedVillageFilter dict
                            |> List.filter (\item -> List.member item.identifier villageResidents)
                    )
                |> Maybe.withDefault []


mergeNutritionValueDicts : Dict Int NutritionValue -> Dict Int NutritionValue -> Dict Int NutritionValue
mergeNutritionValueDicts dict1 dict2 =
    Dict.merge
        (\key nutritionCase -> Dict.insert key nutritionCase)
        (\key nutritionCase1 nutritionCase2 -> Dict.insert key (mergeNutritionValues nutritionCase1 nutritionCase2))
        (\key nutritionCase -> Dict.insert key nutritionCase)
        dict1
        dict2
        Dict.empty


caseManagementMergeDuplicates : List CaseManagement -> List CaseManagement
caseManagementMergeDuplicates cases =
    List.foldl
        (\candidate accum ->
            Dict.get candidate.identifier accum
                |> Maybe.map
                    (\current ->
                        let
                            mergedNutrition =
                                { stunting = mergeNutritionValueDicts candidate.nutrition.stunting candidate.nutrition.stunting
                                , underweight = mergeNutritionValueDicts candidate.nutrition.underweight candidate.nutrition.underweight
                                , wasting = mergeNutritionValueDicts candidate.nutrition.wasting candidate.nutrition.wasting
                                , muac = mergeNutritionValueDicts candidate.nutrition.muac candidate.nutrition.muac
                                , nutritionSigns = mergeNutritionValueDicts candidate.nutrition.nutritionSigns candidate.nutrition.nutritionSigns
                                }

                            merged =
                                { current | nutrition = mergedNutrition }
                        in
                        Dict.insert current.identifier merged accum
                    )
                |> Maybe.withDefault (Dict.insert candidate.identifier candidate accum)
        )
        Dict.empty
        cases
        |> Dict.values


mergeNutritionValues : NutritionValue -> NutritionValue -> NutritionValue
mergeNutritionValues first second =
    case compareNutritionStatus first.class second.class of
        GT ->
            first

        EQ ->
            let
                firstValue =
                    String.toFloat first.value

                secondValue =
                    String.toFloat second.value
            in
            case ( firstValue, secondValue ) of
                ( Just value1, Just value2 ) ->
                    if compare value1 value2 == GT then
                        first

                    else
                        second

                ( Just value1, Nothing ) ->
                    first

                ( Nothing, Just value2 ) ->
                    second

                ( Nothing, Nothing ) ->
                    second

        LT ->
            second


compareNutritionStatus : NutritionStatus -> NutritionStatus -> Order
compareNutritionStatus first second =
    let
        numericValue status =
            case status of
                Backend.Dashboard.Model.Neutral ->
                    0

                Backend.Dashboard.Model.Good ->
                    1

                Backend.Dashboard.Model.Moderate ->
                    2

                Backend.Dashboard.Model.Severe ->
                    3
    in
    compare (numericValue first) (numericValue second)


generateTotalEncounters : TotalEncountersData -> FilterProgramType -> Maybe VillageId -> Periods
generateTotalEncounters data programTypeFilter selectedVillageFilter =
    let
        ( dict, programTypeFilter_ ) =
            case selectedVillageFilter of
                Just village ->
                    ( Dict.get village data.villages
                        |> Maybe.withDefault Dict.empty
                    , FilterAllPrograms
                    )

                -- When village is not selected, we show global data.
                Nothing ->
                    ( data.global, programTypeFilter )
    in
    generateTotalEncountersFromPeriodsDict programTypeFilter_ dict


generateTotalEncountersFromPeriodsDict : FilterProgramType -> Dict ProgramType Periods -> Periods
generateTotalEncountersFromPeriodsDict programTypeFilter dict =
    let
        emptyPeriods =
            Periods 0 0
    in
    case programTypeFilter of
        FilterAllPrograms ->
            let
                achi =
                    Dict.get ProgramAchi dict
                        |> Maybe.withDefault emptyPeriods

                fbf =
                    Dict.get ProgramFbf dict
                        |> Maybe.withDefault emptyPeriods

                pmtct =
                    Dict.get ProgramPmtct dict
                        |> Maybe.withDefault emptyPeriods

                sorwathe =
                    Dict.get ProgramSorwathe dict
                        |> Maybe.withDefault emptyPeriods

                chw =
                    Dict.get ProgramChw dict
                        |> Maybe.withDefault emptyPeriods

                individual =
                    Dict.get ProgramIndividual dict
                        |> Maybe.withDefault emptyPeriods

                sumPeriods p1 p2 =
                    Periods (p1.lastYear + p2.lastYear) (p1.thisYear + p2.thisYear)
            in
            sumPeriods achi fbf
                |> sumPeriods pmtct
                |> sumPeriods sorwathe
                |> sumPeriods chw
                |> sumPeriods individual

        FilterProgramAchi ->
            Dict.get ProgramAchi dict
                |> Maybe.withDefault emptyPeriods

        FilterProgramFbf ->
            Dict.get ProgramFbf dict
                |> Maybe.withDefault emptyPeriods

        FilterProgramPmtct ->
            Dict.get ProgramPmtct dict
                |> Maybe.withDefault emptyPeriods

        FilterProgramSorwathe ->
            Dict.get ProgramSorwathe dict
                |> Maybe.withDefault emptyPeriods

        FilterProgramCommunity ->
            -- This type requires village to be selected, and when it is,
            -- generateTotalEncounters() will invoke this function with
            -- FilterAllPrograms for village data.
            emptyPeriods



--
-- Acute illness - Overview functions.
--


getAcuteIllnessEncountersForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem
getAcuteIllnessEncountersForSelectedMonth selectedDate itemsList =
    List.map .encounters itemsList
        |> List.concat
        |> List.filter (.startDate >> withinSelectedMonth selectedDate)


countAcuteIllnessAssesments : List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessAssesments encounters =
    -- Count number of encounters that occured during selected month.
    List.length encounters


countAcuteIllnessDiagnosedCases : List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessDiagnosedCases encounters =
    List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis) encounters
        |> List.length


countAcuteIllnessCasesByTreatmentApproach : List AcuteIllnessEncounterDataItem -> ( Int, Int )
countAcuteIllnessCasesByTreatmentApproach encounters =
    let
        diagnosedEncounters =
            List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis) encounters

        sentToHC =
            List.filter wasSentToHCByDiagnosis diagnosedEncounters

        managedAtHome =
            List.filter wasManagedAtHomeByDiagnosis diagnosedEncounters
    in
    ( List.length sentToHC, List.length managedAtHome )


{-| There's a difference betweeen non Covid and Covid cases, when making
a decision if to send patient to health center.
Covid case has a specific set of parameters, while non Covid has a simple logic -
only those that Yes answered to quesiton about patien being refered to HC.
-}
wasSentToHCByDiagnosis : AcuteIllnessEncounterDataItem -> Bool
wasSentToHCByDiagnosis encounter =
    case encounter.diagnosis of
        DiagnosisCovid19 ->
            let
                sentToHCBy114 =
                    EverySet.member Call114 encounter.call114Signs
                        && EverySet.member SendToHealthCenter encounter.recommendation114

                sentToHCByHC =
                    EverySet.member ContactedHealthCenter encounter.hcContactSigns
                        && EverySet.member ComeToHealthCenter encounter.hcRecommendation
            in
            sentToHCBy114 || sentToHCByHC

        -- All others, but it must exclude NoAcuteIllnessDiagnosis - invoking function
        -- should be taking care of this.
        _ ->
            -- All that were refered sent to HC.
            EverySet.member ReferToHealthCenter encounter.sendToHCSigns


{-| There's a difference betweeen non Covid and Covid cases, when making
a decision if to manage illness at home.
Covid case has a specific set of parameters, while non Covid has a simple logic -
if patient was not sent to HC, then it was managed at home.
-}
wasManagedAtHomeByDiagnosis : AcuteIllnessEncounterDataItem -> Bool
wasManagedAtHomeByDiagnosis encounter =
    case encounter.diagnosis of
        DiagnosisCovid19 ->
            -- HC was contacted, and it suggested home isolation
            -- or CHW monitoring.
            EverySet.member ContactedHealthCenter encounter.hcContactSigns
                && (EverySet.member HomeIsolation encounter.hcRecommendation
                        || EverySet.member ChwMonitoring encounter.hcRecommendation
                   )

        -- All others, but it must exclude NoAcuteIllnessDiagnosis - invoking function
        -- should be taking care of this.
        _ ->
            -- All that were not refered to HC are managed at home.
            not <| wasSentToHCByDiagnosis encounter


countAcuteIllnessCasesByPossibleDiagnosises : List AcuteIllnessDiagnosis -> Bool -> List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessCasesByPossibleDiagnosises possible whenFeverRecorded encounters =
    List.filter
        (\encounter ->
            let
                feverFilter =
                    if whenFeverRecorded then
                        encounter.feverRecorded

                    else
                        True
            in
            List.member encounter.diagnosis possible
                && feverFilter
        )
        encounters
        |> List.length



--
-- Acute illness - COVID functions.
--


countDiagnosedWithCovidCallsTo114 : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidCallsTo114 encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Covid19 diagnosis,
            -- and there was a call to 114.
            (encounter.diagnosis == DiagnosisCovid19)
                && EverySet.member Call114 encounter.call114Signs
        )
        encounters
        |> List.length


countDiagnosedWithCovidSentToHC : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidSentToHC encounters =
    -- Encounters which has produced Covid19 diagnosis,
    -- and patient was sent to health center.
    List.filter (.diagnosis >> (==) DiagnosisCovid19) encounters
        |> List.filter wasSentToHCByDiagnosis
        |> List.length


countDiagnosedWithCovidManagedAtHome : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidManagedAtHome encounters =
    -- Encounter which has produced Covid19 diagnosis,
    -- and it was decided to manage illness at home.
    List.filter (.diagnosis >> (==) DiagnosisCovid19) encounters
        |> List.filter wasManagedAtHomeByDiagnosis
        |> List.length



--
-- Acute illness - Malaria functions.
--


countDiagnosedWithMalaria : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithMalaria encounters =
    List.filter
        (\encounter ->
            List.member encounter.diagnosis
                [ DiagnosisMalariaComplicated
                , DiagnosisMalariaUncomplicated
                , DiagnosisMalariaUncomplicatedAndPregnant
                ]
        )
        encounters
        |> List.length


countUncomplicatedMalariaManagedByChw : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedMalariaManagedByChw encounters =
    List.filter
        (\encounter ->
            -- Enconter which has produced Uncomplicated Malaria diagnosis,
            -- and patient was not sent to health center.
            (encounter.diagnosis == DiagnosisMalariaUncomplicated)
                && not (EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
        )
        encounters
        |> List.length


countUncomplicatedMalariaAndPregnantSentToHC : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedMalariaAndPregnantSentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Uncomplicated Malaria and Pregnant
            -- diagnosis, and patient was sent to health center.
            (encounter.diagnosis == DiagnosisMalariaUncomplicatedAndPregnant)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
        )
        encounters
        |> List.length


countComplicatedMalariaSentToHC : List AcuteIllnessEncounterDataItem -> Int
countComplicatedMalariaSentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Comlpicated Malaria diagnosis,
            -- and patient was sent to health center.
            (encounter.diagnosis == DiagnosisMalariaComplicated)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
        )
        encounters
        |> List.length


countResolvedMalariaCasesForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countResolvedMalariaCasesForSelectedMonth selectedDate itemsList =
    List.filter
        (\item ->
            case item.dateConcluded of
                Nothing ->
                    False

                Just dateConcluded ->
                    -- Illness that was resolved at selected month,
                    -- and had a Malaria diagnosis.
                    withinSelectedMonth selectedDate dateConcluded
                        && List.member item.diagnosis
                            [ DiagnosisMalariaComplicated
                            , DiagnosisMalariaUncomplicated
                            , DiagnosisMalariaUncomplicatedAndPregnant
                            ]
        )
        itemsList
        |> List.length



--
-- Acute illness - Gastro functions.
--


countDiagnosedWithGI : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithGI encounters =
    List.filter
        (\encounter ->
            List.member encounter.diagnosis
                [ DiagnosisGastrointestinalInfectionComplicated
                , DiagnosisGastrointestinalInfectionUncomplicated
                ]
        )
        encounters
        |> List.length


countUncomplicatedGIManagedByChw : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedGIManagedByChw encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Uncomlicated GI diagnosis,
            -- and patient was not sent to health center.
            (encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated)
                && not (EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
        )
        encounters
        |> List.length


countComplicatedGISentToHC : List AcuteIllnessEncounterDataItem -> Int
countComplicatedGISentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Comlicated GI diagnosis,
            -- and patient was sent to health center.
            (encounter.diagnosis == DiagnosisGastrointestinalInfectionComplicated)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
        )
        encounters
        |> List.length


countResolvedGICasesForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countResolvedGICasesForSelectedMonth selectedDate itemsList =
    List.filter
        (\item ->
            case item.dateConcluded of
                Nothing ->
                    False

                Just dateConcluded ->
                    -- Illness that was resolved at selected month,
                    -- has outcome set, and had a GI diagnosis.
                    withinSelectedMonth selectedDate dateConcluded
                        && isJust item.outcome
                        && List.member item.diagnosis
                            [ DiagnosisGastrointestinalInfectionComplicated
                            , DiagnosisGastrointestinalInfectionUncomplicated
                            ]
        )
        itemsList
        |> List.length



--
-- ANC functions.
--


countNewlyIdentifiedPregananciesForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countNewlyIdentifiedPregananciesForSelectedMonth selectedDate itemsList =
    itemsList
        |> List.filter (.created >> withinSelectedMonth selectedDate)
        |> List.length


countCurrentlyPregnantForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> Int
countCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList =
    getCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList
        |> List.length


getCurrentlyPregnantForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> List PrenatalDataItem
getCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month selectedDate

        dateFirstDayOfNextMonth =
            Date.ceiling Date.Month selectedDate
    in
    itemsList
        |> List.filter
            (\item ->
                let
                    -- Pregnancy was tracked during current month, or before.
                    createdDateFilter =
                        Date.compare item.created dateFirstDayOfNextMonth == LT

                    -- Expected date exists, and is set to 3 weeks or less,
                    -- before the beggining of the range.
                    expectedDateConcludedFilter =
                        item.expectedDateConcluded
                            |> Maybe.map
                                (\expectedDateConcluded ->
                                    Date.diff Date.Weeks expectedDateConcluded dateFirstDayOfSelectedMonth <= 3
                                )
                            |> Maybe.withDefault False

                    -- No date concluded, or it's set within month range, or after that.
                    actualDateConcludedFilter =
                        case item.dateConcluded of
                            Just dateConcluded ->
                                let
                                    compareResult =
                                        Date.compare dateFirstDayOfSelectedMonth dateConcluded
                                in
                                compareResult == LT || compareResult == EQ

                            Nothing ->
                                True
                in
                createdDateFilter && expectedDateConcludedFilter && actualDateConcludedFilter
            )


countCurrentlyPregnantWithDangerSignsForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> Int
countCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate itemsList =
    getCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate itemsList
        |> List.length


getCurrentlyPregnantWithDangerSignsForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> List PrenatalDataItem
getCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate itemsList =
    getCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList
        |> List.filter
            (.encounters
                >> List.any
                    (\encounter ->
                        -- Active pregnancy that got an encounter at
                        -- selected month, where danger signs where recorded.
                        withinSelectedMonth selectedDate encounter.startDate
                            && (not <| EverySet.isEmpty encounter.dangerSigns)
                            && (encounter.dangerSigns /= EverySet.singleton NoDangerSign)
                    )
            )


countNewbornForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countNewbornForSelectedMonth selectedDate itemsList =
    itemsList
        |> List.filter
            (\item ->
                Maybe.map2
                    (\dateConcluded outcome ->
                        -- Live baby born within selected month.
                        (outcome == Pregnancy OutcomeLiveAtTerm || outcome == Pregnancy OutcomeLivePreTerm)
                            && withinSelectedMonth selectedDate dateConcluded
                    )
                    item.dateConcluded
                    item.outcome
                    |> Maybe.withDefault False
            )
        |> List.length


countPregnanciesDueWithin4MonthsForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countPregnanciesDueWithin4MonthsForSelectedMonth selectedDate itemsList =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month selectedDate

        dateLastDayOfSelectedMonth =
            Date.ceiling Date.Month selectedDate
                |> Date.add Date.Days -1
    in
    itemsList
        |> List.filter
            (\item ->
                let
                    -- Either pregnanacy is not concluded, or, it was concluded
                    -- after selected months has ended.
                    dateConcludedFilter =
                        case item.dateConcluded of
                            Just dateConcluded ->
                                Date.compare dateConcluded dateLastDayOfSelectedMonth == GT

                            Nothing ->
                                True

                    -- Expected date exists, is within selected month or
                    -- latter than that, and within 120 days from the
                    -- beginning of selected month.
                    expectedDateConcludedFilter =
                        item.expectedDateConcluded
                            |> Maybe.map
                                (\expectedDateConcluded ->
                                    let
                                        compareResult =
                                            Date.compare expectedDateConcluded dateFirstDayOfSelectedMonth
                                    in
                                    (compareResult == GT || compareResult == EQ)
                                        && (Date.diff Date.Days dateFirstDayOfSelectedMonth expectedDateConcluded <= 120)
                                )
                            |> Maybe.withDefault False
                in
                dateConcludedFilter && expectedDateConcludedFilter
            )
        |> List.length


countDeliveriesAtLocationForSelectedMonth : NominalDate -> DeliveryLocation -> List PrenatalDataItem -> Int
countDeliveriesAtLocationForSelectedMonth selectedDate location itemsList =
    itemsList
        |> List.filter
            (\item ->
                Maybe.map2
                    (\dateConcluded deliveryLocation ->
                        -- Live baby born within selected month.
                        withinSelectedMonth selectedDate dateConcluded
                            && (deliveryLocation == location)
                    )
                    item.dateConcluded
                    item.deliveryLocation
                    |> Maybe.withDefault False
            )
        |> List.length



--
-- Case management functions.
--


getFollowUpsTotals : Language -> NominalDate -> NominalDate -> ModelIndexedDb -> VillageId -> FollowUpMeasurements -> ( Int, Int, Int )
getFollowUpsTotals language currentDate limitDate db villageId followUps =
    let
        followUpsToLimitDate =
            filterFollowUpMeasurementsByLimitDate limitDate followUps

        nutritionFollowUps =
            generateNutritionFollowUps db followUpsToLimitDate
                |> filterVillageResidents villageId identity db

        nutritionEntries =
            generateNutritionFollowUpEntries language currentDate limitDate nutritionFollowUps db

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps db followUpsToLimitDate
                |> filterVillageResidents villageId Tuple.second db

        acuteIllnessEntries =
            generateAcuteIllnessFollowUpEntries language currentDate limitDate acuteIllnessFollowUps db

        prenatalFollowUps =
            generatePrenatalFollowUps db followUpsToLimitDate
                |> filterVillageResidents villageId Tuple.second db

        prenatalEntries =
            generatePrenatalFollowUpEntries language currentDate limitDate prenatalFollowUps db
    in
    ( List.length nutritionEntries
    , List.length acuteIllnessEntries
    , List.length prenatalEntries
    )


getAcuteIllnessFollowUpsBreakdownByDiagnosis : Language -> NominalDate -> NominalDate -> ModelIndexedDb -> VillageId -> FollowUpMeasurements -> ( Int, Int, Int )
getAcuteIllnessFollowUpsBreakdownByDiagnosis language currentDate limitDate db villageId followUps =
    let
        acuteIllnessFollowUps =
            filterFollowUpMeasurementsByLimitDate limitDate followUps
                |> generateAcuteIllnessFollowUps db
                |> filterVillageResidents villageId Tuple.second db

        acuteIllnessEntries =
            generateAcuteIllnessFollowUpEntries language currentDate limitDate acuteIllnessFollowUps db

        covidEntries =
            List.filter (.diagnosis >> (==) DiagnosisCovid19) acuteIllnessEntries

        malariaEntries =
            List.filter
                (\entry ->
                    List.member entry.diagnosis
                        [ DiagnosisMalariaComplicated
                        , DiagnosisMalariaUncomplicated
                        , DiagnosisMalariaUncomplicatedAndPregnant
                        ]
                )
                acuteIllnessEntries

        giEntries =
            List.filter
                (\entry ->
                    List.member entry.diagnosis
                        [ DiagnosisGastrointestinalInfectionComplicated
                        , DiagnosisGastrointestinalInfectionUncomplicated
                        ]
                )
                acuteIllnessEntries
    in
    ( List.length covidEntries
    , List.length malariaEntries
    , List.length giEntries
    )


filterFollowUpMeasurementsByLimitDate : NominalDate -> FollowUpMeasurements -> FollowUpMeasurements
filterFollowUpMeasurementsByLimitDate limitDate followUpMeasurements =
    { followUpMeasurements
        | nutritionGroup = filterByLimitDate limitDate followUpMeasurements.nutritionGroup
        , nutritionIndividual = filterByLimitDate limitDate followUpMeasurements.nutritionIndividual
        , acuteIllness = filterByLimitDate limitDate followUpMeasurements.acuteIllness
        , prenatal = filterByLimitDate limitDate followUpMeasurements.prenatal
    }


generateNutritionPageData : NominalDate -> DashboardStats -> ModelIndexedDb -> FilterProgramType -> Maybe VillageId -> NutritionPageData
generateNutritionPageData currentDate stats db programTypeFilter selectedVillageFilter =
    let
        currentPeriodStats =
            filterStatsWithinPeriod currentDate OneYear stats

        totalBeneficiariesMonthlyDuringPastYear =
            generateTotalBeneficiariesMonthlyDuringPastYear currentDate stats

        emptyTotalBeneficiariesDict =
            List.repeat 12 emptyTotalBeneficiaries
                |> List.indexedMap (\index empty -> ( index + 1, empty ))
                |> Dict.fromList

        caseNutritionTotalsThisYear =
            stats.caseManagement.thisYear
                |> List.map (.nutrition >> generateCaseNutritionTotals)

        caseNutritionTotalsLastYear =
            stats.caseManagement.lastYear
                |> List.map (.nutrition >> generateCaseNutritionTotals)

        totalsGraphData =
            caseNutritionTotalsThisYear
                |> List.foldl accumCaseNutritionTotals emptyTotalBeneficiariesDict
                |> applyTotalBeneficiariesDenomination totalBeneficiariesMonthlyDuringPastYear

        newCasesGraphData =
            stats.caseManagement.thisYear
                |> List.map (.nutrition >> generateCaseNutritionNewCases currentDate)
                |> List.foldl accumCaseNutritionTotals emptyTotalBeneficiariesDict
                |> applyTotalBeneficiariesDenomination totalBeneficiariesMonthlyDuringPastYear
    in
    { caseNutritionTotalsThisYear = caseNutritionTotalsThisYear
    , caseNutritionTotalsLastYear = caseNutritionTotalsLastYear
    , totalEncounters = generateTotalEncounters currentPeriodStats.totalEncounters programTypeFilter selectedVillageFilter
    , totalsGraphData = totalsGraphData
    , newCasesGraphData = newCasesGraphData
    }


generateTotalBeneficiariesMonthlyDuringPastYear :
    NominalDate
    -> DashboardStats
    -> Dict Int Int
generateTotalBeneficiariesMonthlyDuringPastYear currentDate stats =
    let
        currentMonth =
            Date.month currentDate
                |> Date.monthToNumber

        ( thisYear, lastYear ) =
            List.repeat 12 0
                |> List.indexedMap (\index _ -> index + 1)
                |> List.Extra.splitAt currentMonth

        orderedList =
            (lastYear ++ thisYear)
                |> List.reverse
                |> List.indexedMap
                    (\index month ->
                        let
                            maxJoinDate =
                                Date.add Months (-1 * index) currentDate
                                    |> Date.ceiling Date.Month
                                    |> Date.add Days -1

                            minGraduationDate =
                                Date.add Months (-1 * index) currentDate
                                    |> Date.floor Date.Month

                            totalBeneficiaries =
                                stats.childrenBeneficiaries
                                    |> List.filterMap
                                        (\child ->
                                            if
                                                (Date.compare child.memberSince maxJoinDate == LT)
                                                    && (Date.compare minGraduationDate child.graduationDate == LT)
                                            then
                                                Just child.identifier

                                            else
                                                Nothing
                                        )
                                    -- We want to get unique participants.
                                    |> EverySet.fromList
                                    |> EverySet.size
                        in
                        ( month, totalBeneficiaries )
                    )
                |> Dict.fromList
    in
    orderedList


generateCaseNutritionTotals : CaseNutrition -> CaseNutritionTotal
generateCaseNutritionTotals caseNutrition =
    let
        generateTotals nutrition =
            Dict.toList nutrition
                |> List.filterMap
                    (\( month, nutritionValue ) ->
                        if month == 13 then
                            Nothing

                        else
                            case nutritionValue.class of
                                Backend.Dashboard.Model.Moderate ->
                                    Just ( month, Backend.Dashboard.Model.Nutrition 0 1 )

                                Backend.Dashboard.Model.Severe ->
                                    Just ( month, Backend.Dashboard.Model.Nutrition 1 0 )

                                _ ->
                                    Just ( month, Backend.Dashboard.Model.Nutrition 0 0 )
                    )
                |> Dict.fromList
    in
    { stunting = generateTotals caseNutrition.stunting
    , underweight = generateTotals caseNutrition.underweight
    , wasting = generateTotals caseNutrition.wasting
    , muac = generateTotals caseNutrition.muac
    , nutritionSigns = generateTotals caseNutrition.nutritionSigns
    }


generateCaseNutritionNewCases : NominalDate -> CaseNutrition -> CaseNutritionTotal
generateCaseNutritionNewCases currentDate caseNutrition =
    let
        currentMonth =
            Date.month currentDate
                |> Date.monthToNumber

        generateTotals nutrition =
            let
                sorted =
                    Dict.toList nutrition
                        |> List.sortBy Tuple.first

                oneBeforeFirst =
                    List.reverse sorted
                        |> List.head

                ( thisYear, lastYear ) =
                    List.take 12 sorted
                        |> List.Extra.splitAt currentMonth

                yearData =
                    lastYear ++ thisYear

                yearDataShiftedLeft =
                    oneBeforeFirst
                        |> Maybe.map (\beforeFirst -> beforeFirst :: List.take 11 yearData)
                        |> Maybe.withDefault yearData
            in
            List.map2
                (\( month, nutritionValue ) ( _, previousNutritionValue ) ->
                    case nutritionValue.class of
                        Backend.Dashboard.Model.Moderate ->
                            if previousNutritionValue.class /= Backend.Dashboard.Model.Moderate then
                                ( month, Backend.Dashboard.Model.Nutrition 0 1 )

                            else
                                ( month, Backend.Dashboard.Model.Nutrition 0 0 )

                        Backend.Dashboard.Model.Severe ->
                            if previousNutritionValue.class /= Backend.Dashboard.Model.Severe then
                                ( month, Backend.Dashboard.Model.Nutrition 1 0 )

                            else
                                ( month, Backend.Dashboard.Model.Nutrition 0 0 )

                        _ ->
                            ( month, Backend.Dashboard.Model.Nutrition 0 0 )
                )
                yearData
                yearDataShiftedLeft
                |> Dict.fromList
    in
    { stunting = generateTotals caseNutrition.stunting
    , underweight = generateTotals caseNutrition.underweight
    , wasting = generateTotals caseNutrition.wasting
    , muac = generateTotals caseNutrition.muac
    , nutritionSigns = generateTotals caseNutrition.nutritionSigns
    }


accumCaseNutritionTotals : CaseNutritionTotal -> Dict Int TotalBeneficiaries -> Dict Int TotalBeneficiaries
accumCaseNutritionTotals totals dict =
    Dict.toList dict
        |> List.map
            (\( key, accum ) ->
                let
                    stunting =
                        Dict.get key totals.stunting
                            |> Maybe.map
                                (\totalsStunting ->
                                    Nutrition (totalsStunting.severeNutrition + accum.stunting.severeNutrition) (totalsStunting.moderateNutrition + accum.stunting.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.stunting

                    underweight =
                        Dict.get key totals.underweight
                            |> Maybe.map
                                (\totalsUnderweight ->
                                    Nutrition (totalsUnderweight.severeNutrition + accum.underweight.severeNutrition) (totalsUnderweight.moderateNutrition + accum.underweight.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.underweight

                    wasting =
                        Dict.get key totals.wasting
                            |> Maybe.map
                                (\totalsWasting ->
                                    Nutrition (totalsWasting.severeNutrition + accum.wasting.severeNutrition) (totalsWasting.moderateNutrition + accum.wasting.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.wasting

                    muac =
                        Dict.get key totals.muac
                            |> Maybe.map
                                (\totalsMuac ->
                                    Nutrition (totalsMuac.severeNutrition + accum.muac.severeNutrition) (totalsMuac.moderateNutrition + accum.muac.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.muac
                in
                ( key, TotalBeneficiaries stunting underweight wasting muac )
            )
        |> Dict.fromList


applyTotalBeneficiariesDenomination : Dict Int Int -> Dict Int TotalBeneficiaries -> Dict Int TotalBeneficiaries
applyTotalBeneficiariesDenomination beneficiariesPerMonthsDict totalBeneficiariesDict =
    let
        applyDenomination number denominator =
            ceiling (100 * toFloat number / toFloat denominator)
    in
    totalBeneficiariesDict
        |> Dict.map
            (\month totalBeneficiaries ->
                Dict.get month beneficiariesPerMonthsDict
                    |> Maybe.map
                        (\total ->
                            { stunting =
                                Nutrition (applyDenomination totalBeneficiaries.stunting.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.stunting.moderateNutrition total)
                            , underweight =
                                Nutrition (applyDenomination totalBeneficiaries.underweight.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.underweight.moderateNutrition total)
                            , wasting =
                                Nutrition (applyDenomination totalBeneficiaries.wasting.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.wasting.moderateNutrition total)
                            , muac =
                                Nutrition (applyDenomination totalBeneficiaries.muac.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.muac.moderateNutrition total)
                            }
                        )
                    |> Maybe.withDefault totalBeneficiaries
            )


filterStatsWithinPeriod : NominalDate -> FilterPeriod -> DashboardStats -> DashboardStats
filterStatsWithinPeriod currentDate period stats =
    filterStatsByPeriod isBetween currentDate period stats


filterStatsOutsidePeriod : NominalDate -> FilterPeriod -> DashboardStats -> DashboardStats
filterStatsOutsidePeriod currentDate period stats =
    let
        outside start end date =
            isBetween start end date |> not
    in
    filterStatsByPeriod outside currentDate period stats


{-| Filter stats to match the selected period.
-}
filterStatsByPeriod : (NominalDate -> NominalDate -> NominalDate -> Bool) -> NominalDate -> FilterPeriod -> DashboardStats -> DashboardStats
filterStatsByPeriod fiterFunc currentDate period stats =
    let
        ( startDate, endDate ) =
            case period of
                OneYear ->
                    ( Date.add Years -1 currentDate, Date.add Days 1 currentDate )

                ThisMonth ->
                    -- From beginning of the month to this day.
                    ( Date.floor Date.Month currentDate, Date.add Days 1 currentDate )

                LastMonth ->
                    -- From the beginning of last month to the end of last month.
                    ( Date.add Months -1 currentDate
                        |> Date.floor Date.Month
                    , Date.add Months -1 currentDate
                        |> Date.ceiling Date.Month
                        -- We have to remove a day because the "ceiling" function for some reason is going up to the
                        -- first day of the next month.
                        |> Date.add Days -1
                    )

                ThreeMonthsAgo ->
                    -- From the beginning of 3 months ago to the end of 3 months ago.
                    ( Date.add Months -2 currentDate
                        |> Date.floor Date.Month
                    , Date.add Months -2 currentDate
                        |> Date.ceiling Date.Month
                        -- We have to remove a day because the "ceiling" function for some reason is going up to the
                        -- first day of the next month.
                        |> Date.add Days -1
                    )

        filterPartial =
            fiterFunc startDate endDate

        childrenBeneficiariesUpdated =
            stats.childrenBeneficiaries
                |> List.filter (\child -> filterPartial child.memberSince)

        familyPlanningUpdated =
            stats.familyPlanning
                |> List.filter (\familyPlanning -> filterPartial familyPlanning.created)

        completedPrograms =
            stats.completedPrograms
                |> List.filter (\completedProgram -> filterPartial completedProgram.expectedDate)

        missedSessions =
            stats.missedSessions
                |> List.filter (\missedSession -> filterPartial missedSession.expectedDate)
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiariesUpdated
        , familyPlanning = familyPlanningUpdated
        , completedPrograms = completedPrograms
        , missedSessions = missedSessions
    }


{-| Filter stats to match the selected gender.
-}
filterStatsByGender : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByGender currentDate model stats =
    { stats
        | childrenBeneficiaries = applyGenderFilter model stats.childrenBeneficiaries
        , completedPrograms = applyGenderFilter model stats.completedPrograms
        , missedSessions = applyGenderFilter model stats.missedSessions
    }


applyGenderFilter : Model -> List { a | gender : Backend.Person.Model.Gender } -> List { a | gender : Backend.Person.Model.Gender }
applyGenderFilter model list =
    List.filter
        (\item ->
            case ( item.gender, model.beneficiariesGender ) of
                ( Backend.Person.Model.Male, Pages.Dashboard.Model.Boys ) ->
                    True

                ( Backend.Person.Model.Female, Pages.Dashboard.Model.Girls ) ->
                    True

                _ ->
                    False
        )
        list



--
-- Helper functions.
--


withinSelectedMonth : NominalDate -> NominalDate -> Bool
withinSelectedMonth selectedDate date =
    let
        month =
            Date.monthNumber selectedDate

        year =
            Date.year selectedDate
    in
    (Date.monthNumber date == month)
        && (Date.year date == year)


getSelectedDate : NominalDate -> Model -> NominalDate
getSelectedDate currentDate model =
    Date.add Date.Months (-1 * model.monthGap) currentDate


childrenBeneficiariesByProgramType : ProgramType -> Dict ProgramType (List ChildrenBeneficiariesStats) -> List ChildrenBeneficiariesStats
childrenBeneficiariesByProgramType programType childrenBeneficiaries =
    Dict.get programType childrenBeneficiaries
        |> Maybe.withDefault []


filterByLimitDate : NominalDate -> Dict id { a | dateMeasured : NominalDate } -> Dict id { a | dateMeasured : NominalDate }
filterByLimitDate limitDate followUps =
    Dict.filter (\_ followUp -> Date.compare followUp.dateMeasured limitDate == LT) followUps
