module Pages.Dashboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.Dashboard.Model
    exposing
        ( AcuteIllnessDataItem
        , AcuteIllnessEncounterDataItem
        , AssembledData
        , CaseManagement
        , CaseNutrition
        , CaseNutritionTotal
        , ChildScoreboardDataItem
        , ChildScoreboardEncounterDataItem
        , ChildrenBeneficiariesStats
        , DashboardStats
        , DashboardStatsRaw
        , EducationSessionData
        , NCDDataItem
        , NCDEncounterDataItem
        , Nutrition
        , NutritionPageData
        , NutritionStatus
        , NutritionValue
        , PMTCTDataItem
        , PatientDetails
        , Periods
        , PersonIdentifier
        , PrenatalDataItem
        , PrenatalEncounterDataItem
        , ProgramType(..)
        , SPVDataItem
        , SPVEncounterDataItem
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
        , Gender(..)
        , HCContactSign(..)
        , HCRecommendation(..)
        , MedicalCondition(..)
        , Recommendation114(..)
        , SendToHCSign(..)
        , TestExecutionNote(..)
        , TestResult(..)
        , VaccineDose(..)
        , WellChildVaccineType(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.Village.Utils exposing (resolveVillageResidents)
import Date exposing (Unit(..), isBetween)
import EverySet
import Gizra.NominalDate exposing (NominalDate, toLastDayOfMonth)
import List.Extra
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils exposing (allVaccineTypes)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Utils
    exposing
        ( fillPersonName
        , filterFollowUpsOfResidents
        , generateAcuteIllnessFollowUps
        , generateNutritionFollowUps
        , generatePrenatalFollowUps
        )
import Pages.GlobalCaseManagement.View
    exposing
        ( generateAcuteIllnessFollowUpEntries
        , generateNutritionFollowUpEntries
        , generatePrenatalFollowUpEntries
        )
import SyncManager.Model exposing (Site)
import Translate exposing (Language)
import Utils.NominalDate exposing (sortByDate, sortByDateDesc)


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
    , acuteIllnessData = generateFilteredData .acuteIllnessData stats selectedVillageFilter
    , prenatalData = generateFilteredData .prenatalData stats selectedVillageFilter
    , ncdData = generateFilteredData .ncdData stats selectedVillageFilter
    , pmtctData = generateFilteredData .pmtctData stats selectedVillageFilter
    , spvData = generateFilteredData .spvData stats selectedVillageFilter
    , childScoreboardData = generateFilteredData .childScoreboardData stats selectedVillageFilter
    , nutritionIndividualData = generateFilteredData .nutritionIndividualData stats selectedVillageFilter
    , nutritionGroupData = generateFilteredData .nutritionGroupData stats selectedVillageFilter
    , nutritionPageData = generateNutritionPageData currentDate filteredStats db programTypeFilter selectedVillageFilter
    , groupEducationData = generateGroupEducationData stats selectedVillageFilter
    , healthCenterVillages = Dict.keys stats.villagesWithResidents
    , patientsDetails = generateFilteredPatientsDetails stats selectedVillageFilter
    }


generateGroupEducationData : DashboardStatsRaw -> Maybe VillageId -> List EducationSessionData
generateGroupEducationData stats selectedVillageFilter =
    case selectedVillageFilter of
        Just villageId ->
            Dict.get villageId stats.groupEducationData
                |> Maybe.withDefault []

        Nothing ->
            Dict.values stats.groupEducationData
                |> List.concat


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
    , childrenBeneficiaries =
        applyProgramTypeAndResidentsFilters stats.villagesWithResidents
            programTypeFilter
            selectedVillageFilter
            stats.childrenBeneficiaries
    , completedPrograms = stats.completedPrograms
    , familyPlanning = stats.familyPlanning
    , missedSessions = stats.missedSessions
    , totalEncounters = stats.totalEncounters
    , timestamp = stats.timestamp
    }


generateFilteredPatientsDetails :
    DashboardStatsRaw
    -> Maybe VillageId
    -> Dict PersonIdentifier PatientDetails
generateFilteredPatientsDetails stats selectedVillageFilter =
    Maybe.andThen (\villageId -> Dict.get villageId stats.villagesWithResidents) selectedVillageFilter
        |> Maybe.map (\residents -> Dict.filter (\key _ -> List.member key residents) stats.patientsDetails)
        |> Maybe.withDefault stats.patientsDetails


generateFilteredData :
    (DashboardStatsRaw -> List { a | identifier : PersonIdentifier })
    -> DashboardStatsRaw
    -> Maybe VillageId
    -> List { a | identifier : PersonIdentifier }
generateFilteredData getDataFunc stats selectedVillageFilter =
    let
        beforeFiltering =
            getDataFunc stats
    in
    Maybe.andThen (\villageId -> Dict.get villageId stats.villagesWithResidents) selectedVillageFilter
        |> Maybe.map (\residents -> List.filter (\item -> List.member item.identifier residents) beforeFiltering)
        |> Maybe.withDefault beforeFiltering


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

                ( Just _, Nothing ) ->
                    first

                ( Nothing, Just _ ) ->
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
-- Global functions.
--


getEncountersForSelectedMonth :
    NominalDate
    -> List { dataItem | encounters : List { ecounterDataItem | startDate : NominalDate } }
    -> List { ecounterDataItem | startDate : NominalDate }
getEncountersForSelectedMonth dateLastDayOfSelectedMonth =
    List.concatMap .encounters
        >> List.filter (.startDate >> withinSelectedMonth dateLastDayOfSelectedMonth)



--
-- Acute illness - Overview functions.
--


countAcuteIllnessAssessments : List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessAssessments encounters =
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
only those that Yes answered to question about patien being referred to HC.
-}
wasSentToHCByDiagnosis : AcuteIllnessEncounterDataItem -> Bool
wasSentToHCByDiagnosis encounter =
    case encounter.diagnosis of
        DiagnosisCovid19Suspect ->
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
            -- All that were referred sent to HC.
            EverySet.member ReferToHealthCenter encounter.sendToHCSigns


{-| There's a difference betweeen non Covid and Covid cases, when making
a decision if to manage illness at home.
Covid case has a specific set of parameters, while non Covid has a simple logic -
if patient was not sent to HC, then it was managed at home.
-}
wasManagedAtHomeByDiagnosis : AcuteIllnessEncounterDataItem -> Bool
wasManagedAtHomeByDiagnosis encounter =
    case encounter.diagnosis of
        DiagnosisCovid19Suspect ->
            -- HC was contacted, and it suggested home isolation
            -- or CHW monitoring.
            EverySet.member ContactedHealthCenter encounter.hcContactSigns
                && (EverySet.member HomeIsolation encounter.hcRecommendation
                        || EverySet.member ChwMonitoring encounter.hcRecommendation
                   )

        -- All others, but it must exclude NoAcuteIllnessDiagnosis - invoking function
        -- should be taking care of this.
        _ ->
            -- All that were not referred to HC are managed at home.
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
            (encounter.diagnosis == DiagnosisCovid19Suspect)
                && EverySet.member Call114 encounter.call114Signs
        )
        encounters
        |> List.length


countDiagnosedWithCovidSentToHC : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidSentToHC encounters =
    -- Encounters which has produced Covid19 diagnosis,
    -- and patient was sent to health center.
    List.filter (.diagnosis >> (==) DiagnosisCovid19Suspect) encounters
        |> List.filter wasSentToHCByDiagnosis
        |> List.length


countDiagnosedWithCovidManagedAtHome : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidManagedAtHome encounters =
    -- Encounter which has produced Covid19 diagnosis,
    -- and it was decided to manage illness at home.
    List.filter (.diagnosis >> (==) DiagnosisCovid19Suspect) encounters
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


countUncomplicatedMalariaSentToHC : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedMalariaSentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Uncomplicated Malaria diagnosis,
            -- patient is bellow age of 6 months and
            -- patient was sent to health center.
            (encounter.diagnosis == DiagnosisMalariaUncomplicated)
                && (encounter.ageInMonths < 6)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
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
countResolvedMalariaCasesForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (\illness ->
            case illness.dateConcluded of
                Nothing ->
                    False

                Just dateConcluded ->
                    -- Illness that was resolved at selected month,
                    -- and had a Malaria diagnosis.
                    withinSelectedMonth dateLastDayOfSelectedMonth dateConcluded
                        && List.member illness.diagnosis
                            [ DiagnosisMalariaComplicated
                            , DiagnosisMalariaUncomplicated
                            , DiagnosisMalariaUncomplicatedAndPregnant
                            ]
        )
        >> List.length



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
countResolvedGICasesForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (\illness ->
            case illness.dateConcluded of
                Nothing ->
                    False

                Just dateConcluded ->
                    -- Illness that was resolved at selected month,
                    -- has outcome set, and had a GI diagnosis.
                    withinSelectedMonth dateLastDayOfSelectedMonth dateConcluded
                        && isJust illness.outcome
                        && List.member illness.diagnosis
                            [ DiagnosisGastrointestinalInfectionComplicated
                            , DiagnosisGastrointestinalInfectionUncomplicated
                            ]
        )
        >> List.length



--
-- ANC functions.
--


filterNewlyDiagnosesCasesForSelectedMonth : NominalDate -> List PrenatalDiagnosis -> List PrenatalDataItem -> List PrenatalDataItem
filterNewlyDiagnosesCasesForSelectedMonth dateLastDayOfSelectedMonth diagnoses =
    List.filter
        (\pregnancy ->
            let
                matchDates =
                    List.filterMap
                        (\encounter ->
                            if
                                EverySet.toList encounter.diagnoses
                                    |> List.any (\diagnosis -> List.member diagnosis diagnoses)
                            then
                                Just encounter.startDate

                            else
                                Nothing
                        )
                        pregnancy.encounters
                        |> List.sortWith Date.compare
            in
            List.head matchDates
                |> Maybe.map (withinSelectedMonth dateLastDayOfSelectedMonth)
                |> Maybe.withDefault False
        )


filterNewlyDiagnosesMalnutritionForSelectedMonth : NominalDate -> List PrenatalDataItem -> List PrenatalDataItem
filterNewlyDiagnosesMalnutritionForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (\pregnancy ->
            let
                matchDates =
                    List.filterMap
                        (\encounter ->
                            Maybe.andThen
                                (\muac ->
                                    if muac < 21 then
                                        Just encounter.startDate

                                    else
                                        Nothing
                                )
                                encounter.muac
                        )
                        pregnancy.encounters
                        |> List.sortWith Date.compare
            in
            List.head matchDates
                |> Maybe.map (withinSelectedMonth dateLastDayOfSelectedMonth)
                |> Maybe.withDefault False
        )


isNurseEncounter : PrenatalEncounterDataItem -> Bool
isNurseEncounter encounter =
    List.member encounter.encounterType [ NurseEncounter, NursePostpartumEncounter ]


countNewlyIdentifiedPregananciesForSelectedMonth : NominalDate -> Bool -> List PrenatalDataItem -> Int
countNewlyIdentifiedPregananciesForSelectedMonth dateLastDayOfSelectedMonth isChw itemsList =
    let
        ( newByNurse, newByChw ) =
            List.partition
                (\pregnancy ->
                    List.sortWith (sortByDate .startDate) pregnancy.encounters
                        |> List.head
                        |> Maybe.map isNurseEncounter
                        |> Maybe.withDefault False
                )
                itemsList
    in
    if isChw then
        -- Number of pregnancies where first encounter was performed by CHW.
        List.filter (.created >> withinSelectedMonth dateLastDayOfSelectedMonth) newByChw
            |> List.length

    else
        -- Number of pregnancies where first encounter was performed by nurse.
        List.filter (.created >> withinSelectedMonth dateLastDayOfSelectedMonth) newByNurse
            |> List.length


countCurrentlyPregnantForSelectedMonth : NominalDate -> Bool -> List PrenatalDataItem -> Int
countCurrentlyPregnantForSelectedMonth dateLastDayOfSelectedMonth isChw =
    getCurrentlyPregnantForSelectedMonth dateLastDayOfSelectedMonth isChw
        >> List.length


getCurrentlyPregnantForSelectedMonth : NominalDate -> Bool -> List PrenatalDataItem -> List PrenatalDataItem
getCurrentlyPregnantForSelectedMonth dateLastDayOfSelectedMonth isChw =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month dateLastDayOfSelectedMonth

        facilityFilter encounters =
            if isChw then
                List.any (isNurseEncounter >> not) encounters

            else
                List.any isNurseEncounter encounters
    in
    List.filter
        (\pregnancy ->
            let
                -- Pregnancy was tracked during current month, or before.
                createdDateFilter =
                    not <| Date.compare dateLastDayOfSelectedMonth pregnancy.created == GT

                -- Expected date exists, and is set to 3 weeks or less,
                -- before the beggining of the range.
                expectedDateConcludedFilter =
                    pregnancy.expectedDateConcluded
                        |> Maybe.map
                            (\expectedDateConcluded ->
                                Date.diff Date.Weeks expectedDateConcluded dateFirstDayOfSelectedMonth <= 3
                            )
                        |> Maybe.withDefault False

                -- No date concluded, or it's set within month range, or after that.
                actualDateConcludedFilter =
                    case pregnancy.dateConcluded of
                        Just dateConcluded ->
                            let
                                compareResult =
                                    Date.compare dateFirstDayOfSelectedMonth dateConcluded
                            in
                            compareResult == LT || compareResult == EQ

                        Nothing ->
                            True
            in
            facilityFilter pregnancy.encounters
                && createdDateFilter
                && expectedDateConcludedFilter
                && actualDateConcludedFilter
        )


{-| Danger signs is considered a permanent condition until it is "cleared".
That is if a patient had a danger sign last month and has not been seen,
she would still be counted. If she had a danger sign last month, but this month
she had an encounter and she has no danger signs, then she is not counted.
-}
countCurrentlyPregnantWithDangerSignsForSelectedMonth : NominalDate -> Bool -> List PrenatalDataItem -> Int
countCurrentlyPregnantWithDangerSignsForSelectedMonth dateLastDayOfSelectedMonth isChw =
    getCurrentlyPregnantForSelectedMonth dateLastDayOfSelectedMonth isChw
        >> List.filter
            (\pregnancy ->
                let
                    -- All encounters that took place till selected month (including).
                    encountersTillSelectedMonth =
                        let
                            dateFirstDayOfNextMonth =
                                Date.ceiling Date.Month dateLastDayOfSelectedMonth
                        in
                        List.filter
                            (\encounter ->
                                Date.compare encounter.startDate dateFirstDayOfNextMonth == LT
                            )
                            pregnancy.encounters
                in
                List.sortWith (sortByDateDesc .startDate) encountersTillSelectedMonth
                    -- Get last encounter, and check if there were danger signs or not.
                    |> List.head
                    |> Maybe.map
                        (\encounter ->
                            (not <| EverySet.isEmpty encounter.dangerSigns)
                                && (encounter.dangerSigns /= EverySet.singleton NoDangerSign)
                        )
                    |> Maybe.withDefault False
            )
        >> List.length


countNewbornForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countNewbornForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (\item ->
            Maybe.map2
                (\dateConcluded outcome ->
                    -- Live baby born within selected month.
                    (outcome == Pregnancy OutcomeLiveAtTerm || outcome == Pregnancy OutcomeLivePreTerm)
                        && withinSelectedMonth dateLastDayOfSelectedMonth dateConcluded
                )
                item.dateConcluded
                item.outcome
                |> Maybe.withDefault False
        )
        >> List.length


countPregnanciesDueWithin4MonthsForSelectedMonth : NominalDate -> Bool -> List PrenatalDataItem -> Int
countPregnanciesDueWithin4MonthsForSelectedMonth dateLastDayOfSelectedMonth isChw =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month dateLastDayOfSelectedMonth
    in
    List.filter
        (\pregnancy ->
            let
                -- For nurses, we want at least one encounter to
                -- be conducted by nurse at HC.
                -- For CHWs, at least one encounter performed by CHW.
                facilityFilter =
                    let
                        encountersTilldateLastDayOfSelectedMonth =
                            List.filter (\encounter -> not <| Date.compare encounter.startDate dateLastDayOfSelectedMonth == GT)
                                pregnancy.encounters
                    in
                    if isChw then
                        List.any (isNurseEncounter >> not) encountersTilldateLastDayOfSelectedMonth

                    else
                        List.any isNurseEncounter encountersTilldateLastDayOfSelectedMonth

                -- Either pregnanacy is not concluded, or, it was concluded
                -- after selected months has ended.
                dateConcludedFilter =
                    case pregnancy.dateConcluded of
                        Just dateConcluded ->
                            Date.compare dateConcluded dateLastDayOfSelectedMonth == GT

                        Nothing ->
                            True

                -- Expected date exists, is within selected month or
                -- latter than that, and within 120 days from the
                -- beginning of selected month.
                expectedDateConcludedFilter =
                    pregnancy.expectedDateConcluded
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
            facilityFilter && dateConcludedFilter && expectedDateConcludedFilter
        )
        >> List.length


{-| Only for Nurses.
-}
countPregnanciesWith4VisitsOrMoreForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countPregnanciesWith4VisitsOrMoreForSelectedMonth dateLastDayOfSelectedMonth =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month dateLastDayOfSelectedMonth
    in
    List.filter
        (\pregnancy ->
            let
                -- Either pregnanacy is not concluded, or, it was concluded
                -- after selected month has ended.
                dateConcludedFilter =
                    case pregnancy.dateConcluded of
                        Just dateConcluded ->
                            Date.compare dateConcluded dateLastDayOfSelectedMonth == GT

                        Nothing ->
                            True

                -- We want at least one encounter to  be conducted by nurse at HC.
                facilityFilter =
                    let
                        encountersTilldateLastDayOfSelectedMonth =
                            List.filter (\encounter -> not <| Date.compare encounter.startDate dateLastDayOfSelectedMonth == GT)
                                pregnancy.encounters
                    in
                    List.any isNurseEncounter encountersTilldateLastDayOfSelectedMonth
            in
            facilityFilter && dateConcludedFilter
        )
        >> List.length


{-| Pregnancies that a nurse have an encounter at selected month, where
patient was referred to hospital.
-}
countHospitalReferralsForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countHospitalReferralsForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    isNurseEncounter encounter
                        && withinSelectedMonth dateLastDayOfSelectedMonth encounter.startDate
                        && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
                )
        )
        >> List.length


countDeliveriesAtLocationForSelectedMonth : NominalDate -> DeliveryLocation -> List PrenatalDataItem -> Int
countDeliveriesAtLocationForSelectedMonth dateLastDayOfSelectedMonth location =
    List.filter
        (\pregnancy ->
            Maybe.map2
                (\dateConcluded deliveryLocation ->
                    -- Live baby born within selected month.
                    withinSelectedMonth dateLastDayOfSelectedMonth dateConcluded
                        && (deliveryLocation == location)
                )
                pregnancy.dateConcluded
                pregnancy.deliveryLocation
                |> Maybe.withDefault False
        )
        >> List.length



--
-- Case management functions.
--


getFollowUpsTotals : Language -> NominalDate -> NominalDate -> ModelIndexedDb -> VillageId -> FollowUpMeasurements -> ( Int, Int, Int )
getFollowUpsTotals language currentDate limitDate db villageId allFollowUps =
    let
        villageResidents =
            resolveVillageResidents villageId db

        followUps =
            filterFollowUpsOfResidents villageResidents allFollowUps

        nutritionFollowUps =
            generateNutritionFollowUps limitDate followUps
                |> fillPersonName identity db

        nutritionEntries =
            generateNutritionFollowUpEntries language limitDate nutritionFollowUps db

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps limitDate db followUps
                |> fillPersonName Tuple.second db

        acuteIllnessEntries =
            generateAcuteIllnessFollowUpEntries language currentDate limitDate acuteIllnessFollowUps db

        prenatalFollowUps =
            generatePrenatalFollowUps limitDate db followUps
                |> fillPersonName Tuple.second db

        prenatalEntries =
            generatePrenatalFollowUpEntries language currentDate limitDate prenatalFollowUps db
    in
    ( List.length nutritionEntries
    , List.length acuteIllnessEntries
    , List.length prenatalEntries
    )


getAcuteIllnessFollowUpsBreakdownByDiagnosis :
    Language
    -> NominalDate
    -> NominalDate
    -> ModelIndexedDb
    -> VillageId
    -> FollowUpMeasurements
    -> ( Int, Int, Int )
getAcuteIllnessFollowUpsBreakdownByDiagnosis language currentDate limitDate db villageId allFollowUps =
    let
        villageResidents =
            resolveVillageResidents villageId db

        followUps =
            filterFollowUpsOfResidents villageResidents allFollowUps

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps limitDate db followUps
                |> fillPersonName Tuple.second db

        acuteIllnessEntries =
            generateAcuteIllnessFollowUpEntries language currentDate limitDate acuteIllnessFollowUps db

        covidEntries =
            List.filter (.diagnosis >> (==) DiagnosisCovid19Suspect) acuteIllnessEntries

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
    in
    (lastYear ++ thisYear)
        |> List.reverse
        |> List.indexedMap
            (\index month ->
                let
                    maxJoinDate =
                        Date.add Months (-1 * index) currentDate
                            |> toLastDayOfMonth

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
            round (100 * toFloat number / toFloat denominator)
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
                        |> toLastDayOfMonth
                    )

                ThreeMonthsAgo ->
                    -- From the beginning of 3 months ago to the end of 3 months ago.
                    ( Date.add Months -2 currentDate
                        |> Date.floor Date.Month
                    , Date.add Months -2 currentDate
                        |> toLastDayOfMonth
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


applyGenderFilter : Model -> List { a | gender : Backend.Measurement.Model.Gender } -> List { a | gender : Backend.Measurement.Model.Gender }
applyGenderFilter model list =
    List.filter
        (\item ->
            case ( item.gender, model.beneficiariesGender ) of
                ( Backend.Measurement.Model.Male, Pages.Dashboard.Model.Boys ) ->
                    True

                ( Backend.Measurement.Model.Female, Pages.Dashboard.Model.Girls ) ->
                    True

                _ ->
                    False
        )
        list


{-| Counts all data items (representing NCD participants) that had Hypertension
diagnosis recorded during any of it's encounters.
-}
countTotalNumberOfPatientsWithHypertension : NominalDate -> List NCDDataItem -> Int
countTotalNumberOfPatientsWithHypertension dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> -- Take only encounters that were ran at current month
               -- or prior to that.
               List.filter (.startDate >> withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth)
            >> --  Generate dates of all encounters where Hypertension was diagnosed.
               generateHypertensionDiagnosisEncountersDates True
            >> List.isEmpty
            >> not
        )
        >> List.length


{-| Counts all data items (representing NCD participants) that had first Hypertension
diagnosis recorded during selected month.
Hypertension can be recorded as a diagnosis, or medical condition (from Co-Morbidities and
Outside care activities).
-}
countNewlyIdentifieHypertensionCasesForSelectedMonth : NominalDate -> List NCDDataItem -> Int
countNewlyIdentifieHypertensionCasesForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> --  Generate dates of all encounters where Hypertension was diagnosed.
               generateHypertensionDiagnosisEncountersDates False
            -- Take first date, which represents first diagnosis for Hypertension.
            >> List.head
            -- Check if it falls within selected month.
            >> Maybe.map (withinSelectedMonth dateLastDayOfSelectedMonth)
            >> Maybe.withDefault False
        )
        >> List.length


{-| Generate dates of all encounters where Hypertension was diagnosed.
Hypertension can be recorded as a diagnosis, or medical condition (from Co-Morbidities and
Outside care activities).
-}
generateHypertensionDiagnosisEncountersDates : Bool -> List NCDEncounterDataItem -> List NominalDate
generateHypertensionDiagnosisEncountersDates includingMedicalConditions =
    List.filterMap
        (\encounter ->
            let
                byDiagnosis =
                    List.any (\diagnosis -> EverySet.member diagnosis encounter.diagnoses)
                        [ DiagnosisHypertensionStage1, DiagnosisHypertensionStage2, DiagnosisHypertensionStage3 ]

                byMedicalConditions =
                    if includingMedicalConditions then
                        let
                            medicalConditions =
                                EverySet.union encounter.medicalConditions encounter.coMorbidities
                        in
                        List.any (\condition -> EverySet.member condition medicalConditions)
                            [ MedicalConditionHypertension, MedicalConditionPregnancyRelatedHypertension ]

                    else
                        False
            in
            if byDiagnosis || byMedicalConditions then
                Just encounter.startDate

            else
                Nothing
        )
        -- Sort by date ASC.
        >> List.sortWith Date.compare


{-| Counts all data items (representing NCD participants) that had Diabetes
diagnosis recorded during any of it's encounters.
-}
countTotalNumberOfPatientsWithDiabetes : NominalDate -> List NCDDataItem -> Int
countTotalNumberOfPatientsWithDiabetes dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> -- Take only encounters that were ran at current month
               -- or prior to that.
               List.filter (.startDate >> withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth)
            >> --  Generate dates of all encounters where Diabetes was diagnosed.
               generateDiabetesDiagnosisEncountersDates True
            >> List.isEmpty
            >> not
        )
        >> List.length


{-| Counts all data items (representing NCD participants) that had first Diabetes
diagnosis recorded during selected month.
-}
countNewlyIdentifiedDiabetesCasesForSelectedMonth : NominalDate -> List NCDDataItem -> Int
countNewlyIdentifiedDiabetesCasesForSelectedMonth dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> --  Generate dates of all encounters where Diabetes was diagnosed.
               generateDiabetesDiagnosisEncountersDates False
            -- Take first date, which represents first diagnosis for Diabetes.
            >> List.head
            -- Check if it falls within selected month.
            >> Maybe.map (withinSelectedMonth dateLastDayOfSelectedMonth)
            >> Maybe.withDefault False
        )
        >> List.length


{-| Counts all data items (representing ANC participants) that had Diabetes
diagnosis recorded during any of it's encounters.
-}
countTotalNumberOfPatientsWithGestationalDiabetes : NominalDate -> List PrenatalDataItem -> Int
countTotalNumberOfPatientsWithGestationalDiabetes dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> -- Take only encounters that were ran at current month
               -- or prior to that.
               List.filter (.startDate >> withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth)
            >> -- Check if any of them had Gestational Diabetes diagnosis.
               List.any
                (.diagnoses
                    >> EverySet.toList
                    >> List.any
                        (\diagnosis ->
                            List.member diagnosis
                                [ DiagnosisGestationalDiabetesInitialPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                        )
                )
        )
        -- Since one woman may have multiple pregnancies, we make sure
        -- that patient identifier is unique before we count the total.
        >> List.map .identifier
        >> EverySet.fromList
        >> EverySet.size


{-| Generate dates of all encounters where Diabetes was diagnosed.
Diabetes can be recorded as a diagnosis, or medical condition (from Co-Morbidities and
Outside care activities).
-}
generateDiabetesDiagnosisEncountersDates : Bool -> List NCDEncounterDataItem -> List NominalDate
generateDiabetesDiagnosisEncountersDates includingMedicalConditions =
    List.filterMap
        (\encounter ->
            let
                byDiagnosis =
                    List.any (\diagnosis -> EverySet.member diagnosis encounter.diagnoses)
                        [ DiagnosisDiabetesInitial, DiagnosisDiabetesRecurrent ]

                byMedicalConditions =
                    if includingMedicalConditions then
                        let
                            medicalConditions =
                                EverySet.union encounter.medicalConditions encounter.coMorbidities
                        in
                        EverySet.member MedicalConditionDiabetes medicalConditions

                    else
                        False
            in
            if byDiagnosis || byMedicalConditions then
                Just encounter.startDate

            else
                Nothing
        )
        -- Sort by date ASC.
        >> List.sortWith Date.compare


generatePatientsWithHIV : NominalDate -> List NCDDataItem -> List PersonIdentifier
generatePatientsWithHIV dateLastDayOfSelectedMonth =
    List.filter
        (.encounters
            >> -- Take only encounters that were ran at current month
               -- or prior to that.
               List.filter (.startDate >> withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth)
            >> -- Generate dates of all encounters where Hypertension was diagnosed.
               List.filter
                (\encounter ->
                    let
                        byTestResult =
                            Maybe.map ((==) TestPositive)
                                encounter.hivTestResult
                                |> Maybe.withDefault False

                        byTestExecutionNote =
                            Maybe.map ((==) TestNoteKnownAsPositive)
                                encounter.hivTestExecutionNote
                                |> Maybe.withDefault False

                        byMedicalConditions =
                            let
                                medicalConditions =
                                    EverySet.union encounter.medicalConditions encounter.coMorbidities
                            in
                            EverySet.member MedicalConditionHIV medicalConditions
                    in
                    byTestResult || byTestExecutionNote || byMedicalConditions
                )
            >> List.isEmpty
            >> not
        )
        >> List.map .identifier


generateVaccinationProgressDict : Site -> Gender -> List SPVEncounterDataItem -> List ChildScoreboardEncounterDataItem -> VaccinationProgressDict
generateVaccinationProgressDict site gender spvEncounters childScoreboardEncounters =
    let
        bcgImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .bcgImminizationDates spvEncounters)
                (resolveAllVaccinationDates .bcgImminizationDates childScoreboardEncounters)

        opvImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .opvImminizationDates spvEncounters)
                (resolveAllVaccinationDates .opvImminizationDates childScoreboardEncounters)

        dtpImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .dtpImminizationDates spvEncounters)
                (resolveAllVaccinationDates .dtpImminizationDates childScoreboardEncounters)

        pcv13ImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .pcv13ImminizationDates spvEncounters)
                (resolveAllVaccinationDates .pcv13ImminizationDates childScoreboardEncounters)

        rotarixImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .rotarixImminizationDates spvEncounters)
                (resolveAllVaccinationDates .rotarixImminizationDates childScoreboardEncounters)

        ipvImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .ipvImminizationDates spvEncounters)
                (resolveAllVaccinationDates .ipvImminizationDates childScoreboardEncounters)

        mrImminizationDates =
            EverySet.union
                (resolveAllVaccinationDates .mrImminizationDates spvEncounters)
                (resolveAllVaccinationDates .mrImminizationDates childScoreboardEncounters)

        resolveAllVaccinationDates resolveFunc encounters =
            List.map resolveFunc encounters
                |> List.foldl EverySet.union EverySet.empty

        dtpStandaloneEntry =
            -- This entry shall appear after 3 doses of DTP were given.
            if EverySet.size dtpImminizationDates == 3 then
                let
                    dtpStandaloneImminizationDates =
                        EverySet.union
                            (resolveAllVaccinationDates .dtpStandaloneImminizationDates spvEncounters)
                            (resolveAllVaccinationDates .dtpStandaloneImminizationDates childScoreboardEncounters)
                in
                [ ( VaccineDTPStandalone, generateVaccinationProgressForVaccine dtpStandaloneImminizationDates ) ]

            else
                []

        hpvEntry =
            if gender == Female then
                let
                    hpvImminizationDates =
                        resolveAllVaccinationDates .hpvImminizationDates spvEncounters
                in
                [ ( VaccineHPV, generateVaccinationProgressForVaccine hpvImminizationDates ) ]

            else
                []

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

        vaccineTypesForSite =
            allVaccineTypes site
    in
    [ ( VaccineBCG, generateVaccinationProgressForVaccine bcgImminizationDates )
    , ( VaccineOPV, generateVaccinationProgressForVaccine opvImminizationDates )
    , ( VaccineDTP, generateVaccinationProgressForVaccine dtpImminizationDates )
    , ( VaccinePCV13, generateVaccinationProgressForVaccine pcv13ImminizationDates )
    , ( VaccineRotarix, generateVaccinationProgressForVaccine rotarixImminizationDates )
    , ( VaccineIPV, generateVaccinationProgressForVaccine ipvImminizationDates )
    , ( VaccineMR, generateVaccinationProgressForVaccine mrImminizationDates )
    ]
        ++ dtpStandaloneEntry
        ++ hpvEntry
        |> List.filter
            (\( vaccineType, _ ) ->
                List.member vaccineType vaccineTypesForSite
            )
        |> Dict.fromList


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



--
-- Helper functions.
--


withinSelectedMonth : NominalDate -> NominalDate -> Bool
withinSelectedMonth dateLastDayOfSelectedMonth date =
    withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth date
        && withinOrAfterSelectedMonth dateLastDayOfSelectedMonth date


withinOrBeforeSelectedMonth : NominalDate -> NominalDate -> Bool
withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth date =
    not <| Date.compare date dateLastDayOfSelectedMonth == GT


withinOrAfterSelectedMonth : NominalDate -> NominalDate -> Bool
withinOrAfterSelectedMonth dateLastDayOfSelectedMonth date =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month dateLastDayOfSelectedMonth
    in
    not <| Date.compare date dateFirstDayOfSelectedMonth == LT


childrenBeneficiariesByProgramType : ProgramType -> Dict ProgramType (List ChildrenBeneficiariesStats) -> List ChildrenBeneficiariesStats
childrenBeneficiariesByProgramType programType childrenBeneficiaries =
    Dict.get programType childrenBeneficiaries
        |> Maybe.withDefault []


filterByLimitDate : NominalDate -> Dict id { a | dateMeasured : NominalDate } -> Dict id { a | dateMeasured : NominalDate }
filterByLimitDate limitDate followUps =
    Dict.filter (\_ followUp -> Date.compare followUp.dateMeasured limitDate == LT) followUps


isAcuteIllnessNurseEncounter : AcuteIllnessEncounterDataItem -> Bool
isAcuteIllnessNurseEncounter encounter =
    encounter.encounterType /= AcuteIllnessEncounterCHW
