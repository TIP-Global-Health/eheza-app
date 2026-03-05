module Backend.StockUpdate.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (StockCorrectionReason(..), StockManagementMeasurements, StockSupplier(..), StockUpdate, StockUpdateType(..), VillageStockManagementMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.StockUpdate.Model exposing (..)
import Backend.Village.Utils exposing (resolveVillageResidents)
import Date
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Utils.NominalDate exposing (sortByDate)


generateStockManagementData :
    NominalDate
    -> StockManagementMeasurements
    -> StockManagementData
generateStockManagementData currentDate measurements =
    let
        allDistributions =
            (Dict.values measurements.childFbf
                |> List.map
                    (\fbf ->
                        { dateMeasured = fbf.dateMeasured
                        , distributedAmount = fbf.value.distributedAmount
                        }
                    )
            )
                ++ (Dict.values measurements.motherFbf
                        |> List.map
                            (\fbf ->
                                { dateMeasured = fbf.dateMeasured
                                , distributedAmount = fbf.value.distributedAmount
                                }
                            )
                   )

        allStockUpdates =
            Dict.values measurements.stockUpdate
    in
    generateStockManagementDataFromDistributions currentDate allDistributions allStockUpdates


generateVillageStockManagementData :
    NominalDate
    -> VillageId
    -> ModelIndexedDb
    -> VillageStockManagementMeasurements
    -> StockManagementData
generateVillageStockManagementData currentDate villageId db measurements =
    let
        villageResidents =
            resolveVillageResidents villageId db

        allDistributions =
            (Dict.values measurements.ahezaChild
                |> List.filter (\aheza -> List.member aheza.participantId villageResidents)
                |> List.map
                    (\aheza ->
                        { dateMeasured = aheza.dateMeasured
                        , distributedAmount = aheza.value
                        }
                    )
            )
                ++ (Dict.values measurements.ahezaMother
                        |> List.filter (\aheza -> List.member aheza.participantId villageResidents)
                        |> List.map
                            (\aheza ->
                                { dateMeasured = aheza.dateMeasured
                                , distributedAmount = aheza.value.distributedAmount
                                }
                            )
                   )

        allStockUpdates =
            Dict.values measurements.stockUpdate
                |> List.filter (\stockUpdate -> stockUpdate.village == Just villageId)
    in
    generateStockManagementDataFromDistributions currentDate allDistributions allStockUpdates


generateStockManagementDataFromDistributions :
    NominalDate
    -> List DistributionEntry
    -> List StockUpdate
    -> StockManagementData
generateStockManagementDataFromDistributions currentDate allDistributions allStockUpdates =
    let
        firstMonthForDisplay =
            Date.add Date.Months -12 currentDate
                |> dateToMonthYear

        -- Stock Management feature is launched by initial setup is
        -- performed using stock update mechanism.
        -- Nurses should count current stock and enter it as
        -- stock update entry.
        firstStockUpdateMonthYear =
            allStockUpdates
                |> List.sortWith (sortByDate .dateRecorded)
                |> List.head
                |> Maybe.map .dateRecorded
                |> Maybe.withDefault
                    -- If no stock update entries were found, we know that
                    -- health center is yet to activate Stock Management
                    -- feature.
                    -- Indication of this would be setting first stock update
                    -- month to future date.
                    (Date.add Date.Months 1 currentDate)
                |> dateToMonthYear

        -- Average month consumption is calculated by number of distributions issued
        -- during past 6 months, so we use this data structure to calculate it.
        distributionsByMonth =
            List.range 1 18
                |> List.map
                    (\monthGap ->
                        let
                            monthYear =
                                Date.add Date.Months (-1 * monthGap) currentDate
                                    |> dateToMonthYear

                            quantityForMonthYear =
                                List.filterMap
                                    (\entry ->
                                        if compareMonthYear monthYear (dateToMonthYear entry.dateMeasured) == EQ then
                                            Just entry.distributedAmount

                                        else
                                            Nothing
                                    )
                                    allDistributions
                                    |> List.sum
                        in
                        ( monthYear, quantityForMonthYear )
                    )
                |> List.reverse

        distributionsForDisplay =
            List.filter
                (\entry ->
                    let
                        entryMonthYear =
                            dateToMonthYear entry.dateMeasured
                    in
                    compareMonthYear entryMonthYear firstMonthForDisplay /= LT
                )
                allDistributions

        stockUpdateForDisplay =
            allStockUpdates
                |> List.filter
                    (\stockUpdate ->
                        let
                            stockUpdateMonthYear =
                                dateToMonthYear stockUpdate.dateRecorded
                        in
                        compareMonthYear stockUpdateMonthYear firstMonthForDisplay /= LT
                    )

        -- To be able to present correct numbers, we need to know the initial
        -- starting stock.
        -- There are 2 options for this:
        --   1. Initial setup was performed during display period:
        --     Here, we simply look at that month quantities by distribution and
        --     Stock update entries.
        --   2. Initial setup was performed before display period:
        --     In this case, we need to count quantities by distribution and
        --     Stock update entries from initial setup month (included),
        --     up until first display month (not included).
        startingStockFilteringCondition processedMonthYear =
            if compareMonthYear firstStockUpdateMonthYear firstMonthForDisplay == GT then
                compareMonthYear processedMonthYear firstStockUpdateMonthYear == EQ

            else
                (compareMonthYear processedMonthYear firstStockUpdateMonthYear /= LT)
                    && (compareMonthYear processedMonthYear firstMonthForDisplay == LT)

        initialStockByStockUpdate =
            allStockUpdates
                |> List.filterMap
                    (\stockUpdate ->
                        if startingStockFilteringCondition <| dateToMonthYear stockUpdate.dateRecorded then
                            Just stockUpdate.quantity

                        else
                            Nothing
                    )
                |> List.sum

        initialStockByDistributions =
            List.filterMap
                (\entry ->
                    if startingStockFilteringCondition <| dateToMonthYear entry.dateMeasured then
                        Just entry.distributedAmount

                    else
                        Nothing
                )
                allDistributions
                |> List.sum

        initialStartingStock =
            toFloat initialStockByStockUpdate - initialStockByDistributions

        receivedIssuedByMonthYear =
            List.range 0 12
                |> List.map
                    (\monthGap ->
                        let
                            monthYear =
                                Date.add Date.Months (-1 * monthGap) currentDate
                                    |> dateToMonthYear

                            stockUpdates =
                                List.filter
                                    (\stockUpdate ->
                                        let
                                            stockUpdateMonthYear =
                                                dateToMonthYear stockUpdate.dateRecorded
                                        in
                                        compareMonthYear stockUpdateMonthYear monthYear == EQ
                                    )
                                    stockUpdateForDisplay

                            distributions =
                                List.filter
                                    (\entry ->
                                        let
                                            entryMonthYear =
                                                dateToMonthYear entry.dateMeasured
                                        in
                                        compareMonthYear entryMonthYear monthYear == EQ
                                    )
                                    distributionsForDisplay
                        in
                        ( monthYear, ( stockUpdates, distributions ) )
                    )
    in
    List.foldr
        (\( monthYear, ( stockUpdates, distributions ) ) accum ->
            let
                received =
                    List.map .quantity stockUpdates
                        |> List.sum
                        |> toFloat

                issued =
                    List.map .distributedAmount distributions
                        |> List.sum

                prevMonthYear =
                    getPrevMonthYear monthYear

                -- Starting stock for current month can be calculated by looking
                -- at data generated for previous month.
                -- If exists, it's the final balance of that month.
                -- If not, we set initial starting stock, if previous month was
                -- the setup month, or latter.
                startingStock =
                    Dict.get prevMonthYear accum
                        |> Maybe.map .currentBalance
                        |> Maybe.withDefault
                            (if compareMonthYear prevMonthYear firstStockUpdateMonthYear /= LT then
                                Just initialStartingStock

                             else
                                Nothing
                            )

                -- Total distribution consumption during previous 6 months.
                -- Used to calculate monthly average consumption.
                consumptionSixPastMonths =
                    List.Extra.findIndex
                        (\( key, _ ) ->
                            compareMonthYear key monthYear == EQ
                        )
                        distributionsByMonth
                        |> Maybe.map
                            (\index ->
                                List.Extra.splitAt (index - 6) distributionsByMonth
                                    |> Tuple.second
                                    |> List.map Tuple.second
                                    |> List.take 6
                                    |> List.sum
                            )
                        |> Maybe.withDefault 0
            in
            Dict.insert monthYear
                { startingStock = startingStock
                , received = received
                , issued = issued
                , currentBalance =
                    Maybe.map
                        (\starting ->
                            Just (starting + received - issued)
                        )
                        startingStock
                        |> Maybe.withDefault
                            (if compareMonthYear monthYear firstStockUpdateMonthYear == EQ then
                                Just (received - issued)

                             else
                                Nothing
                            )
                , consumptionAverage = consumptionSixPastMonths / 6
                , stockUpdates = stockUpdates
                , distributions = distributions
                }
                accum
        )
        Dict.empty
        receivedIssuedByMonthYear


dateToMonthYear : NominalDate -> MonthYear
dateToMonthYear date =
    ( Date.monthNumber date
    , Date.year date
        |> modBy 1000
    )


compareMonthYear : MonthYear -> MonthYear -> Order
compareMonthYear ( m1, y1 ) ( m2, y2 ) =
    if y1 < y2 then
        LT

    else if y1 > y2 then
        GT

    else if m1 < m2 then
        LT

    else if m1 > m2 then
        GT

    else
        EQ


monthYearDiff : MonthYear -> MonthYear -> Int
monthYearDiff ( m1, y1 ) ( m2, y2 ) =
    if compareMonthYear ( m1, y1 ) ( m2, y2 ) == LT then
        monthYearDiff ( m2, y2 ) ( m1, y1 )

    else if y1 == y2 then
        m1 - m2

    else
        (y1 - y2) * 12 + m1 - m2


getPrevMonthYear : MonthYear -> MonthYear
getPrevMonthYear ( month, year ) =
    if month > 1 then
        ( month - 1, year )

    else
        ( 12, year - 1 )


stockUpdateTypeToString : StockUpdateType -> String
stockUpdateTypeToString value =
    case value of
        UpdateReceivingSupplies ->
            "receive-supply"

        UpdateCorrection ->
            "correction"


stockUpdateTypeFromString : String -> Maybe StockUpdateType
stockUpdateTypeFromString value =
    case value of
        "receive-supply" ->
            Just UpdateReceivingSupplies

        "correction" ->
            Just UpdateCorrection

        _ ->
            Nothing


stockSupplierToString : StockSupplier -> String
stockSupplierToString value =
    case value of
        SupplierMOH ->
            "moh"

        SupplierRBC ->
            "rbc"

        SupplierUNICEF ->
            "unicef"

        SupplierRMSCentral ->
            "rms-center"

        SupplierRMSDistrict ->
            "rms-district"

        SupplierBUFMAR ->
            "bufmar"


stockSupplierFromString : String -> Maybe StockSupplier
stockSupplierFromString value =
    case value of
        "moh" ->
            Just SupplierMOH

        "rbc" ->
            Just SupplierRBC

        "unicef" ->
            Just SupplierUNICEF

        "rms-center" ->
            Just SupplierRMSCentral

        "rms-district" ->
            Just SupplierRMSDistrict

        "bufmar" ->
            Just SupplierBUFMAR

        _ ->
            Nothing


stockCorrectionReasonToString : StockCorrectionReason -> String
stockCorrectionReasonToString value =
    case value of
        ReasonInputError ->
            "input-error"

        ReasonExpiration ->
            "expiration"

        ReasonMissing ->
            "missing"

        ReasonOther ->
            "other"


stockCorrectionReasonFromString : String -> Maybe StockCorrectionReason
stockCorrectionReasonFromString value =
    case value of
        "input-error" ->
            Just ReasonInputError

        "expiration" ->
            Just ReasonExpiration

        "missing" ->
            Just ReasonMissing

        "other" ->
            Just ReasonOther

        _ ->
            Nothing
