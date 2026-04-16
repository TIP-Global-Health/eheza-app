module Backend.StockUpdate.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model exposing (ImageUrl(..), StockCorrectionReason(..), StockSupplier(..), StockUpdate, StockUpdateType(..))
import Backend.StockUpdate.Model exposing (..)
import Backend.StockUpdate.Utils exposing (..)
import Date
import Expect exposing (FloatingPointTolerance(..))
import Gizra.NominalDate exposing (NominalDate)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Time


{-| Helper to construct a StockUpdate with only the fields relevant to
balance calculations. All other fields get dummy values.
-}
makeStockUpdate :
    { dateRecorded : NominalDate
    , quantity : Int
    , updateType : StockUpdateType
    }
    -> StockUpdate
makeStockUpdate { dateRecorded, quantity, updateType } =
    { nurse = toEntityUuid "test-nurse"
    , dateMeasured = dateRecorded
    , updateType = updateType
    , quantity = quantity
    , dateRecorded = dateRecorded
    , dateExpires = Nothing
    , batchNumber = Nothing
    , supplier = Nothing
    , notes = Nothing
    , correctionReason = Nothing
    , healthCenter = toEntityUuid "test-hc"
    , deleted = False
    , shard = Nothing
    , village = Nothing
    , signature = ImageUrl ""
    }


{-| Shorthand for creating a date.
-}
date : Int -> Time.Month -> Int -> NominalDate
date year month day =
    Date.fromCalendarDate year month day


generateDataTest : Test
generateDataTest =
    let
        currentDate =
            date 2026 Time.Feb 15

        currentMonthYear =
            ( 2, 26 )

        lookupMonth monthYear data =
            Dict.get monthYear data
    in
    describe "generateStockManagementDataFromDistributions"
        [ test "no data - all balances are Nothing" <|
            \_ ->
                let
                    result =
                        generateStockManagementDataFromDistributions currentDate [] []
                in
                lookupMonth currentMonthYear result
                    |> Maybe.andThen .currentBalance
                    |> Expect.equal Nothing
        , test "initial stock setup only - balance equals received quantity" <|
            \_ ->
                let
                    stockUpdates =
                        [ makeStockUpdate
                            { dateRecorded = date 2026 Time.Feb 1
                            , quantity = 100
                            , updateType = UpdateReceivingSupplies
                            }
                        ]

                    result =
                        generateStockManagementDataFromDistributions currentDate [] stockUpdates
                in
                lookupMonth currentMonthYear result
                    |> Maybe.andThen .currentBalance
                    |> Expect.equal (Just 100)
        , test "stock minus distributions - balance decreases" <|
            \_ ->
                let
                    stockUpdates =
                        [ makeStockUpdate
                            { dateRecorded = date 2026 Time.Feb 1
                            , quantity = 100
                            , updateType = UpdateReceivingSupplies
                            }
                        ]

                    distributions =
                        [ { dateMeasured = date 2026 Time.Feb 5, distributedAmount = 30 }
                        , { dateMeasured = date 2026 Time.Feb 10, distributedAmount = 20 }
                        ]

                    result =
                        generateStockManagementDataFromDistributions currentDate distributions stockUpdates
                in
                lookupMonth currentMonthYear result
                    |> Maybe.andThen .currentBalance
                    |> Expect.equal (Just 50)
        , test "received and issued totals are correct for a month" <|
            \_ ->
                let
                    stockUpdates =
                        [ makeStockUpdate
                            { dateRecorded = date 2026 Time.Feb 1
                            , quantity = 50
                            , updateType = UpdateReceivingSupplies
                            }
                        , makeStockUpdate
                            { dateRecorded = date 2026 Time.Feb 10
                            , quantity = 30
                            , updateType = UpdateReceivingSupplies
                            }
                        ]

                    distributions =
                        [ { dateMeasured = date 2026 Time.Feb 3, distributedAmount = 15 }
                        ]

                    result =
                        generateStockManagementDataFromDistributions currentDate distributions stockUpdates

                    monthData =
                        lookupMonth currentMonthYear result
                in
                Expect.all
                    [ \_ ->
                        monthData
                            |> Maybe.map .received
                            |> Expect.equal (Just 80)
                    , \_ ->
                        monthData
                            |> Maybe.map .issued
                            |> Expect.equal (Just 15)
                    ]
                    ()
        , test "balance carries forward across months" <|
            \_ ->
                let
                    stockUpdates =
                        [ makeStockUpdate
                            { dateRecorded = date 2026 Time.Jan 5
                            , quantity = 100
                            , updateType = UpdateReceivingSupplies
                            }
                        ]

                    distributions =
                        [ { dateMeasured = date 2026 Time.Jan 10, distributedAmount = 40 } ]

                    result =
                        generateStockManagementDataFromDistributions currentDate distributions stockUpdates

                    janBalance =
                        lookupMonth ( 1, 26 ) result
                            |> Maybe.andThen .currentBalance

                    febStarting =
                        lookupMonth currentMonthYear result
                            |> Maybe.andThen .startingStock
                in
                Expect.all
                    [ \_ -> janBalance |> Expect.equal (Just 60)
                    , \_ -> febStarting |> Expect.equal (Just 60)
                    ]
                    ()
        , test "no stock updates means feature not activated - balances are Nothing" <|
            \_ ->
                let
                    distributions =
                        [ { dateMeasured = date 2026 Time.Feb 5, distributedAmount = 10 } ]

                    result =
                        generateStockManagementDataFromDistributions currentDate distributions []
                in
                lookupMonth currentMonthYear result
                    |> Maybe.andThen .currentBalance
                    |> Expect.equal Nothing
        , test "consumption average over 6 months" <|
            \_ ->
                let
                    stockUpdates =
                        [ makeStockUpdate
                            { dateRecorded = date 2025 Time.May 1
                            , quantity = 1000
                            , updateType = UpdateReceivingSupplies
                            }
                        ]

                    distributions =
                        [ { dateMeasured = date 2025 Time.Jul 10, distributedAmount = 10 }
                        , { dateMeasured = date 2025 Time.Aug 10, distributedAmount = 10 }
                        , { dateMeasured = date 2025 Time.Sep 10, distributedAmount = 10 }
                        , { dateMeasured = date 2025 Time.Oct 10, distributedAmount = 10 }
                        , { dateMeasured = date 2025 Time.Nov 10, distributedAmount = 10 }
                        , { dateMeasured = date 2025 Time.Dec 10, distributedAmount = 10 }
                        ]

                    result =
                        generateStockManagementDataFromDistributions currentDate distributions stockUpdates
                in
                lookupMonth ( 1, 26 ) result
                    |> Maybe.map .consumptionAverage
                    |> Maybe.map (\avg -> Expect.within (Absolute 0.01) 10 avg)
                    |> Maybe.withDefault (Expect.fail "Month not found in result")
        , test "multiple stock updates across months accumulate correctly" <|
            \_ ->
                let
                    stockUpdates =
                        [ makeStockUpdate
                            { dateRecorded = date 2025 Time.Dec 1
                            , quantity = 50
                            , updateType = UpdateReceivingSupplies
                            }
                        , makeStockUpdate
                            { dateRecorded = date 2026 Time.Jan 15
                            , quantity = 30
                            , updateType = UpdateReceivingSupplies
                            }
                        ]

                    distributions =
                        [ { dateMeasured = date 2025 Time.Dec 10, distributedAmount = 20 }
                        , { dateMeasured = date 2026 Time.Jan 5, distributedAmount = 10 }
                        ]

                    result =
                        generateStockManagementDataFromDistributions currentDate distributions stockUpdates
                in
                Expect.all
                    [ \_ ->
                        lookupMonth ( 12, 25 ) result
                            |> Maybe.andThen .currentBalance
                            |> Expect.equal (Just 30)
                    , \_ ->
                        lookupMonth ( 1, 26 ) result
                            |> Maybe.andThen .currentBalance
                            |> Expect.equal (Just 50)
                    , \_ ->
                        lookupMonth currentMonthYear result
                            |> Maybe.andThen .currentBalance
                            |> Expect.equal (Just 50)
                    ]
                    ()
        ]


all : Test
all =
    describe "StockUpdate tests"
        [ generateDataTest
        ]
