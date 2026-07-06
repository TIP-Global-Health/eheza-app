module SyncManager.Test exposing (all)

import Device.Model exposing (Device)
import EverySet
import Expect
import Json.Encode
import Pages.Page exposing (Page(..))
import RemoteData
import SyncManager.Model
    exposing
        ( DownloadPhotosStatus(..)
        , Flags
        , IndexDbSaveError(..)
        , Model
        , Msg(..)
        , Site(..)
        , SyncCycle(..)
        , SyncInfoStatus(..)
        , SyncStatus(..)
        , emptyModel
        )
import SyncManager.Update
import SyncManager.Utils exposing (determineDownloadPhotosStatus)
import Test exposing (Test, describe, test)
import Time


testFlags : Flags
testFlags =
    { syncInfoGeneral =
        { lastFetchedRevisionId = 0
        , lastSuccesfulContact = 0
        , remainingToUpload = 0
        , remainingToDownload = 0
        , deviceName = ""
        , status = NotAvailable
        , rollbarToken = ""
        , site = SiteUnknown
        , features = EverySet.empty
        }
    , syncInfoAuthorities = Nothing
    , batchSize = 100
    , syncSpeed =
        { idle = 3000
        , cycle = 50
        , offline = 10000
        }
    }


testModel : Model
testModel =
    emptyModel testFlags


testDevice : Device
testDevice =
    { accessToken = ""
    , refreshToken = ""
    , backendUrl = ""
    , deviceId = Nothing
    }


all : Test
all =
    describe "SyncManager photo lane"
        [ test "determineDownloadPhotosStatus progresses the photo lane while the data lane is downloading" <|
            \() ->
                determineDownloadPhotosStatus
                    { testModel
                        | syncStatus = SyncDownloadAuthority RemoteData.NotAsked
                        , downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCycleOn
                    }
                    |> .downloadPhotosStatus
                    |> Expect.notEqual DownloadPhotosIdle
        , test "determineDownloadPhotosStatus keeps the photo lane idle when the sync cycle is paused" <|
            \() ->
                determineDownloadPhotosStatus
                    { testModel
                        | syncStatus = SyncIdle
                        , downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCyclePause
                    }
                    |> .downloadPhotosStatus
                    |> Expect.equal DownloadPhotosIdle
        , test "SavedAtIndexDbHandle for a successful DeferredPhotos save kicks the photo lane out of idle" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "DeferredPhotos" )
                            , ( "status", Json.Encode.string "Success" )
                            , ( "timestamp", Json.Encode.string "" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel
                        | downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCycleOn
                    }
                    |> .model
                    |> .downloadPhotosStatus
                    |> Expect.notEqual DownloadPhotosIdle
        , test "SavedAtIndexDbHandle records a storage-full error for a QuotaExceededError failure" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "Authority" )
                            , ( "status", Json.Encode.string "Failure" )
                            , ( "timestamp", Json.Encode.string "" )
                            , ( "reason", Json.Encode.string "QuotaExceededError" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    testModel
                    |> .model
                    |> .lastSaveError
                    |> Expect.equal (Just IndexDbSaveErrorStorageFull)
        , test "SavedAtIndexDbHandle records a non-quota failure as a generic save error" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "Authority" )
                            , ( "status", Json.Encode.string "Failure" )
                            , ( "timestamp", Json.Encode.string "" )
                            , ( "reason", Json.Encode.string "BulkError" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    testModel
                    |> .model
                    |> .lastSaveError
                    |> Expect.equal (Just (IndexDbSaveErrorOther "BulkError"))
        , test "SavedAtIndexDbHandle clears a previous save error on a successful save" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "AuthorityStats" )
                            , ( "status", Json.Encode.string "Success" )
                            , ( "timestamp", Json.Encode.string "" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel | lastSaveError = Just IndexDbSaveErrorStorageFull }
                    |> .model
                    |> .lastSaveError
                    |> Expect.equal Nothing

        -- The download lanes complete only on save success, so a batch-save
        -- failure must park the waiting lane back to idle (retried next
        -- cycle) instead of leaving it Loading forever. downloadRequestTime
        -- is set explicitly in each test, so the in-flight request
        -- timestamp is "0".
        , test "SavedAtIndexDbHandle parks a Loading Authority download lane to idle when its batch save fails" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "Authority" )
                            , ( "status", Json.Encode.string "Failure" )
                            , ( "timestamp", Json.Encode.string "0" )
                            , ( "reason", Json.Encode.string "QuotaExceededError" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel
                        | syncStatus = SyncDownloadAuthority RemoteData.Loading
                        , downloadRequestTime = Time.millisToPosix 0
                    }
                    |> .model
                    |> .syncStatus
                    |> Expect.equal SyncIdle
        , test "SavedAtIndexDbHandle parks a Loading General download lane to idle when its batch save fails" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "General" )
                            , ( "status", Json.Encode.string "Failure" )
                            , ( "timestamp", Json.Encode.string "0" )
                            , ( "reason", Json.Encode.string "QuotaExceededError" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel
                        | syncStatus = SyncDownloadGeneral RemoteData.Loading
                        , downloadRequestTime = Time.millisToPosix 0
                    }
                    |> .model
                    |> .syncStatus
                    |> Expect.equal SyncIdle
        , test "SavedAtIndexDbHandle ignores a save failure from a superseded (timed-out) request" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "Authority" )
                            , ( "status", Json.Encode.string "Failure" )
                            , ( "timestamp", Json.Encode.string "999" )
                            , ( "reason", Json.Encode.string "QuotaExceededError" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel
                        | syncStatus = SyncDownloadAuthority RemoteData.Loading
                        , downloadRequestTime = Time.millisToPosix 0
                    }
                    |> .model
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadAuthority RemoteData.Loading)
        , test "SavedAtIndexDbHandle leaves the download lane alone when another table's save fails" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "DeferredPhotos" )
                            , ( "status", Json.Encode.string "Failure" )
                            , ( "timestamp", Json.Encode.string "0" )
                            , ( "reason", Json.Encode.string "QuotaExceededError" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel
                        | syncStatus = SyncDownloadAuthority RemoteData.Loading
                        , downloadRequestTime = Time.millisToPosix 0
                    }
                    |> .model
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadAuthority RemoteData.Loading)
        ]
