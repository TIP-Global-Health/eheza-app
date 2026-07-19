module SyncManager.Test exposing (all)

import Device.Model exposing (Device)
import EverySet
import Expect
import Json.Encode
import Pages.Page exposing (Page(..), UserPage(..))
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
import SyncManager.Utils exposing (determineDownloadPhotosStatus, pageAllowsBackgroundRefresh)
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
    describe "SyncManager"
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

        -- A long catch-up sync can schedule a page reload. It must not fire
        -- while a nurse is logged in and possibly mid-form, or their unsaved
        -- entries are lost; it is only allowed on the pre-login screens.
        , test "background refresh is skipped on a logged-in page" <|
            \() ->
                pageAllowsBackgroundRefresh (UserPage ClinicalPage)
                    |> Expect.equal False
        , test "background refresh is allowed on the PIN page" <|
            \() ->
                pageAllowsBackgroundRefresh PinCodePage
                    |> Expect.equal True
        , test "background refresh is allowed on the device page" <|
            \() ->
                pageAllowsBackgroundRefresh DevicePage
                    |> Expect.equal True
        ]
