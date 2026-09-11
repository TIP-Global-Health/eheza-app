module SyncManager.Test exposing (all)

import AssocList as Dict
import Device.Model exposing (Device)
import EverySet
import Expect
import Http
import Json.Encode
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData
import SyncManager.Encoder
import SyncManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity
        , DownloadPhotosStatus(..)
        , DownloadSyncResponse
        , Flags
        , IndexDbSaveError(..)
        , Model
        , Msg(..)
        , Site(..)
        , SyncCycle(..)
        , SyncInfoStatus(..)
        , SyncStatus(..)
        , UploadMethod(..)
        , emptyModel
        )
import SyncManager.Update
import SyncManager.Utils exposing (determineDownloadPhotosStatus, pageAllowsBackgroundRefresh)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson)
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


emptyGeneralResponse : DownloadSyncResponse BackendGeneralEntity
emptyGeneralResponse =
    { entities = []
    , revisionCount = 0
    , deviceName = ""
    , rollbarToken = ""
    , site = SiteUnknown
    , features = EverySet.empty
    }


{-| An edit of a person who already has a photo, waiting to be uploaded. The
photo rows are the ones the service worker made for photos taken here.
-}
encodedPersonEdit : List ( Int, String ) -> String
encodedPersonEdit uploadedPhotos =
    { entities =
        [ ( BackendAuthorityPerson
                { uuid = "person-uuid"
                , revision = 7
                , entity = { testPerson | avatarUrl = Just "https://example.com/sites/default/files/styles/patient-photo/public/photo.jpg" }
                }
          , UploadMethodUpdate
          )
        ]
    , remaining = 0
    , uploadPhotos =
        List.indexedMap
            (\index ( localId, url ) ->
                ( localId
                , { uuid = "photo-uuid-" ++ String.fromInt index
                  , photo = url
                  , localId = localId
                  , fileId = Just (100 + index)
                  }
                )
            )
            uploadedPhotos
            |> Dict.fromList
    }
        |> SyncManager.Encoder.encodeIndexDbQueryUploadAuthorityResultRecord 1
        |> Json.Encode.object
        |> Json.Encode.encode 0


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

        -- Each download lane reports its Http errors against its own
        -- response. The general lane runs before the authority lane, so when
        -- the general batch is saved the authority response still holds the
        -- previous cycle's outcome, and a failure there is not a general
        -- error.
        , test "BackendGeneralFetchedDataSavedHandle reports no error while the authority response still holds a previous failure" <|
            \() ->
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (BackendGeneralFetchedDataSavedHandle "0")
                    { testModel
                        | downloadGeneralResponse = RemoteData.Success emptyGeneralResponse
                        , downloadAuthorityResponse = RemoteData.Failure Http.NetworkError
                        , downloadRequestTime = Time.millisToPosix 0
                    }
                    |> .error
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
        , -- A photo that came down from the backend has no upload row, so
          -- there is no file ID to send. The key is left out, which leaves
          -- the stored photo alone; sending null would delete it.
          test "an edit that did not re-take the photo sends no photo key" <|
            \() ->
                encodedPersonEdit []
                    |> String.contains "photo"
                    |> Expect.equal False
        , test "an edit that did re-take the photo sends its file ID" <|
            \() ->
                encodedPersonEdit [ ( 7, "/cache-upload/images/photo.jpg" ) ]
                    |> String.contains "\"photo\":100"
                    |> Expect.equal True
        ]
