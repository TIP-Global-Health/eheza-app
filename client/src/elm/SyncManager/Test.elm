module SyncManager.Test exposing (all)

import AssocList as Dict
import Device.Model exposing (Device)
import EverySet
import Expect
import Http
import Json.Encode
import List.Zipper as Zipper exposing (Zipper)
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
        , SyncInfoAuthority
        , SyncInfoStatus(..)
        , SyncStatus(..)
        , UploadMethod(..)
        , emptyModel
        , emptySyncInfoAuthority
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


{-| A download batch of authority records.
-}
authorityResponse : List BackendAuthorityEntity -> DownloadSyncResponse BackendAuthorityEntity
authorityResponse entities =
    { entities = entities
    , revisionCount = List.length entities
    , deviceName = ""
    , rollbarToken = ""
    , site = SiteUnknown
    , features = EverySet.empty
    }


{-| One person record at the given revision.
-}
personAtRevision : Int -> BackendAuthorityEntity
personAtRevision revision =
    BackendAuthorityPerson { uuid = "person-uuid", revision = revision, entity = testPerson }


{-| The device's health centres, with the sync cycle on the first one.
-}
authorities : String -> List String -> Zipper SyncInfoAuthority
authorities current others =
    authoritiesFrom 0 current others


{-| The same, with the current health centre's revision cursor at `cursor`.
-}
authoritiesFrom : Int -> String -> List String -> Zipper SyncInfoAuthority
authoritiesFrom cursor current others =
    let
        current_ =
            emptySyncInfoAuthority current
    in
    Zipper.from [] { current_ | lastFetchedRevisionId = cursor } (List.map emptySyncInfoAuthority others)


{-| A model whose general download is in flight, issued at time 5000 from `cursor`.
-}
downloadingGeneral : Int -> Model
downloadingGeneral cursor =
    let
        syncInfoGeneral =
            testModel.syncInfoGeneral
    in
    { testModel
        | syncStatus = SyncDownloadGeneral RemoteData.Loading
        , syncInfoGeneral = { syncInfoGeneral | lastFetchedRevisionId = cursor }
        , downloadRequestTime = Time.millisToPosix 5000
    }


{-| A model whose authority download is in flight, issued at `time`.
-}
downloadingAuthority : Zipper SyncInfoAuthority -> Int -> Model
downloadingAuthority zipper time =
    { testModel
        | syncStatus = SyncDownloadAuthority RemoteData.Loading
        , syncInfoAuthorities = Just zipper
        , downloadRequestTime = Time.millisToPosix time
    }


{-| IndexedDB's acknowledgement that the batch stamped `timestamp` was saved.
-}
authorityBatchSaved : String -> Msg
authorityBatchSaved timestamp =
    Json.Encode.object
        [ ( "table", Json.Encode.string "Authority" )
        , ( "status", Json.Encode.string "Success" )
        , ( "timestamp", Json.Encode.string timestamp )
        ]
        |> SavedAtIndexDbHandle


runUpdate : Msg -> Model -> Model
runUpdate msg model =
    SyncManager.Update.update (Time.millisToPosix 0) DevicePage 0 testDevice msg model
        |> .model


{-| Each authority's revision cursor, in list order.
-}
cursors : Model -> List ( String, Int )
cursors model =
    model.syncInfoAuthorities
        |> Maybe.map (Zipper.toList >> List.map (\authority -> ( authority.uuid, authority.lastFetchedRevisionId )))
        |> Maybe.withDefault []


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
        , test "a late authority download response is applied while its authority and base revision are unchanged" <|
            \() ->
                downloadingAuthority (authorities "hc-A" [ "hc-B" ]) 5000
                    |> runUpdate
                        (BackendAuthorityFetchHandle (authorities "hc-A" [ "hc-B" ])
                            (Time.millisToPosix 1000)
                            (RemoteData.Success (authorityResponse [ personAtRevision 900 ]))
                        )
                    |> runUpdate (authorityBatchSaved "5000")
                    |> cursors
                    |> Expect.equal [ ( "hc-A", 900 ), ( "hc-B", 0 ) ]
        , test "an authority download response is dropped when its authority was removed and re-added since the request" <|
            \() ->
                downloadingAuthority (authorities "hc-A" [ "hc-B" ]) 5000
                    |> runUpdate
                        (BackendAuthorityFetchHandle (authoritiesFrom 5000 "hc-A" [ "hc-B" ])
                            (Time.millisToPosix 5000)
                            (RemoteData.Success (authorityResponse [ personAtRevision 5900 ]))
                        )
                    |> (\model -> ( model.syncStatus, model.downloadAuthorityResponse ))
                    |> Expect.equal ( SyncDownloadAuthority RemoteData.NotAsked, RemoteData.NotAsked )
        , test "a superseded authority download response whose base revision moved is ignored while a newer request is in flight" <|
            \() ->
                downloadingAuthority (authoritiesFrom 500 "hc-A" [ "hc-B" ]) 5000
                    |> runUpdate
                        (BackendAuthorityFetchHandle (authorities "hc-A" [ "hc-B" ])
                            (Time.millisToPosix 1000)
                            (RemoteData.Success (authorityResponse [ personAtRevision 500 ]))
                        )
                    |> (\model -> ( model.syncStatus, model.downloadAuthorityResponse ))
                    |> Expect.equal ( SyncDownloadAuthority RemoteData.Loading, RemoteData.NotAsked )
        , test "an authority download response is dropped when its authority is no longer current" <|
            \() ->
                downloadingAuthority (authorities "hc-B" [ "hc-A" ]) 5000
                    |> runUpdate
                        (BackendAuthorityFetchHandle (authorities "hc-A" [ "hc-B" ])
                            (Time.millisToPosix 5000)
                            (RemoteData.Success (authorityResponse [ personAtRevision 900 ]))
                        )
                    |> (\model -> ( model.syncStatus, model.downloadAuthorityResponse ))
                    |> Expect.equal ( SyncDownloadAuthority RemoteData.NotAsked, RemoteData.NotAsked )
        , test "a saved authority batch moves the revision cursor of its own authority" <|
            \() ->
                downloadingAuthority (authorities "hc-A" [ "hc-B" ]) 5000
                    |> runUpdate
                        (BackendAuthorityFetchHandle (authorities "hc-A" [ "hc-B" ])
                            (Time.millisToPosix 5000)
                            (RemoteData.Success (authorityResponse [ personAtRevision 900 ]))
                        )
                    |> runUpdate (authorityBatchSaved "5000")
                    |> cursors
                    |> Expect.equal [ ( "hc-A", 900 ), ( "hc-B", 0 ) ]
        , test "an authority batch saved after the list changed does not move another authority's cursor" <|
            \() ->
                let
                    model =
                        downloadingAuthority (authorities "hc-B" [ "hc-A" ]) 5000
                in
                { model
                    | downloadAuthorityResponse = RemoteData.Success (authorityResponse [ personAtRevision 900 ])
                    , downloadAuthorityAtRequest = emptySyncInfoAuthority "hc-A"
                }
                    |> runUpdate (authorityBatchSaved "5000")
                    |> (\updated -> ( cursors updated, updated.syncStatus ))
                    |> Expect.equal ( [ ( "hc-B", 0 ), ( "hc-A", 0 ) ], SyncDownloadAuthority RemoteData.NotAsked )
        , test "a late general download response is applied while the base revision is unchanged" <|
            \() ->
                downloadingGeneral 0
                    |> runUpdate (BackendGeneralFetchHandle 0 (Time.millisToPosix 1000) (RemoteData.Success emptyGeneralResponse))
                    |> .downloadGeneralResponse
                    |> Expect.equal (RemoteData.Success emptyGeneralResponse)
        , test "a general download response is dropped when the base revision moved since the request" <|
            \() ->
                downloadingGeneral 500
                    |> runUpdate (BackendGeneralFetchHandle 0 (Time.millisToPosix 5000) (RemoteData.Success emptyGeneralResponse))
                    |> (\model -> ( model.syncStatus, model.downloadGeneralResponse ))
                    |> Expect.equal ( SyncDownloadGeneral RemoteData.NotAsked, RemoteData.NotAsked )
        , test "a statistics response for an authority that is no longer current leaves the list alone" <|
            \() ->
                { testModel
                    | syncStatus = SyncDownloadAuthorityDashboardStats RemoteData.Loading
                    , syncInfoAuthorities = Just (authorities "hc-B" [ "hc-A" ])
                }
                    |> runUpdate (BackendAuthorityDashboardStatsFetchHandle (authorities "hc-A" []) (RemoteData.Success (authorityResponse [])))
                    |> (\model -> ( model.syncInfoAuthorities, model.syncStatus ))
                    |> Expect.equal ( Just (authorities "hc-B" [ "hc-A" ]), SyncDownloadAuthorityDashboardStats RemoteData.NotAsked )
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
