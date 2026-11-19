module SyncManager.Test exposing (all)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Device.Model exposing (Device)
import EverySet
import Expect
import Http
import Json.Encode
import List.Zipper as Zipper exposing (Zipper)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Encoder
import SyncManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity
        , DownloadPhotosStatus(..)
        , DownloadSyncResponse
        , IndexDbSaveError(..)
        , Model
        , Msg(..)
        , Site(..)
        , SyncCycle(..)
        , SyncIncidentType(..)
        , SyncInfoAuthority
        , SyncInfoStatus(..)
        , SyncStatus(..)
        , UploadMethod(..)
        , UploadRec
        , emptySyncInfoAuthority
        , emptyUploadRec
        )
import SyncManager.Update
import SyncManager.Utils exposing (determineDownloadPhotosStatus, determineSyncStatus, pageAllowsBackgroundRefresh)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson, testSyncManagerModel)
import Time


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
            testSyncManagerModel.syncInfoGeneral
    in
    { testSyncManagerModel
        | syncStatus = SyncDownloadGeneral RemoteData.Loading
        , syncInfoGeneral = { syncInfoGeneral | lastFetchedRevisionId = cursor }
        , downloadRequestTime = Time.millisToPosix 5000
    }


{-| A model whose authority download is in flight, issued at `time`.
-}
downloadingAuthority : Zipper SyncInfoAuthority -> Int -> Model
downloadingAuthority zipper time =
    { testSyncManagerModel
        | syncStatus = SyncDownloadAuthority RemoteData.Loading
        , syncInfoAuthorities = Just zipper
        , downloadRequestTime = Time.millisToPosix time
    }


{-| A model whose statistics download is in flight.
-}
downloadingStats : Zipper SyncInfoAuthority -> Model
downloadingStats zipper =
    { testSyncManagerModel
        | syncStatus = SyncDownloadAuthorityDashboardStats RemoteData.Loading
        , syncInfoAuthorities = Just zipper
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


{-| Each authority's sync status, in list order.
-}
statuses : Model -> List ( String, SyncInfoStatus )
statuses model =
    model.syncInfoAuthorities
        |> Maybe.map (Zipper.toList >> List.map (\authority -> ( authority.uuid, authority.status )))
        |> Maybe.withDefault []


{-| A page of the group session.
-}
groupSessionPage : SessionPage -> Page
groupSessionPage =
    UserPage << SessionPage (toEntityUuid "session-uuid")


{-| The group session's child page, where a child's photo is taken among
their other activities.
-}
childPage : Page
childPage =
    groupSessionPage (ChildPage (toEntityUuid "child-uuid"))


{-| The group session's bulk Photo page, where every child's photo is taken.
-}
bulkPhotoPage : Page
bulkPhotoPage =
    groupSessionPage (ActivityPage (ChildActivity ChildPicture))


{-| Both pages on which sync was paused for photos.
-}
photoPages : List ( String, Page )
photoPages =
    [ ( "child page", childPage ), ( "bulk Photo page", bulkPhotoPage ) ]


{-| Pages that are neither of the two above, including pages of the same group
session and the individual encounters' own photo activity.
-}
otherPages : List ( String, Page )
otherPages =
    [ ( "clinical page", UserPage ClinicalPage )
    , ( "PIN page", PinCodePage )
    , ( "group session activities", groupSessionPage ActivitiesPage )
    , ( "group session attendance", groupSessionPage AttendancePage )
    , ( "group session Height activity", groupSessionPage (ActivityPage (ChildActivity Height)) )
    , ( "group session participants", groupSessionPage ParticipantsPage )
    , ( "group session mother page", groupSessionPage (MotherPage (toEntityUuid "mother-uuid")) )
    , ( "group session progress report", groupSessionPage (ProgressReportPage (toEntityUuid "child-uuid")) )
    , ( "Well Child photo activity", UserPage (WellChildActivityPage (toEntityUuid "encounter-uuid") WellChildPhoto) )
    ]


{-| A model with the sync cycle on, at `status`, holding the device's health
centres with the first one current.
-}
atStage : SyncStatus -> List String -> Model
atStage status healthCenters =
    { testSyncManagerModel
        | syncStatus = status
        , syncCycle = SyncCycleOn
        , syncInfoAuthorities =
            case healthCenters of
                [] ->
                    Nothing

                first :: rest ->
                    Just (authorities first rest)
    }


{-| An upload lane whose IndexedDB query found nothing left to send.
-}
uploadDone : UploadRec a
uploadDone =
    { emptyUploadRec | indexDbRemoteData = RemoteData.Success Nothing }


{-| A model at the same stage once that stage has sent or fetched everything
it had: the shape each stage is left in when nothing remains.
-}
finishStage : Model -> Model
finishStage model =
    let
        finished =
            case model.syncStatus of
                SyncDownloadAuthority _ ->
                    SyncDownloadAuthority (RemoteData.Success (authorityResponse []))

                SyncDownloadAuthorityDashboardStats _ ->
                    SyncDownloadAuthorityDashboardStats (RemoteData.Success (authorityResponse []))

                SyncDownloadGeneral _ ->
                    SyncDownloadGeneral (RemoteData.Success emptyGeneralResponse)

                SyncIdle ->
                    SyncIdle

                SyncReportIncident incident ->
                    SyncReportIncident incident

                SyncUploadAuthority _ ->
                    SyncUploadAuthority uploadDone

                SyncUploadGeneral _ ->
                    SyncUploadGeneral uploadDone

                SyncUploadPhoto _ _ ->
                    SyncUploadPhoto 0 (RemoteData.Success Nothing)

                SyncUploadScreenshot _ _ ->
                    SyncUploadScreenshot 0 (RemoteData.Success Nothing)

                SyncUploadWhatsApp _ ->
                    SyncUploadWhatsApp uploadDone
    in
    { model | syncStatus = finished }


{-| A readable name for the stage a model is at, with the current health
centre for the stages that go through them one by one.
-}
stageName : Model -> String
stageName model =
    let
        healthCenter =
            model.syncInfoAuthorities
                |> Maybe.map (\zipper -> " " ++ (Zipper.current zipper).uuid)
                |> Maybe.withDefault ""
    in
    case model.syncStatus of
        SyncDownloadAuthority _ ->
            "download authority" ++ healthCenter

        SyncDownloadAuthorityDashboardStats _ ->
            "download statistics" ++ healthCenter

        SyncDownloadGeneral _ ->
            "download general"

        SyncIdle ->
            "idle"

        SyncReportIncident _ ->
            "report incident"

        SyncUploadAuthority _ ->
            "upload authority" ++ healthCenter

        SyncUploadGeneral _ ->
            "upload general"

        SyncUploadPhoto _ _ ->
            "upload photo"

        SyncUploadScreenshot _ _ ->
            "upload screenshot"

        SyncUploadWhatsApp _ ->
            "upload whatsapp"


{-| The stages sync goes through on `page` over `ticks` ticks, each stage
finishing its work before the next tick, and the model it ends with.
-}
runSync : Page -> Int -> Model -> ( List String, Model )
runSync page ticks model =
    List.foldl
        (\_ ( visited, current ) ->
            let
                next =
                    determineSyncStatus page (finishStage current)
            in
            ( visited ++ [ stageName next ], next )
        )
        ( [], model )
        (List.range 1 ticks)


{-| Whether a stage uploads photos or records that can carry them. The
WhatsApp lane sends report records only.
-}
uploadsPhotosOrRecords : String -> Bool
uploadsPhotosOrRecords name =
    String.startsWith "upload" name && name /= "upload whatsapp"


{-| The parts of the model `determineSyncStatus` moves.
-}
syncPosition : Model -> ( SyncStatus, Maybe (Zipper SyncInfoAuthority) )
syncPosition model =
    ( model.syncStatus, model.syncInfoAuthorities )


{-| Whether the photo pages leave a model exactly where it was, as they did
for every stage before. Reports the pages that moved it.
-}
photoPagesLeaveUnchanged : Model -> Expect.Expectation
photoPagesLeaveUnchanged model =
    photoPages
        |> List.filter (\( _, page ) -> syncPosition (determineSyncStatus page model) /= syncPosition model)
        |> List.map Tuple.first
        |> Expect.equal []


{-| A full cycle from idle on a device with two health centres, stage by stage.
-}
fullCycle : List String
fullCycle =
    [ "upload photo"
    , "upload screenshot"
    , "upload general"
    , "upload authority hc-A"
    , "upload authority hc-B"
    , "upload whatsapp"
    , "download general"
    , "download authority hc-A"
    , "download authority hc-B"
    , "download statistics hc-A"
    , "download statistics hc-B"
    , "idle"
    ]


all : Test
all =
    describe "SyncManager"
        [ test "determineDownloadPhotosStatus progresses the photo lane while the data lane is downloading" <|
            \() ->
                determineDownloadPhotosStatus
                    { testSyncManagerModel
                        | syncStatus = SyncDownloadAuthority RemoteData.NotAsked
                        , downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCycleOn
                    }
                    |> .downloadPhotosStatus
                    |> Expect.notEqual DownloadPhotosIdle
        , test "determineDownloadPhotosStatus keeps the photo lane idle when the sync cycle is paused" <|
            \() ->
                determineDownloadPhotosStatus
                    { testSyncManagerModel
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
                    { testSyncManagerModel
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
                    testSyncManagerModel
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
                    testSyncManagerModel
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
                    { testSyncManagerModel | lastSaveError = Just IndexDbSaveErrorStorageFull }
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
                    { testSyncManagerModel
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
                    { testSyncManagerModel
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
                    { testSyncManagerModel
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
        , test "a failed authority download reply marks the lane failed and leaves the cursors alone" <|
            \() ->
                downloadingAuthority (authorities "hc-A" [ "hc-B" ]) 5000
                    |> runUpdate (BackendAuthorityFetchHandle (authorities "hc-A" [ "hc-B" ]) (Time.millisToPosix 5000) (RemoteData.Failure Http.NetworkError))
                    |> (\model -> ( cursors model, model.syncStatus ))
                    |> Expect.equal ( [ ( "hc-A", 0 ), ( "hc-B", 0 ) ], SyncDownloadAuthority (RemoteData.Failure Http.NetworkError) )
        , test "a failed general download reply marks the lane failed and keeps the previous response" <|
            \() ->
                downloadingGeneral 0
                    |> runUpdate (BackendGeneralFetchHandle 0 (Time.millisToPosix 5000) (RemoteData.Failure Http.NetworkError))
                    |> (\model -> ( model.syncStatus, model.downloadGeneralResponse, model.syncInfoGeneral.lastFetchedRevisionId ))
                    |> Expect.equal ( SyncDownloadGeneral (RemoteData.Failure Http.NetworkError), RemoteData.NotAsked, 0 )
        , test "a failed authority reply from a request sent again is ignored while the newer copy is in flight" <|
            \() ->
                downloadingAuthority (authorities "hc-A" [ "hc-B" ]) 5000
                    |> runUpdate (BackendAuthorityFetchHandle (authorities "hc-A" [ "hc-B" ]) (Time.millisToPosix 1000) (RemoteData.Failure Http.NetworkError))
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadAuthority RemoteData.Loading)
        , test "a failed general reply from a request sent again is ignored while the newer copy is in flight" <|
            \() ->
                downloadingGeneral 0
                    |> runUpdate (BackendGeneralFetchHandle 0 (Time.millisToPosix 1000) (RemoteData.Failure Http.NetworkError))
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadGeneral RemoteData.Loading)

        -- After 30 seconds a download request is sent again, and both copies
        -- reply. A copy that reaches the cycle once it has moved on to another
        -- lane answers a request that lane no longer waits on.
        , test "an authority reply arriving in the statistics lane is ignored" <|
            \() ->
                downloadingStats (authorities "hc-A" [])
                    |> runUpdate (BackendAuthorityFetchHandle (authorities "hc-A" []) (Time.millisToPosix 0) (RemoteData.Success (authorityResponse [])))
                    |> (\model -> ( model.syncStatus, model.downloadAuthorityResponse ))
                    |> Expect.equal ( SyncDownloadAuthorityDashboardStats RemoteData.Loading, RemoteData.NotAsked )
        , test "an authority reply for an earlier authority arriving in the statistics lane does not restart the authority lane" <|
            \() ->
                downloadingStats (authorities "hc-A" [ "hc-B" ])
                    |> runUpdate (BackendAuthorityFetchHandle (authorities "hc-B" [ "hc-A" ]) (Time.millisToPosix 0) (RemoteData.Success (authorityResponse [])))
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadAuthorityDashboardStats RemoteData.Loading)
        , test "an authority batch acknowledged in the statistics lane does not restart the authority lane" <|
            \() ->
                let
                    model =
                        downloadingStats (authorities "hc-A" [])
                in
                { model
                    | downloadAuthorityResponse = RemoteData.Success (authorityResponse [])
                    , downloadAuthorityAtRequest = emptySyncInfoAuthority "hc-A"
                }
                    |> runUpdate (authorityBatchSaved "0")
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadAuthorityDashboardStats RemoteData.Loading)
        , test "an authority reply arriving in the general lane is ignored" <|
            \() ->
                let
                    model =
                        downloadingGeneral 0
                in
                { model | syncInfoAuthorities = Just (authorities "hc-A" []) }
                    |> runUpdate (BackendAuthorityFetchHandle (authorities "hc-A" []) (Time.millisToPosix 5000) (RemoteData.Success (authorityResponse [])))
                    |> (\updated -> ( updated.syncStatus, updated.downloadAuthorityResponse ))
                    |> Expect.equal ( SyncDownloadGeneral RemoteData.Loading, RemoteData.NotAsked )
        , test "a general reply arriving in the statistics lane is ignored" <|
            \() ->
                downloadingStats (authorities "hc-A" [])
                    |> runUpdate (BackendGeneralFetchHandle 0 (Time.millisToPosix 0) (RemoteData.Success emptyGeneralResponse))
                    |> (\model -> ( model.syncStatus, model.downloadGeneralResponse ))
                    |> Expect.equal ( SyncDownloadAuthorityDashboardStats RemoteData.Loading, RemoteData.NotAsked )
        , test "a general reply arriving in the authority lane is ignored" <|
            \() ->
                downloadingAuthority (authorities "hc-A" []) 5000
                    |> runUpdate (BackendGeneralFetchHandle 0 (Time.millisToPosix 5000) (RemoteData.Success emptyGeneralResponse))
                    |> (\model -> ( model.syncStatus, model.downloadGeneralResponse ))
                    |> Expect.equal ( SyncDownloadAuthority RemoteData.Loading, RemoteData.NotAsked )
        , test "a general batch acknowledged in the authority lane does not restart the authority lane" <|
            \() ->
                let
                    model =
                        downloadingAuthority (authorities "hc-A" []) 0
                in
                { model | downloadGeneralResponse = RemoteData.Success emptyGeneralResponse }
                    |> runUpdate (BackendGeneralFetchedDataSavedHandle "0")
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadAuthority RemoteData.Loading)
        , test "a statistics reply arriving in the authority lane is ignored" <|
            \() ->
                downloadingAuthority (authorities "hc-A" []) 5000
                    |> runUpdate (BackendAuthorityDashboardStatsFetchHandle (authorities "hc-A" []) (RemoteData.Success (authorityResponse [])))
                    |> (\model -> ( model.syncStatus, statuses model ))
                    |> Expect.equal ( SyncDownloadAuthority RemoteData.Loading, [ ( "hc-A", NotAvailable ) ] )
        , test "a statistics reply arriving in the general lane is ignored, also once no health centre is left" <|
            \() ->
                downloadingGeneral 0
                    |> runUpdate (BackendAuthorityDashboardStatsFetchHandle (authorities "hc-A" []) (RemoteData.Success (authorityResponse [])))
                    |> .syncStatus
                    |> Expect.equal (SyncDownloadGeneral RemoteData.Loading)
        , test "a statistics response for an authority that is no longer current leaves the list alone" <|
            \() ->
                { testSyncManagerModel
                    | syncStatus = SyncDownloadAuthorityDashboardStats RemoteData.Loading
                    , syncInfoAuthorities = Just (authorities "hc-B" [ "hc-A" ])
                }
                    |> runUpdate (BackendAuthorityDashboardStatsFetchHandle (authorities "hc-A" []) (RemoteData.Success (authorityResponse [])))
                    |> (\model -> ( model.syncInfoAuthorities, model.syncStatus ))
                    |> Expect.equal ( Just (authorities "hc-B" [ "hc-A" ]), SyncDownloadAuthorityDashboardStats RemoteData.NotAsked )

        -- A failed statistics download must not hold the sync cycle, or the
        -- next cycle's uploads never start. Statistics are retried next cycle.
        , test "a failed statistics download marks its authority failed and moves on to the next one" <|
            \() ->
                downloadingStats (authorities "hc-A" [ "hc-B" ])
                    |> runUpdate (BackendAuthorityDashboardStatsFetchHandle (authorities "hc-A" [ "hc-B" ]) (RemoteData.Failure Http.Timeout))
                    |> (\model -> ( statuses model, Maybe.map (Zipper.current >> .uuid) model.syncInfoAuthorities, model.syncStatus ))
                    |> Expect.equal ( [ ( "hc-A", Error ), ( "hc-B", NotAvailable ) ], Just "hc-B", SyncDownloadAuthorityDashboardStats RemoteData.NotAsked )
        , test "a failed statistics download on the last authority ends the sync cycle" <|
            \() ->
                downloadingStats (authorities "hc-A" [])
                    |> runUpdate (BackendAuthorityDashboardStatsFetchHandle (authorities "hc-A" []) (RemoteData.Failure Http.Timeout))
                    |> .syncStatus
                    |> Expect.equal SyncIdle
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
                    { testSyncManagerModel
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
                    { testSyncManagerModel
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

        -- A group session's child page and its bulk Photo page used to hold
        -- sync at whatever stage it was in when they opened. Opened while
        -- records were uploading, the app kept uploading records for the
        -- whole visit, and every photo taken there went up without its image.
        -- These tests run determineSyncStatus with each stage already done,
        -- which is the shape the update leaves it in once nothing is left.
        , describe "Sync on the group session photo pages"
            [ describe "Opened while records upload, sync moves on once the waiting records are sent"
                [ test "on a group session's child page, sync moves on once the waiting records are sent" <|
                    \() ->
                        atStage (SyncUploadAuthority uploadDone) [ "hc-A" ]
                            |> determineSyncStatus childPage
                            |> stageName
                            |> Expect.notEqual "upload authority hc-A"
                , test "on the bulk Photo page, sync moves on once the waiting records are sent" <|
                    \() ->
                        atStage (SyncUploadAuthority uploadDone) [ "hc-A" ]
                            |> determineSyncStatus bulkPhotoPage
                            |> stageName
                            |> Expect.notEqual "upload authority hc-A"

                -- The general and WhatsApp lanes upload records too.
                , test "on the photo pages, sync moves on from general records once they are sent" <|
                    \() ->
                        photoPages
                            |> List.filter (\( _, page ) -> stageName (determineSyncStatus page (atStage (SyncUploadGeneral uploadDone) [ "hc-A" ])) == "upload general")
                            |> List.map Tuple.first
                            |> Expect.equal []
                , test "on the photo pages, sync moves on from WhatsApp records once they are sent" <|
                    \() ->
                        photoPages
                            |> List.filter (\( _, page ) -> stageName (determineSyncStatus page (atStage (SyncUploadWhatsApp uploadDone) [ "hc-A" ])) == "upload whatsapp")
                            |> List.map Tuple.first
                            |> Expect.equal []

                -- Records are uploaded one health centre at a time. The
                -- records waiting for the next health centre are waiting
                -- records too, so they are sent before sync moves on.
                , test "on the photo pages, records waiting for the next health centre are sent before sync moves on" <|
                    \() ->
                        photoPages
                            |> List.map (\( name, page ) -> ( name, stageName (determineSyncStatus page (atStage (SyncUploadAuthority uploadDone) [ "hc-A", "hc-B" ])) ))
                            |> Expect.equal [ ( "child page", "upload authority hc-B" ), ( "bulk Photo page", "upload authority hc-B" ) ]

                -- Until the IndexedDB query comes back empty there are still
                -- records to send, so the lane stays where it is.
                , test "on the photo pages, a record lane that still has records to send stays where it is" <|
                    \() ->
                        [ atStage (SyncUploadAuthority { emptyUploadRec | indexDbRemoteData = RemoteData.Success (Just { entities = [], remaining = 3, uploadPhotos = Dict.empty }) }) [ "hc-A" ]
                        , atStage (SyncUploadAuthority { emptyUploadRec | indexDbRemoteData = RemoteData.Loading }) [ "hc-A" ]
                        , atStage (SyncUploadGeneral emptyUploadRec) [ "hc-A" ]
                        , atStage (SyncUploadWhatsApp { emptyUploadRec | indexDbRemoteData = RemoteData.Loading }) [ "hc-A" ]
                        ]
                            |> List.map photoPagesLeaveUnchanged
                            |> List.map (\expectation -> \() -> expectation)
                            |> (\checks -> Expect.all checks ())
                ]
            , describe "Photos taken on those pages stay on the device until the nurse leaves, then go up first"
                [ test "on a group session's child page, no photo or record is uploaded after the waiting records are sent, until the nurse leaves" <|
                    \() ->
                        atStage (SyncUploadAuthority uploadDone) [ "hc-A" ]
                            |> runSync childPage 30
                            |> Tuple.first
                            |> List.filter uploadsPhotosOrRecords
                            |> Expect.equal []
                , test "on the bulk Photo page, no photo or record is uploaded after the waiting records are sent, until the nurse leaves" <|
                    \() ->
                        atStage (SyncUploadAuthority uploadDone) [ "hc-A" ]
                            |> runSync bulkPhotoPage 30
                            |> Tuple.first
                            |> List.filter uploadsPhotosOrRecords
                            |> Expect.equal []

                -- The same visit, followed by the nurse moving to the
                -- clinical page: the next upload sync makes is the photos.
                , test "once the nurse leaves a photo page opened while records uploaded, the next cycle uploads photos first" <|
                    \() ->
                        photoPages
                            |> List.map
                                (\( name, page ) ->
                                    atStage (SyncUploadAuthority uploadDone) [ "hc-A" ]
                                        |> runSync page 30
                                        |> Tuple.second
                                        |> runSync (UserPage ClinicalPage) 30
                                        |> Tuple.first
                                        |> List.filter (String.startsWith "upload")
                                        |> List.head
                                        |> Tuple.pair name
                                )
                            |> Expect.equal [ ( "child page", Just "upload photo" ), ( "bulk Photo page", Just "upload photo" ) ]
                , test "on a photo page opened while sync is idle, nothing is uploaded during the visit" <|
                    \() ->
                        photoPages
                            |> List.concatMap (\( _, page ) -> atStage SyncIdle [ "hc-A" ] |> runSync page 30 |> Tuple.first)
                            |> List.filter ((/=) "idle")
                            |> Expect.equal []
                , test "once the nurse leaves a photo page opened while sync was idle, the next cycle starts with photos" <|
                    \() ->
                        atStage SyncIdle [ "hc-A" ]
                            |> runSync childPage 5
                            |> Tuple.second
                            |> determineSyncStatus (UserPage ClinicalPage)
                            |> stageName
                            |> Expect.equal "upload photo"
                ]

            -- Before, the photo pages left every stage as it was.
            , describe "Opened while sync is in any other stage, those pages behave as before"
                [ test "opened while sync is idle, the photo pages keep it idle" <|
                    \() ->
                        photoPagesLeaveUnchanged (atStage SyncIdle [ "hc-A" ])
                , test "opened while photos upload, the photo pages leave the photo lane where it is" <|
                    \() ->
                        Expect.all
                            [ \() -> photoPagesLeaveUnchanged (atStage (SyncUploadPhoto 0 RemoteData.Loading) [ "hc-A" ])
                            , \() -> photoPagesLeaveUnchanged (atStage (SyncUploadPhoto 0 (RemoteData.Success Nothing)) [ "hc-A" ])
                            ]
                            ()
                , test "opened while screenshots upload, the photo pages leave the screenshot lane where it is" <|
                    \() ->
                        photoPagesLeaveUnchanged (atStage (SyncUploadScreenshot 0 (RemoteData.Success Nothing)) [ "hc-A" ])
                , test "opened while general data downloads, the photo pages leave the download where it is" <|
                    \() ->
                        photoPagesLeaveUnchanged (atStage (SyncDownloadGeneral (RemoteData.Success emptyGeneralResponse)) [ "hc-A" ])
                , test "opened while health centre data downloads, the photo pages leave the download where it is" <|
                    \() ->
                        photoPagesLeaveUnchanged (atStage (SyncDownloadAuthority (RemoteData.Success (authorityResponse []))) [ "hc-A", "hc-B" ])
                , test "opened while statistics download, the photo pages leave the download where it is" <|
                    \() ->
                        photoPagesLeaveUnchanged (atStage (SyncDownloadAuthorityDashboardStats (RemoteData.Success (authorityResponse []))) [ "hc-A", "hc-B" ])
                , test "opened while an incident is reported, the photo pages leave the report where it is" <|
                    \() ->
                        photoPagesLeaveUnchanged (atStage (SyncReportIncident (FileUploadIncident "/cache-upload/images/photo.jpg")) [ "hc-A" ])

                -- A sync cycle paused or held on its stage from the device
                -- page stays so on the photo pages, records sent or not.
                , test "with the sync cycle paused or held on its stage, the photo pages leave sent records where they are" <|
                    \() ->
                        let
                            model =
                                atStage (SyncUploadAuthority uploadDone) [ "hc-A" ]
                        in
                        Expect.all
                            [ \() -> photoPagesLeaveUnchanged { model | syncCycle = SyncCyclePause }
                            , \() -> photoPagesLeaveUnchanged { model | syncCycle = SyncCycleStayOnCurrentSyncStatus }
                            ]
                            ()
                ]
            , describe "Upload order and every other page's sync behaviour are unchanged"
                [ test "on every other page, a full cycle uploads photos, screenshots, general and health centre records, WhatsApp, then downloads" <|
                    \() ->
                        otherPages
                            |> List.filter (\( _, page ) -> Tuple.first (runSync page 12 (atStage SyncIdle [ "hc-A", "hc-B" ])) /= fullCycle)
                            |> List.map Tuple.first
                            |> Expect.equal []
                , test "on every other page, sync moves on from sent health centre records to WhatsApp" <|
                    \() ->
                        otherPages
                            |> List.filter (\( _, page ) -> stageName (determineSyncStatus page (atStage (SyncUploadAuthority uploadDone) [ "hc-A" ])) /= "upload whatsapp")
                            |> List.map Tuple.first
                            |> Expect.equal []

                -- Moving on from general records on a photo page keeps the
                -- usual order: each health centre's records, then WhatsApp.
                , test "on the photo pages, sync moves on through the record uploads in their usual order" <|
                    \() ->
                        photoPages
                            |> List.map (\( name, page ) -> ( name, Tuple.first (runSync page 3 (atStage (SyncUploadGeneral uploadDone) [ "hc-A", "hc-B" ])) ))
                            |> Expect.equal
                                [ ( "child page", [ "upload authority hc-A", "upload authority hc-B", "upload whatsapp" ] )
                                , ( "bulk Photo page", [ "upload authority hc-A", "upload authority hc-B", "upload whatsapp" ] )
                                ]
                ]
            ]
        ]
