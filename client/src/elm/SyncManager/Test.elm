module SyncManager.Test exposing (all)

import EverySet
import Expect
import RemoteData
import SyncManager.Model
    exposing
        ( DownloadPhotosStatus(..)
        , Flags
        , Model
        , Site(..)
        , SyncCycle(..)
        , SyncInfoStatus(..)
        , SyncStatus(..)
        , emptyModel
        )
import SyncManager.Utils exposing (determineDownloadPhotosStatus)
import Test exposing (Test, describe, test)


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
        ]
