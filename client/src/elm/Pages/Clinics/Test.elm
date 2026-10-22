module Pages.Clinics.Test exposing (all)

import AssocList as Dict
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (ClinicId, HealthCenterId)
import Backend.Model exposing (MsgIndexedDb(..), emptyModelIndexedDb)
import Expect
import List.Zipper as Zipper
import Pages.Clinics.Fetch exposing (fetch)
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (SyncInfoStatus(..), emptySyncInfoAuthority)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testSyncManagerModel)


healthCenterId : HealthCenterId
healthCenterId =
    toEntityUuid "hc-1"


clinicId : ClinicId
clinicId =
    toEntityUuid "clinic-1"


{-| The messages the Clinics page asks for, with one FBF group at the health
center, and the health center at the given sync status and download backlog.
-}
fetchAt : SyncInfoStatus -> Int -> List MsgIndexedDb
fetchAt status remainingToDownload =
    let
        authority =
            emptySyncInfoAuthority "hc-1"

        db =
            { emptyModelIndexedDb
                | clinics =
                    Dict.singleton clinicId
                        { name = "Group"
                        , healthCenterId = healthCenterId
                        , clinicType = Fbf
                        , villageId = Nothing
                        }
                        |> RemoteData.Success
            }

        syncManager =
            { testSyncManagerModel
                | syncInfoAuthorities =
                    Just (Zipper.singleton { authority | status = status, remainingToDownload = remainingToDownload })
            }
    in
    fetch healthCenterId db syncManager { clinicType = Just Fbf }


fetchTest : Test
fetchTest =
    describe "Pages.Clinics.Fetch.fetch"
        [ test "asks for the group's sessions while the health center is uploading" <|
            -- The page shows the groups while uploading. Without the sessions,
            -- a tap on the group creates a second session for today.
            \_ ->
                fetchAt Uploading 0
                    |> List.member (FetchSessionsByClinic clinicId)
                    |> Expect.equal True
        , test "asks for the group's sessions during a small download" <|
            \_ ->
                fetchAt Downloading 100
                    |> List.member (FetchSessionsByClinic clinicId)
                    |> Expect.equal True
        , test "asks for the group's sessions once synced" <|
            \_ ->
                fetchAt Success 0
                    |> List.member (FetchSessionsByClinic clinicId)
                    |> Expect.equal True
        , test "does not ask while the page is hidden for a large download" <|
            \_ ->
                fetchAt Downloading 2000
                    |> List.member (FetchSessionsByClinic clinicId)
                    |> Expect.equal False
        , test "does not ask while the health center is not synced" <|
            \_ ->
                fetchAt NotAvailable 0
                    |> List.member (FetchSessionsByClinic clinicId)
                    |> Expect.equal False
        ]


all : Test
all =
    describe "Pages.Clinics"
        [ fetchTest
        ]
