module Backend.EducationSession.Test exposing (all)

import AssocList as Dict
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..))
import Backend.Entities exposing (EducationSessionId, PersonId)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), emptyModelIndexedDb)
import Backend.Update exposing (updateIndexedDb)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Flags, Site(..))
import Test exposing (Test, describe, test)
import Time
import Translate.Model exposing (Language(..))
import ZScore.Model


all : Test
all =
    describe "Backend.EducationSession"
        [ optimisticSessionCacheTests ]



-- FIXTURES


currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


sessionId : EducationSessionId
sessionId =
    toEntityUuid "education-session"


firstParticipant : PersonId
firstParticipant =
    toEntityUuid "person-1"


secondParticipant : PersonId
secondParticipant =
    toEntityUuid "person-2"


{-| A session as it stands right after being created - no topics were selected,
and no participants were checked in yet.
-}
session : EducationSession
session =
    { startDate = currentDate
    , nurse = toEntityUuid "nurse"
    , village = toEntityUuid "village"
    , topics = EverySet.empty
    , participants = EverySet.empty
    , endDate = Nothing
    , deleted = False
    , shard = Nothing
    }


modelWithSession : ModelIndexedDb
modelWithSession =
    { emptyModelIndexedDb
        | educationSessions = Dict.singleton sessionId (Success session)
    }


syncManagerFlags : Flags
syncManagerFlags =
    { syncInfoGeneral =
        { lastFetchedRevisionId = 0
        , lastSuccesfulContact = 0
        , remainingToUpload = 0
        , remainingToDownload = 0
        , deviceName = ""
        , status = SyncManager.Model.NotAvailable
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


{-| Runs an education session message through the real `updateIndexedDb`, and
hands back the resulting `ModelIndexedDb`. The only inputs that matter here are
the message and the model - the rest are inert defaults.
-}
applyMsg : Backend.EducationSession.Model.Msg -> ModelIndexedDb -> ModelIndexedDb
applyMsg subMsg model =
    let
        ( updated, _, _ ) =
            updateIndexedDb English
                currentDate
                (Time.millisToPosix 0)
                Nothing
                ZScore.Model.emptyModel
                SiteRwanda
                EverySet.empty
                Nothing
                Nothing
                Nothing
                False
                False
                PinCodePage
                (SyncManager.Model.emptyModel syncManagerFlags)
                (MsgEducationSession sessionId subMsg)
                model
    in
    updated


cachedSession : ModelIndexedDb -> Maybe EducationSession
cachedSession model =
    Dict.get sessionId model.educationSessions
        |> Maybe.andThen RemoteData.toMaybe



-- TESTS


{-| The scenarios mirror issue #1927. Every education session write is a full
entity PATCH, rebuilt from the session held at `educationSessions`. That dict
used to be refreshed only once the revision for the PATCH echoed back from the
service worker, asynchronously - so a write that fired before the echo of its
predecessor landed rebuilt the entity from the pre-update session, and reverted
the field that predecessor had set.

The cached session is exactly what the next PATCH is built from, which is why
asserting on it is asserting on the payload that gets written.

-}
optimisticSessionCacheTests : Test
optimisticSessionCacheTests =
    let
        checkInParticipants participants =
            applyMsg <| Backend.EducationSession.Model.Update (\value -> { value | participants = participants })

        selectTopics topics =
            applyMsg <| Backend.EducationSession.Model.Update (\value -> { value | topics = topics })

        endSession =
            applyMsg <| Backend.EducationSession.Model.Update (\value -> { value | endDate = Just currentDate })
    in
    describe "education session cache"
        [ test "an update is applied to the cached session right away, without waiting for its revision to echo back" <|
            \_ ->
                modelWithSession
                    |> checkInParticipants (EverySet.singleton firstParticipant)
                    |> cachedSession
                    |> Maybe.map .participants
                    |> Expect.equal (Just <| EverySet.singleton firstParticipant)
        , test "ending a session right after a participant was checked in does not drop that participant" <|
            \_ ->
                let
                    participants =
                        EverySet.fromList [ firstParticipant, secondParticipant ]
                in
                modelWithSession
                    |> checkInParticipants participants
                    |> endSession
                    |> cachedSession
                    |> Expect.equal
                        (Just
                            { session
                                | participants = participants
                                , endDate = Just currentDate
                            }
                        )
        , test "checking in a participant right after topics were selected does not drop the topics" <|
            \_ ->
                let
                    topics =
                        EverySet.fromList [ TopicMalaria, TopicNCD ]
                in
                modelWithSession
                    |> selectTopics topics
                    |> checkInParticipants (EverySet.singleton firstParticipant)
                    |> cachedSession
                    |> Expect.equal
                        (Just
                            { session
                                | topics = topics
                                , participants = EverySet.singleton firstParticipant
                            }
                        )
        , test "a message that is not an update leaves the cached session as is" <|
            \_ ->
                modelWithSession
                    |> applyMsg (Backend.EducationSession.Model.HandleUpdated (Success ()))
                    |> cachedSession
                    |> Expect.equal (Just session)
        ]
