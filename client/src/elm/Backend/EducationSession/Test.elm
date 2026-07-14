module Backend.EducationSession.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..), Msg(..))
import Backend.EducationSession.Utils exposing (applyUpdateToSessions)
import Backend.Entities exposing (EducationSessionId, PersonId)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Backend.EducationSession"
        [ applyUpdateToSessionsTests ]



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


sessions : Dict EducationSessionId (WebData EducationSession)
sessions =
    Dict.singleton sessionId (Success session)


cachedSession : Dict EducationSessionId (WebData EducationSession) -> Maybe EducationSession
cachedSession =
    Dict.get sessionId >> Maybe.andThen RemoteData.toMaybe



-- TESTS


{-| The scenarios mirror issue #1927. Every education session write is a full
entity PATCH, rebuilt from the session held at the sessions dict. That dict used
to be refreshed only once the revision for the PATCH echoed back from the
service worker, asynchronously - so a write that fired before the echo of its
predecessor landed rebuilt the entity from the pre-update session, and reverted
the field that predecessor had set.

The session held at the dict is exactly what the next PATCH is built from, which
is why asserting on it is asserting on the payload that gets written. Every test
here applies its updates back to back, with no revision echoing in between.

-}
applyUpdateToSessionsTests : Test
applyUpdateToSessionsTests =
    let
        checkInParticipants participants =
            applyUpdateToSessions sessionId <|
                Update (\value -> { value | participants = participants })

        selectTopics topics =
            applyUpdateToSessions sessionId <|
                Update (\value -> { value | topics = topics })

        endSession =
            applyUpdateToSessions sessionId <|
                Update (\value -> { value | endDate = Just currentDate })
    in
    describe "applyUpdateToSessions"
        [ test "an update is applied to the session right away, without waiting for its revision to echo back" <|
            \_ ->
                sessions
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
                sessions
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
                sessions
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
        , test "a message that is not an update leaves the session as is" <|
            \_ ->
                sessions
                    |> applyUpdateToSessions sessionId (HandleUpdated (Success ()))
                    |> cachedSession
                    |> Expect.equal (Just session)
        , test "an update for a session we do not hold is a no-op" <|
            \_ ->
                Dict.empty
                    |> endSession
                    |> Expect.equal Dict.empty
        ]
