module Backend.EducationSession.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.EducationSession.Decoder exposing (decodeEducationSession)
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..), Msg(..))
import Backend.EducationSession.Utils exposing (applyUpdateToSessions)
import Backend.Entities exposing (EducationSessionId, PersonId)
import Backend.NCDEncounter.Decoder exposing (decodeNCDEncounter)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.NutritionEncounter.Decoder exposing (decodeNutritionEncounter)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode exposing (decodeString)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Backend.EducationSession"
        [ applyUpdateToSessionsTests
        , emptyMultiValueFieldTests
        ]


{-| The server used to run an unguarded `explode(',', ...)` over multi-value
fields. A field with no values arrives from GROUP\_CONCAT as NULL, and exploding
NULL yields a single empty string, so an empty field synced as `[""]` instead of
as nothing at all.

For the enum fields that stayed invisible - their decoders drop the unparseable
"" - but `decodeEntityUuid` accepts "", and an education session is created
empty the moment the nurse begins it. So every device received a session with a
phantom empty-string participant in it.

The server now sends `null` for an empty field. These pin that each affected
decoder reads `null` exactly as it read the old `[""]`: the phantom participant
is gone, and the encounter fields are unchanged.

-}
emptyMultiValueFieldTests : Test
emptyMultiValueFieldTests =
    describe "empty multi-value fields"
        [ test "OLD payload: [\"\"] put a phantom empty UUID in the participant set" <|
            \_ ->
                decodeString decodeEducationSession (educationSessionJson "[\"\"]")
                    |> Result.map (.participants >> EverySet.toList >> List.map fromEntityUuid)
                    |> Expect.equal (Ok [ "" ])
        , test "NEW payload: null decodes to no participants at all" <|
            \_ ->
                decodeString decodeEducationSession (educationSessionJson "null")
                    |> Result.map (.participants >> EverySet.isEmpty)
                    |> Expect.equal (Ok True)
        , test "NCD diagnoses: null reads the same as the old [\"\"]" <|
            \_ ->
                let
                    decodeDiagnoses payload =
                        decodeString decodeNCDEncounter (ncdEncounterJson payload)
                            |> Result.map .diagnoses
                in
                ( decodeDiagnoses "[\"\"]", decodeDiagnoses "null" )
                    |> Expect.equal
                        ( Ok (EverySet.singleton NoNCDDiagnosis)
                        , Ok (EverySet.singleton NoNCDDiagnosis)
                        )
        , test "skipped forms: null reads the same as the old [\"\"]" <|
            \_ ->
                let
                    decodeSkipped payload =
                        decodeString decodeNutritionEncounter (nutritionEncounterJson payload)
                            |> Result.map (.skippedForms >> EverySet.isEmpty)
                in
                ( decodeSkipped "[\"\"]", decodeSkipped "null" )
                    |> Expect.equal ( Ok True, Ok True )
        ]


educationSessionJson : String -> String
educationSessionJson participants =
    """
    { "scheduled_date": { "value": "2026-07-14" }
    , "nurse": "nurse-uuid"
    , "village_ref": "village-uuid"
    , "education_topics": null
    , "participating_patients": """ ++ participants ++ """
    , "deleted": false
    }
    """


ncdEncounterJson : String -> String
ncdEncounterJson diagnoses =
    """
    { "individual_participant": "participant-uuid"
    , "scheduled_date": { "value": "2026-07-14" }
    , "ncd_diagnoses": """ ++ diagnoses ++ """
    , "deleted": false
    }
    """


nutritionEncounterJson : String -> String
nutritionEncounterJson skippedForms =
    """
    { "individual_participant": "participant-uuid"
    , "scheduled_date": { "value": "2026-07-14" }
    , "nutrition_encounter_type": "nurse"
    , "skipped_forms": """ ++ skippedForms ++ """
    , "deleted": false
    }
    """



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
