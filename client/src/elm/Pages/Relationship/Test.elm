module Pages.Relationship.Test exposing (all)

import Backend.Entities exposing (PersonId, SessionId)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator(..))
import Expect
import Pages.Relationship.Fetch exposing (fetch)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)


sessionId : SessionId
sessionId =
    toEntityUuid "session-1"


person1 : PersonId
person1 =
    toEntityUuid "person-1"


person2 : PersonId
person2 =
    toEntityUuid "person-2"


fetchTest : Test
fetchTest =
    describe "Pages.Relationship.Fetch.fetch"
        [ test "asks for the session when we came from a group encounter" <|
            -- The page reads the session to know which group it belongs to. Without
            -- this the group selector has no options at all and the nurse can't save.
            \_ ->
                fetch person1 person2 (GroupEncounterOrigin sessionId)
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal True
        , test "does not ask for a session when we came from the participant directory" <|
            \_ ->
                fetch person1 person2 ParticipantDirectoryOrigin
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal False
        , test "still asks for what the page always needed" <|
            \_ ->
                fetch person1 person2 (GroupEncounterOrigin sessionId)
                    |> Expect.all
                        [ List.member (FetchRelationshipsForPerson person1) >> Expect.equal True
                        , List.member (FetchParticipantsForPerson person1) >> Expect.equal True
                        , List.member (FetchPerson person1) >> Expect.equal True
                        , List.member (FetchPerson person2) >> Expect.equal True
                        , List.member FetchClinics >> Expect.equal True
                        ]
        ]


all : Test
all =
    describe "Pages.Relationship"
        [ fetchTest ]
