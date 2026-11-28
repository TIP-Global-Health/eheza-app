module Pages.People.Test exposing (all)

import Backend.Entities exposing (PersonId, SessionId)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator(..))
import Expect
import Pages.People.Fetch exposing (fetch)
import Pages.People.Model exposing (emptyModel)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)


sessionId : SessionId
sessionId =
    toEntityUuid "session-1"


personId : PersonId
personId =
    toEntityUuid "person-1"


fetchTest : Test
fetchTest =
    describe "Pages.People.Fetch.fetch"
        [ test "asks for the session when we came from a group encounter" <|
            -- The search results are filtered by the group's graduating age, which
            -- needs the session; without it every child is listed.
            \_ ->
                fetch Nothing (GroupEncounterOrigin sessionId) emptyModel
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal True
        , test "does not ask for a session when we came from the participant directory" <|
            \_ ->
                fetch Nothing ParticipantDirectoryOrigin emptyModel
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal False
        , test "still asks for what the page always needed" <|
            \_ ->
                fetch Nothing (GroupEncounterOrigin sessionId) emptyModel
                    |> Expect.all
                        [ List.member FetchHealthCenters >> Expect.equal True
                        , List.member FetchVillages >> Expect.equal True
                        ]
        , test "asks for the relationships of the person we add a relation to" <|
            -- Without them, people already related to that person are listed.
            \_ ->
                fetch (Just personId) ParticipantDirectoryOrigin emptyModel
                    |> Expect.all
                        [ List.member (FetchPerson personId) >> Expect.equal True
                        , List.member (FetchRelationshipsForPerson personId) >> Expect.equal True
                        ]
        ]


all : Test
all =
    describe "Pages.People"
        [ fetchTest ]
