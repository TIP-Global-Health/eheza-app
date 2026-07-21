module Pages.Person.Test exposing (all)

import Backend.Entities exposing (PersonId, SessionId)
import Backend.Model exposing (MsgIndexedDb(..), emptyModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..))
import Expect
import Pages.Person.Fetch exposing (fetchForCreateOrEdit)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)


sessionId : SessionId
sessionId =
    toEntityUuid "session-1"


relatedId : PersonId
relatedId =
    toEntityUuid "person-1"


fetchForCreateOrEditTest : Test
fetchForCreateOrEditTest =
    describe "Pages.Person.Fetch.fetchForCreateOrEdit"
        [ test "asks for the session when we came from a group encounter" <|
            -- The form limits the birth date by the group's age range, which it
            -- can only do with the session in hand.
            \_ ->
                fetchForCreateOrEdit Nothing (GroupEncounterOrigin sessionId) emptyModelIndexedDb
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal True
        , test "asks for the session when registering a relative of someone" <|
            \_ ->
                fetchForCreateOrEdit (Just relatedId) (GroupEncounterOrigin sessionId) emptyModelIndexedDb
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal True
        , test "does not ask for a session when we came from the participant directory" <|
            \_ ->
                fetchForCreateOrEdit Nothing ParticipantDirectoryOrigin emptyModelIndexedDb
                    |> List.member (FetchSession sessionId)
                    |> Expect.equal False
        , test "still asks for what the form always needed" <|
            \_ ->
                fetchForCreateOrEdit Nothing (GroupEncounterOrigin sessionId) emptyModelIndexedDb
                    |> Expect.all
                        [ List.member FetchHealthCenters >> Expect.equal True
                        , List.member FetchVillages >> Expect.equal True
                        , List.member FetchClinics >> Expect.equal True
                        ]
        ]


all : Test
all =
    describe "Pages.Person"
        [ fetchForCreateOrEditTest ]
