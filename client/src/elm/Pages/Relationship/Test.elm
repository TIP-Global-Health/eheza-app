module Pages.Relationship.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (HealthCenterId, PersonId, SessionId)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), emptyModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Relationship.Model exposing (MyRelationship)
import Backend.Utils exposing (clearFinishedPost)
import Date
import Expect
import Http
import Pages.Relationship.Fetch exposing (fetch)
import Pages.Relationship.Model exposing (emptyModel)
import Pages.Relationship.View exposing (view)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class)
import Time
import Translate.Model exposing (Language(..))


sessionId : SessionId
sessionId =
    toEntityUuid "session-1"


person1 : PersonId
person1 =
    toEntityUuid "person-1"


person2 : PersonId
person2 =
    toEntityUuid "person-2"


person3 : PersonId
person3 =
    toEntityUuid "person-3"


healthCenterId : HealthCenterId
healthCenterId =
    toEntityUuid "health-center-1"


adult : Person
adult =
    { name = "Adult One"
    , firstName = "One"
    , secondName = "Adult"
    , nationalIdNumber = Nothing
    , hmisNumber = Nothing
    , avatarUrl = Nothing
    , birthDate = Just (Date.fromCalendarDate 1990 Time.Jan 1)
    , isDateOfBirthEstimated = False
    , gender = Female
    , hivStatus = Nothing
    , numberOfChildren = Nothing
    , modeOfDelivery = Nothing
    , ubudehe = Nothing
    , educationLevel = Nothing
    , maritalStatus = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , registrationLatitude = Nothing
    , registrationLongitude = Nothing
    , saveGPSLocation = False
    , telephoneNumber = Nothing
    , spouseName = Nothing
    , spousePhoneNumber = Nothing
    , nextOfKinName = Nothing
    , nextOfKinPhoneNumber = Nothing
    , healthCenterId = Nothing
    , deleted = False
    , shard = Nothing
    }


child : Person
child =
    { adult | name = "Child", birthDate = Just (Date.fromCalendarDate 2025 Time.Jan 1) }


{-| Everything the page needs is loaded: the adult and two children, with
no relationships yet.
-}
loadedDb : ModelIndexedDb
loadedDb =
    { emptyModelIndexedDb
        | people =
            Dict.fromList
                [ ( person1, Success adult )
                , ( person2, Success child )
                , ( person3, Success child )
                ]
        , relationshipsByPerson = Dict.fromList [ ( person1, Success Dict.empty ) ]
        , participantsByPerson = Dict.fromList [ ( person1, Success Dict.empty ) ]
        , clinics = Success Dict.empty
    }


renderPage : PersonId -> ModelIndexedDb -> Query.Single Pages.Relationship.Model.Msg
renderPage relatedId db =
    view English (Date.fromCalendarDate 2026 Time.Nov 12) SiteRwanda ( healthCenterId, Nothing ) False ParticipantDirectoryOrigin person1 relatedId db emptyModel
        |> Query.fromHtml


failedSave : Dict PersonId (WebData MyRelationship)
failedSave =
    Dict.fromList [ ( person1, Failure Http.NetworkError ) ]


expectNoBanner : Query.Single msg -> Expect.Expectation
expectNoBanner =
    Query.findAll [ class "warning", class "message" ] >> Query.count (Expect.equal 0)


saveErrorBannerTest : Test
saveErrorBannerTest =
    describe "Pages.Relationship.View: save error banner"
        [ test "no banner when no save was attempted" <|
            \_ ->
                renderPage person3 loadedDb
                    |> expectNoBanner
        , test "a failed save shows the banner" <|
            \_ ->
                renderPage person2 { loadedDb | postRelationship = failedSave }
                    |> Query.findAll [ class "warning", class "message" ]
                    |> Query.count (Expect.equal 1)
        , test "no banner on another pair once the failed save is cleared on entry" <|
            \_ ->
                renderPage person3 { loadedDb | postRelationship = clearFinishedPost person1 failedSave }
                    |> expectNoBanner
        ]


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
        [ fetchTest, saveErrorBannerTest ]
