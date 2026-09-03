module Pages.GlobalCaseManagement.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model as IndividualEncounterParticipant exposing (emptyIndividualEncounterParticipant)
import Backend.Measurement.Model exposing (FollowUpOption(..), FollowUpValue)
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.TuberculosisEncounter.Model exposing (TuberculosisEncounter)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..), emptyWellChildEncounter)
import Date
import EverySet
import Expect
import Pages.GlobalCaseManagement.Model exposing (AcuteIllnessFollowUpItem, EncounterStartedToday(..), ImmunizationFollowUpItem)
import Pages.GlobalCaseManagement.Utils exposing (filterResolvedFollowUps, generateTuberculosisFollowUps, resolveEncounterStartedToday)
import Pages.GlobalCaseManagement.View exposing (generateImmunizationFollowUpEntries)
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.GlobalCaseManagement"
        [ filterResolvedFollowUpsTests
        , generateImmunizationFollowUpEntriesTests
        , generateTuberculosisFollowUpsTests
        , resolveEncounterStartedTodayTests
        ]


{-| filterResolvedFollowUps is the keep-predicate for pending follow-ups:
an item resolved ON or BEFORE the limit date is resolved as of that date
and must be dropped; an item resolved AFTER the limit date (or not at
all) was still open and must be kept. The server stamps resolution dates
at resolution time, so for the case-management view (limit = today)
every resolved item must be dropped.
-}
filterResolvedFollowUpsTests : Test
filterResolvedFollowUpsTests =
    let
        limitDate =
            Date.fromCalendarDate 2026 Time.Jun 15
    in
    describe "filterResolvedFollowUps"
        [ test "drops a follow up resolved before the limit date" <|
            \_ ->
                filterResolvedFollowUps limitDate (Just <| Date.fromCalendarDate 2026 Time.Jun 10)
                    |> Expect.equal False
        , test "drops a follow up resolved on the limit date" <|
            \_ ->
                filterResolvedFollowUps limitDate (Just <| Date.fromCalendarDate 2026 Time.Jun 15)
                    |> Expect.equal False
        , test "keeps a follow up resolved after the limit date" <|
            \_ ->
                filterResolvedFollowUps limitDate (Just <| Date.fromCalendarDate 2026 Time.Jun 20)
                    |> Expect.equal True
        , test "keeps a follow up with no resolution date" <|
            \_ ->
                filterResolvedFollowUps limitDate Nothing
                    |> Expect.equal True
        ]


{-| The immunization pane lists a child whose Well Child encounter left
immunisations owed, until a later Well Child encounter takes place. The
encounter that scheduled the follow up starts on the day the follow up
was measured, so it is not the one that answers it.
-}
generateImmunizationFollowUpEntriesTests : Test
generateImmunizationFollowUpEntriesTests =
    let
        personId =
            toEntityUuid "person"

        participantId =
            toEntityUuid "well-child-participant"

        date year month day =
            Date.fromCalendarDate year month day

        scheduledOn =
            date 2026 Time.Jun 1

        item =
            ImmunizationFollowUpItem scheduledOn (date 2026 Time.Jun 8) "Patient"

        dbWithEncountersOn startDates =
            { emptyModelIndexedDb
                | individualParticipantsByPerson =
                    Dict.singleton personId
                        (RemoteData.Success <|
                            Dict.singleton participantId
                                (emptyIndividualEncounterParticipant scheduledOn
                                    personId
                                    IndividualEncounterParticipant.WellChildEncounter
                                    (toEntityUuid "health-center")
                                )
                        )
                , wellChildEncountersByParticipant =
                    Dict.singleton participantId
                        (RemoteData.Success <|
                            Dict.fromList <|
                                List.indexedMap
                                    (\index startDate ->
                                        ( toEntityUuid <| "well-child-encounter-" ++ String.fromInt index
                                        , emptyWellChildEncounter participantId startDate PediatricCare Nothing
                                        )
                                    )
                                    startDates
                        )
            }

        run currentDate startDates =
            generateImmunizationFollowUpEntries
                (Date.add Date.Days 1 currentDate)
                (Dict.singleton personId item)
                (dbWithEncountersOn startDates)
                |> List.length
    in
    describe "generateImmunizationFollowUpEntries"
        [ test "lists the child on the day the follow up was scheduled" <|
            \_ ->
                run scheduledOn [ scheduledOn ]
                    |> Expect.equal 1
        , test "still lists the child on later days, when no Well Child encounter has taken place since" <|
            \_ ->
                run (date 2026 Time.Jun 20) [ scheduledOn ]
                    |> Expect.equal 1
        , test "lists the child when the only Well Child encounter predates the follow up" <|
            \_ ->
                run (date 2026 Time.Jun 20) [ date 2026 Time.May 1 ]
                    |> Expect.equal 1
        , test "lists the child when there is no Well Child encounter at all" <|
            \_ ->
                run (date 2026 Time.Jun 20) []
                    |> Expect.equal 1
        , test "drops the child once a Well Child encounter takes place after the follow up" <|
            \_ ->
                run (date 2026 Time.Jun 20) [ scheduledOn, date 2026 Time.Jun 15 ]
                    |> Expect.equal 0
        ]


{-| The Tuberculosis pane merges follow ups recorded at a Tuberculosis
encounter with follow ups recorded at an Acute Illness encounter where
Tuberculosis was suspected. When a patient has both, the more recent one
is shown, and the patient must appear either way.
-}
generateTuberculosisFollowUpsTests : Test
generateTuberculosisFollowUpsTests =
    let
        limitDate =
            Date.fromCalendarDate 2026 Time.Jun 15

        personId =
            toEntityUuid "person"

        participantId =
            toEntityUuid "tuberculosis-participant"

        encounterId =
            toEntityUuid "tuberculosis-encounter"

        db =
            { emptyModelIndexedDb
                | tuberculosisEncounters =
                    Dict.singleton encounterId
                        (RemoteData.Success
                            (TuberculosisEncounter participantId
                                (Date.fromCalendarDate 2026 Time.Jun 1)
                                Nothing
                                False
                                Nothing
                            )
                        )
            }

        followUpsWithTuberculosisAt date =
            { nutritionGroup = Dict.empty
            , nutritionIndividual = Dict.empty
            , acuteIllness = Dict.empty
            , prenatal = Dict.empty
            , wellChild = Dict.empty
            , tuberculosis =
                Dict.singleton (toEntityUuid "tuberculosis-follow-up")
                    { dateMeasured = date
                    , nurse = Nothing
                    , healthCenter = Nothing
                    , participantId = personId
                    , deleted = False
                    , encounterId = Just encounterId
                    , value = FollowUpValue (EverySet.singleton OneMonth) Nothing
                    }
            , hiv = Dict.empty
            , traceContacts = Dict.empty
            , prenatalLabs = Dict.empty
            , ncdLabs = Dict.empty
            , nextVisit = Dict.empty
            }

        acuteIllnessFollowUpAt date =
            Dict.singleton ( toEntityUuid "acute-illness-participant", personId )
                (AcuteIllnessFollowUpItem date
                    "Patient"
                    (Just <| toEntityUuid "acute-illness-encounter")
                    1
                    { options = EverySet.singleton OneMonth
                    , resolutionDate = Nothing
                    , diagnosis = Nothing
                    }
                )

        run tuberculosisDate acuteIllnessDate =
            generateTuberculosisFollowUps limitDate
                db
                (followUpsWithTuberculosisAt tuberculosisDate)
                (acuteIllnessFollowUpAt acuteIllnessDate)
    in
    describe "generateTuberculosisFollowUps"
        [ test "keeps the Tuberculosis follow up when it is the more recent of the two" <|
            \_ ->
                run (Date.fromCalendarDate 2026 Time.Jun 10) (Date.fromCalendarDate 2026 Time.Jun 1)
                    |> Tuple.first
                    |> Dict.keys
                    |> Expect.equal [ ( participantId, personId ) ]
        , test "keeps the Tuberculosis follow up when both were recorded on the same day" <|
            \_ ->
                run (Date.fromCalendarDate 2026 Time.Jun 10) (Date.fromCalendarDate 2026 Time.Jun 10)
                    |> Tuple.first
                    |> Dict.keys
                    |> Expect.equal [ ( participantId, personId ) ]
        , test "takes the Acute Illness date when that follow up is the more recent, keeping the Tuberculosis encounter" <|
            \_ ->
                run (Date.fromCalendarDate 2026 Time.Jun 1) (Date.fromCalendarDate 2026 Time.Jun 10)
                    |> Tuple.first
                    |> Dict.values
                    |> List.map (\item -> ( item.dateMeasured, item.encounterId ))
                    |> Expect.equal [ ( Date.fromCalendarDate 2026 Time.Jun 10, Just encounterId ) ]
        ]


{-| Tapping a Case Management follow up entry must not open a second encounter
on a day the patient already had one. Today's open encounter is offered
instead, and a completed one leaves nothing to do.
-}
resolveEncounterStartedTodayTests : Test
resolveEncounterStartedTodayTests =
    let
        currentDate =
            Date.fromCalendarDate 2026 Time.Jun 15

        encounter id startDate endDate =
            ( toEntityUuid id
            , { startDate = startDate, endDate = endDate }
            )

        today =
            currentDate

        yesterday =
            Date.fromCalendarDate 2026 Time.Jun 14
    in
    describe "resolveEncounterStartedToday"
        [ test "reports no encounter when the patient has none at all" <|
            \_ ->
                resolveEncounterStartedToday currentDate []
                    |> Expect.equal NoEncounterStartedToday
        , test "reports no encounter when every encounter predates today" <|
            \_ ->
                resolveEncounterStartedToday currentDate
                    [ encounter "a" yesterday (Just yesterday) ]
                    |> Expect.equal NoEncounterStartedToday
        , test "offers today's encounter while it is still open" <|
            \_ ->
                resolveEncounterStartedToday currentDate
                    [ encounter "a" yesterday (Just yesterday), encounter "b" today Nothing ]
                    |> Expect.equal (EncounterActiveToday <| toEntityUuid "b")
        , test "reports a completed encounter when today's was already closed" <|
            \_ ->
                resolveEncounterStartedToday currentDate
                    [ encounter "a" today (Just today) ]
                    |> Expect.equal EncounterCompletedToday
        , test "offers the open one when today holds both a closed and an open encounter" <|
            \_ ->
                resolveEncounterStartedToday currentDate
                    [ encounter "a" today (Just today), encounter "b" today Nothing ]
                    |> Expect.equal (EncounterActiveToday <| toEntityUuid "b")
        , test "ignores an encounter left open on an earlier day" <|
            \_ ->
                resolveEncounterStartedToday currentDate
                    [ encounter "a" yesterday Nothing ]
                    |> Expect.equal NoEncounterStartedToday
        , test "does not offer an encounter left open on an earlier day when today's was completed" <|
            \_ ->
                resolveEncounterStartedToday currentDate
                    [ encounter "a" yesterday Nothing, encounter "b" today (Just today) ]
                    |> Expect.equal EncounterCompletedToday
        ]
