module Pages.Person.Test exposing (all)

import AssocList as Dict
import Backend.Entities exposing (AcuteIllnessEncounterId, IndividualEncounterParticipantId, PersonId, PrenatalEncounterId, SessionId)
import Backend.FamilyEncounterParticipant.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterParticipantOutcome(..), IndividualEncounterType(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (emptyPrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), emptyModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..), emptyPrenatalEncounter)
import Date
import Expect exposing (Expectation)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Person.Fetch exposing (fetchForCreateOrEdit)
import Pages.Prenatal.Activity.Types exposing (NextStepsTask(..))
import Pages.Prenatal.Activity.Utils exposing (nextStepsTaskCompleted, resolveNextStepsTasks)
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures
import Time


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



-- NEWBORN ENROLMENT FROM PRENATAL NEXT STEPS
--
-- On a CHW postpartum visit after a live birth, Next Steps -> Enrol newborn
-- opens the person create page with the mother as the related person and the
-- `PrenatalNextStepsNewbornEnrolmentOrigin` initiator. On Save the app links
-- the baby to the pregnancy by reading the visit (prenatal encounter) from
-- memory, finding its pregnancy (the individual encounter participant), and
-- updating that participant, which must also be in memory.
--
-- The app keeps in memory only what the current page keeps asking for: data
-- nobody asks for for five minutes is dropped, and a reload starts with
-- nothing. So while the registration form is open, the page itself has to
-- keep asking for the visit and for its pregnancy, whatever the state of the
-- cache. These tests read the page's request list in each cache state the
-- form can meet:
--
--   - both loaded (the form was just opened from Next Steps, and it must keep
--     them alive however long it stays open);
--   - nothing loaded (just after a reload, or after both were dropped);
--   - the visit loaded but not its pregnancy (the step after a reload, once
--     the visit has arrived and names its pregnancy);
--   - the visit or the pregnancy still loading.
--
-- A request for one id may be made with either the single or the list form
-- of the fetch message; both keep the record in memory, so both are accepted.


newbornBirthDate : NominalDate
newbornBirthDate =
    Date.fromCalendarDate 2026 Time.Oct 30


visitDate : NominalDate
visitDate =
    Date.fromCalendarDate 2026 Time.Nov 2


motherId : PersonId
motherId =
    toEntityUuid "mother-1"


babyId : PersonId
babyId =
    toEntityUuid "baby-1"


visitId : PrenatalEncounterId
visitId =
    toEntityUuid "postpartum-visit-1"


pregnancyId : IndividualEncounterParticipantId
pregnancyId =
    toEntityUuid "pregnancy-1"


{-| Some other pregnancy, to check that the page asks for the one the visit
names rather than any pregnancy it happens to know about.
-}
otherPregnancyId : IndividualEncounterParticipantId
otherPregnancyId =
    toEntityUuid "pregnancy-other"


contactsTracingEncounterId : AcuteIllnessEncounterId
contactsTracingEncounterId =
    toEntityUuid "acute-illness-encounter-1"


newbornEnrolment : Initiator
newbornEnrolment =
    PrenatalNextStepsNewbornEnrolmentOrigin newbornBirthDate visitId


{-| The CHW postpartum visit the Enrol newborn task was opened from.
-}
postpartumVisit : PrenatalEncounter
postpartumVisit =
    emptyPrenatalEncounter pregnancyId visitDate ChwPostpartumEncounter Nothing


{-| The mother's pregnancy, concluded with a live birth, and with no newborn
linked yet.
-}
pregnancy : IndividualEncounterParticipant
pregnancy =
    let
        base =
            TestFixtures.testParticipant (Date.add Date.Months -9 visitDate) AntenatalEncounter
    in
    { base
        | person = motherId
        , dateConcluded = Just newbornBirthDate
        , outcome = Just (Pregnancy OutcomeLiveAtTerm)
    }


withVisit : WebData PrenatalEncounter -> ModelIndexedDb -> ModelIndexedDb
withVisit visit db =
    { db | prenatalEncounters = Dict.insert visitId visit db.prenatalEncounters }


withPregnancy : IndividualEncounterParticipantId -> WebData IndividualEncounterParticipant -> ModelIndexedDb -> ModelIndexedDb
withPregnancy id participant db =
    { db | individualParticipants = Dict.insert id participant db.individualParticipants }


{-| The cache as it is when the CHW taps Enrol newborn: Next Steps had the
visit and its pregnancy loaded.
-}
dbWithVisitAndPregnancy : ModelIndexedDb
dbWithVisitAndPregnancy =
    emptyModelIndexedDb
        |> withVisit (Success postpartumVisit)
        |> withPregnancy pregnancyId (Success pregnancy)


asksForVisit : PrenatalEncounterId -> List MsgIndexedDb -> Bool
asksForVisit id =
    List.any
        (\msg ->
            case msg of
                FetchPrenatalEncounter requested ->
                    requested == id

                FetchPrenatalEncounters requested ->
                    List.member id requested

                _ ->
                    False
        )


asksForPregnancy : IndividualEncounterParticipantId -> List MsgIndexedDb -> Bool
asksForPregnancy id =
    List.any
        (\msg ->
            case msg of
                FetchIndividualEncounterParticipant requested ->
                    requested == id

                FetchIndividualEncounterParticipants requested ->
                    List.member id requested

                _ ->
                    False
        )


newbornEnrolmentFetchTest : Test
newbornEnrolmentFetchTest =
    describe "Enrol newborn: the registration form keeps the visit and its pregnancy in memory"
        [ test "Registering the baby from Enrol newborn links it to the pregnancy, however long the registration form was open before Save" <|
            -- The form was opened from Next Steps with the visit and its
            -- pregnancy in memory. Every time the app checks what is wanted,
            -- the form has to ask for both again, or they are dropped five
            -- minutes later and Save can no longer link the baby.
            \_ ->
                fetchForCreateOrEdit (Just motherId) newbornEnrolment dbWithVisitAndPregnancy
                    |> Expect.all
                        [ asksForVisit visitId >> Expect.equal True
                        , asksForPregnancy pregnancyId >> Expect.equal True
                        ]
        , test "The same holds when the app reloads while the registration form is open, and the baby is then saved" <|
            -- Straight after a reload nothing is in memory. The form only
            -- knows the visit id, so it has to ask for the visit first.
            \_ ->
                fetchForCreateOrEdit (Just motherId) newbornEnrolment emptyModelIndexedDb
                    |> asksForVisit visitId
                    |> Expect.equal True
        , test "after a reload, once the visit has arrived, asks for the pregnancy the visit names" <|
            -- The second step after a reload: the visit is back and names its
            -- pregnancy, which is not loaded yet. Another pregnancy is known,
            -- to make sure it is the visit's own that gets asked for.
            \_ ->
                emptyModelIndexedDb
                    |> withVisit (Success postpartumVisit)
                    |> withPregnancy otherPregnancyId (Success pregnancy)
                    |> fetchForCreateOrEdit (Just motherId) newbornEnrolment
                    |> asksForPregnancy pregnancyId
                    |> Expect.equal True
        , test "keeps asking for the visit while it is still loading" <|
            \_ ->
                emptyModelIndexedDb
                    |> withVisit Loading
                    |> fetchForCreateOrEdit (Just motherId) newbornEnrolment
                    |> asksForVisit visitId
                    |> Expect.equal True
        , test "keeps asking for the visit and the pregnancy while the pregnancy is still loading" <|
            \_ ->
                emptyModelIndexedDb
                    |> withVisit (Success postpartumVisit)
                    |> withPregnancy pregnancyId Loading
                    |> fetchForCreateOrEdit (Just motherId) newbornEnrolment
                    |> Expect.all
                        [ asksForVisit visitId >> Expect.equal True
                        , asksForPregnancy pregnancyId >> Expect.equal True
                        ]
        , test "keeps asking for the visit and the pregnancy after a newborn is already linked" <|
            -- The other shape of `newborn`. Nothing about the request list
            -- should depend on it: the page must keep reaching the same
            -- pregnancy whether or not it already holds a link.
            \_ ->
                emptyModelIndexedDb
                    |> withVisit (Success postpartumVisit)
                    |> withPregnancy pregnancyId (Success { pregnancy | newborn = Just babyId })
                    |> fetchForCreateOrEdit (Just motherId) newbornEnrolment
                    |> Expect.all
                        [ asksForVisit visitId >> Expect.equal True
                        , asksForPregnancy pregnancyId >> Expect.equal True
                        ]
        , test "still asks for the mother and for what the form always needed" <|
            -- The mother is the related person; Save creates the
            -- mother-child relationship from her.
            \_ ->
                fetchForCreateOrEdit (Just motherId) newbornEnrolment dbWithVisitAndPregnancy
                    |> Expect.all
                        [ List.member (FetchPerson motherId) >> Expect.equal True
                        , List.member FetchHealthCenters >> Expect.equal True
                        , List.member FetchVillages >> Expect.equal True
                        , List.member FetchClinics >> Expect.equal True
                        ]
        ]


{-| The requests every create/edit form makes without a related person or a
group session.
-}
baseRequests : List MsgIndexedDb
baseRequests =
    [ FetchHealthCenters, FetchVillages, FetchClinics ]


{-| Same requests in any order, each once.
-}
expectSameRequests : List MsgIndexedDb -> List MsgIndexedDb -> Expectation
expectSameRequests expected actual =
    Expect.all
        [ \_ -> Expect.equal (List.length expected) (List.length actual)
        , \_ -> Expect.equal [] (List.filter (\msg -> not (List.member msg actual)) expected)
        , \_ -> Expect.equal [] (List.filter (\msg -> not (List.member msg expected)) actual)
        ]
        ()


otherOriginsTest : Test
otherOriginsTest =
    let
        -- The visit and its pregnancy sit in memory, as they would if the CHW
        -- had been on Next Steps earlier. Registering from anywhere else must
        -- neither ask for them nor otherwise change what it asks for.
        db =
            dbWithVisitAndPregnancy

        criterion =
            "Registering a person from anywhere else (a group session, the participant directory, adding a family member) works as before"
    in
    describe criterion
        [ test (criterion ++ ": group session") <|
            \_ ->
                fetchForCreateOrEdit Nothing (GroupEncounterOrigin sessionId) db
                    |> expectSameRequests (baseRequests ++ [ FetchSession sessionId ])
        , test (criterion ++ ": participant directory") <|
            \_ ->
                fetchForCreateOrEdit Nothing ParticipantDirectoryOrigin db
                    |> expectSameRequests baseRequests
        , test (criterion ++ ": adding a family member") <|
            -- Adding a relative of the mother from the participant directory
            -- (also what editing her asks for): the related person is the
            -- mother, as with Enrol newborn, but the origin is not a visit.
            \_ ->
                fetchForCreateOrEdit (Just motherId) ParticipantDirectoryOrigin db
                    |> expectSameRequests (baseRequests ++ [ FetchPerson motherId ])
        , test "adding a family member to someone in a group session" <|
            \_ ->
                fetchForCreateOrEdit (Just motherId) (GroupEncounterOrigin sessionId) db
                    |> expectSameRequests (baseRequests ++ [ FetchPerson motherId, FetchSession sessionId ])
        , test "registering from an individual encounter" <|
            \_ ->
                fetchForCreateOrEdit Nothing (IndividualEncounterOrigin AntenatalEncounter) db
                    |> expectSameRequests baseRequests
        , test "registering a contact from acute illness contacts tracing" <|
            \_ ->
                fetchForCreateOrEdit Nothing (AcuteIllnessContactsTracingActivityOrigin contactsTracingEncounterId) db
                    |> expectSameRequests baseRequests
        , test "registering from a family encounter" <|
            \_ ->
                fetchForCreateOrEdit Nothing (FamilyEncounterOrigin Backend.FamilyEncounterParticipant.Model.NutritionEncounter) db
                    |> expectSameRequests baseRequests
        ]


{-| The cache Next Steps reads after Save: the visit, its measurements, its
pregnancy (with or without the newborn linked) and the mother.
-}
nextStepsDb : IndividualEncounterParticipant -> ModelIndexedDb
nextStepsDb participant =
    let
        db =
            emptyModelIndexedDb
                |> withVisit (Success postpartumVisit)
                |> withPregnancy pregnancyId (Success participant)
    in
    { db
        | prenatalMeasurements = Dict.singleton visitId (Success emptyPrenatalMeasurements)
        , people = Dict.singleton motherId (Success TestFixtures.testPerson)
    }


{-| Whether Next Steps offers the Enrol newborn task, and whether it reads it
as done, the way the page reads them: from the data assembled out of the cache.
-}
enrolNewbornTask : IndividualEncounterParticipant -> Maybe ( Bool, Bool )
enrolNewbornTask participant =
    generateAssembledData visitId (nextStepsDb participant)
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                ( List.member NextStepsNewbornEnrolment (resolveNextStepsTasks visitDate assembled)
                , nextStepsTaskCompleted visitDate assembled NextStepsNewbornEnrolment
                )
            )


nextStepsAfterSaveTest : Test
nextStepsAfterSaveTest =
    describe "Next Steps after Enrol newborn"
        [ test "After Save, back on Next Steps, the Enrol newborn task shows as done and no longer offers to register a baby" <|
            -- Once Save has linked the baby to the pregnancy, Next Steps reads
            -- the task as done. The Enrol newborn button is enabled only while
            -- the pregnancy has no newborn, the same condition as "not done".
            \_ ->
                enrolNewbornTask { pregnancy | newborn = Just babyId }
                    |> Expect.equal (Just ( True, True ))
        , test "before Save, the Enrol newborn task is offered and not done" <|
            \_ ->
                enrolNewbornTask pregnancy
                    |> Expect.equal (Just ( True, False ))
        ]


all : Test
all =
    describe "Pages.Person"
        [ fetchForCreateOrEditTest
        , newbornEnrolmentFetchTest
        , otherOriginsTest
        , nextStepsAfterSaveTest
        ]
