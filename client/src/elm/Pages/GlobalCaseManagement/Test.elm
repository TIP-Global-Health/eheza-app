module Pages.GlobalCaseManagement.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PersonId)
import Backend.HIVEncounter.Model exposing (HIVEncounter)
import Backend.IndividualEncounterParticipant.Model as IndividualEncounterParticipant exposing (emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (resolveOpenParticipant)
import Backend.Measurement.Model exposing (FollowUpMeasurements, FollowUpOption(..), FollowUpValue, Gender(..), LaboratoryTest(..), PrenatalAssesment(..))
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.TuberculosisEncounter.Model exposing (TuberculosisEncounter)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..), emptyWellChildEncounter)
import Date
import EverySet
import Expect
import Pages.GlobalCaseManagement.Model exposing (AcuteIllnessFollowUpItem, EncounterStartedToday(..), ImmunizationFollowUpItem)
import Pages.GlobalCaseManagement.Utils exposing (allLabsCompleted, fillPersonName, filterFollowUpsOfChasedChildren, filterFollowUpsOfResidents, filterResolvedFollowUps, generateHIVFollowUps, generateHIVParticipants, generateTuberculosisEncounters, generateTuberculosisFollowUps, generateTuberculosisParticipants, resolveEncounterStartedToday)
import Pages.GlobalCaseManagement.View exposing (generateImmunizationFollowUpEntries)
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson)
import Time


all : Test
all =
    describe "Pages.GlobalCaseManagement"
        [ allLabsCompletedTest
        , filterFollowUpsOfChasedChildrenTests
        , filterFollowUpsOfResidentsTests
        , filterResolvedFollowUpsTests
        , generateImmunizationFollowUpEntriesTests
        , generateTuberculosisFollowUpsTests
        , resolveEncounterStartedTodayTests
        , resolveOpenParticipantTests
        , tuberculosisSuspectEntryTests
        ]


allLabsCompletedTest : Test
allLabsCompletedTest =
    describe "allLabsCompleted"
        [ test "a completed test that is no longer performed does not stand in for a pending one" <|
            \_ ->
                allLabsCompleted (EverySet.fromList [ TestHIV, TestHemoglobin ]) (EverySet.fromList [ TestVitalsRecheck, TestHIV ])
                    |> Expect.equal False
        , test "every performed test completed" <|
            \_ ->
                allLabsCompleted (EverySet.fromList [ TestHIV ]) (EverySet.fromList [ TestHIV, TestVitalsRecheck ])
                    |> Expect.equal True
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


{-| The Immunization pane chases a child only while the schedule has a dose
ahead: under two for the infant doses, or a girl of twelve where HPV is given.
An older child, or one without a birth date, could not have the Well Child
visit that would close the entry.
-}
filterFollowUpsOfChasedChildrenTests : Test
filterFollowUpsOfChasedChildrenTests =
    let
        personId =
            toEntityUuid "person"

        currentDate =
            Date.fromCalendarDate 2026 Time.Jun 20

        -- Whether a follow up of a person with the given birth date and
        -- gender is kept on the given site.
        kept site birthDate gender =
            { emptyModelIndexedDb
                | people = Dict.singleton personId (RemoteData.Success { testPerson | birthDate = birthDate, gender = gender })
            }
                |> keptIn site

        keptIn site db =
            filterFollowUpsOfChasedChildren currentDate site db (Dict.singleton personId ())
                |> Dict.isEmpty
                |> not

        -- A child of the given age in years, a few months past the birthday.
        bornYearsAgo years =
            Just <| Date.fromCalendarDate (2026 - years) Time.Jan 1
    in
    describe "filterFollowUpsOfChasedChildren"
        [ test "keeps a child under two" <|
            \_ ->
                kept SiteRwanda (bornYearsAgo 1) Male
                    |> Expect.equal True
        , test "drops a child of two, who has only infant doses ahead" <|
            \_ ->
                kept SiteRwanda (bornYearsAgo 2) Male
                    |> Expect.equal False
        , test "drops a child on the second birthday" <|
            \_ ->
                kept SiteRwanda (Just <| Date.fromCalendarDate 2024 Time.Jun 20) Male
                    |> Expect.equal False
        , test "drops a child without a birth date, who cannot have a Well Child visit" <|
            \_ ->
                kept SiteRwanda Nothing Male
                    |> Expect.equal False
        , test "keeps a girl of twelve, who is due HPV" <|
            \_ ->
                kept SiteRwanda (bornYearsAgo 12) Female
                    |> Expect.equal True
        , test "drops a boy of twelve" <|
            \_ ->
                kept SiteRwanda (bornYearsAgo 12) Male
                    |> Expect.equal False
        , test "drops a girl of thirteen, who can no longer have a Well Child visit" <|
            \_ ->
                kept SiteRwanda (bornYearsAgo 13) Female
                    |> Expect.equal False
        , test "drops a girl of twelve on Burundi, where HPV is not given" <|
            \_ ->
                kept SiteBurundi (bornYearsAgo 12) Female
                    |> Expect.equal False
        , test "keeps a child whose record has not loaded yet" <|
            \_ ->
                keptIn SiteRwanda emptyModelIndexedDb
                    |> Expect.equal True
        ]


{-| A community health worker's Case Management shows the follow ups of the
selected village's residents only. This holds for every pane the health worker
has, the Tuberculosis and HIV panes included: a patient of another village of
the same health centre is not listed, also when that patient's record and
encounter are already on the device (after a search, for example).

The scenario: village residents are "resident" and "second-resident";
"outsider" lives in another village of the same health centre. All of them
have follow ups in the health centre's follow up measurements.

-}
filterFollowUpsOfResidentsTests : Test
filterFollowUpsOfResidentsTests =
    let
        resident =
            toEntityUuid "resident"

        secondResident =
            toEntityUuid "second-resident"

        outsider =
            toEntityUuid "outsider"

        residents =
            [ resident, secondResident ]

        date =
            Date.fromCalendarDate 2026 Time.Jun 10

        limitDate =
            Date.fromCalendarDate 2026 Time.Jun 15

        measurement personId encounterId value =
            { dateMeasured = date
            , nurse = Nothing
            , healthCenter = Nothing
            , participantId = personId
            , deleted = False
            , encounterId = encounterId
            , value = value
            }

        genericValue =
            FollowUpValue (EverySet.singleton OneMonth) Nothing

        nutritionValue =
            { options = EverySet.singleton OneMonth, resolutionDate = Nothing, assesment = EverySet.empty }

        acuteIllnessValue =
            { options = EverySet.singleton OneMonth, resolutionDate = Nothing, diagnosis = Nothing }

        prenatalValue =
            { options = EverySet.singleton OneMonth, resolutionDate = Nothing, assesment = AssesmentNormalPregnancy }

        nextVisitValue =
            { immunisationDate = Just date, asapImmunisationDate = Nothing, pediatricVisitDate = Nothing, resolutionDate = Nothing }

        -- One follow up per given patient. The measurement, the encounter
        -- and the participant are named after the patient.
        followUpsOf value patients =
            List.map
                (\patient ->
                    ( toEntityUuid <| "follow-up-" ++ patient
                    , measurement (toEntityUuid patient) (Just <| toEntityUuid <| "encounter-" ++ patient) value
                    )
                )
                patients
                |> Dict.fromList

        everyone =
            [ "resident", "second-resident", "outsider" ]

        tuberculosisOf patients =
            { noFollowUps | tuberculosis = followUpsOf genericValue patients }

        hivOf patients =
            { noFollowUps | hiv = followUpsOf genericValue patients }

        -- Every pane holds follow ups of both residents and of the outsider.
        allPanes =
            { noFollowUps
                | nutritionGroup = followUpsOf nutritionValue everyone
                , nutritionIndividual = followUpsOf nutritionValue everyone
                , acuteIllness = followUpsOf acuteIllnessValue everyone
                , prenatal = followUpsOf prenatalValue everyone
                , wellChild = followUpsOf nutritionValue everyone
                , tuberculosis = followUpsOf genericValue everyone
                , hiv = followUpsOf genericValue everyone
                , nextVisit = followUpsOf nextVisitValue everyone
            }

        -- The device has everything loaded: the three people, and the
        -- Tuberculosis and HIV encounters their follow ups were recorded at.
        loadedDb =
            { emptyModelIndexedDb
                | people =
                    List.map
                        (\patient -> ( toEntityUuid patient, RemoteData.Success { testPerson | name = patient } ))
                        everyone
                        |> Dict.fromList
                , tuberculosisEncounters =
                    List.map
                        (\patient ->
                            ( toEntityUuid <| "encounter-" ++ patient
                            , RemoteData.Success <|
                                TuberculosisEncounter (toEntityUuid <| "participant-" ++ patient) date Nothing False Nothing
                            )
                        )
                        everyone
                        |> Dict.fromList
                , hivEncounters =
                    List.map
                        (\patient ->
                            ( toEntityUuid <| "encounter-" ++ patient
                            , RemoteData.Success <|
                                HIVEncounter (toEntityUuid <| "participant-" ++ patient) date Nothing False Nothing
                            )
                        )
                        everyone
                        |> Dict.fromList
            }

        -- The names the health worker reads in the Tuberculosis pane, built
        -- the way the page builds it from the narrowed follow ups.
        tuberculosisPane db followUps =
            generateTuberculosisFollowUps limitDate db (filterFollowUpsOfResidents residents followUps) Dict.empty
                |> Tuple.first
                |> fillPersonName Tuple.second db
                |> Dict.values
                |> List.map .personName
                |> List.sort

        -- Same, for the HIV pane.
        hivPane db followUps =
            generateHIVFollowUps limitDate db (filterFollowUpsOfResidents residents followUps)
                |> fillPersonName Tuple.second db
                |> Dict.values
                |> List.map .personName
                |> List.sort

        filtered =
            filterFollowUpsOfResidents residents allPanes
    in
    describe "filterFollowUpsOfResidents"
        [ describe "Tuberculosis"
            [ test "A community health worker's Case Management lists TB follow-ups only for residents of the selected village" <|
                \_ ->
                    filterFollowUpsOfResidents residents (tuberculosisOf everyone)
                        |> .tuberculosis
                        |> patientsOf
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "A TB follow-up of a patient from another village is not listed, even when that patient is already loaded on the device" <|
                \_ ->
                    tuberculosisPane loadedDb (tuberculosisOf [ "outsider" ])
                        |> Expect.equal []
            , test "A resident's TB follow-up is still listed" <|
                \_ ->
                    tuberculosisPane loadedDb (tuberculosisOf [ "resident" ])
                        |> Expect.equal [ "resident" ]
            , test "the TB pane of a device with everyone loaded names the residents and not the outsider" <|
                \_ ->
                    tuberculosisPane loadedDb (tuberculosisOf everyone)
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "a TB follow-up of a patient from another village is dropped when it is the only one" <|
                \_ ->
                    filterFollowUpsOfResidents residents (tuberculosisOf [ "outsider" ])
                        |> .tuberculosis
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "no TB follow-up is kept for a village without residents" <|
                \_ ->
                    filterFollowUpsOfResidents [] (tuberculosisOf everyone)
                        |> .tuberculosis
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "every TB follow-up of a resident is kept when the resident has several" <|
                \_ ->
                    let
                        followUps =
                            { noFollowUps
                                | tuberculosis =
                                    Dict.fromList
                                        [ ( toEntityUuid "first", measurement resident (Just <| toEntityUuid "encounter-a") genericValue )
                                        , ( toEntityUuid "second", measurement resident (Just <| toEntityUuid "encounter-b") genericValue )
                                        , ( toEntityUuid "third", measurement outsider (Just <| toEntityUuid "encounter-c") genericValue )
                                        ]
                            }
                    in
                    filterFollowUpsOfResidents residents followUps
                        |> .tuberculosis
                        |> patientsOf
                        |> Expect.equal [ "resident", "resident" ]
            , test "a resident's TB follow-up recorded without an encounter is kept, and the outsider's is dropped" <|
                \_ ->
                    let
                        followUps =
                            { noFollowUps
                                | tuberculosis =
                                    Dict.fromList
                                        [ ( toEntityUuid "first", measurement resident Nothing genericValue )
                                        , ( toEntityUuid "second", measurement outsider Nothing genericValue )
                                        ]
                            }
                    in
                    filterFollowUpsOfResidents residents followUps
                        |> .tuberculosis
                        |> patientsOf
                        |> Expect.equal [ "resident" ]
            , test "the encounters and participants to load for the TB pane are those of residents only" <|
                \_ ->
                    let
                        encounters =
                            filterFollowUpsOfResidents residents (tuberculosisOf everyone)
                                |> generateTuberculosisEncounters
                    in
                    ( EverySet.toList encounters |> List.map fromEntityUuid |> List.sort
                    , generateTuberculosisParticipants encounters loadedDb
                        |> EverySet.toList
                        |> List.map fromEntityUuid
                        |> List.sort
                    )
                        |> Expect.equal
                            ( [ "encounter-resident", "encounter-second-resident" ]
                            , [ "participant-resident", "participant-second-resident" ]
                            )
            ]
        , describe "HIV"
            [ test "A community health worker's Case Management lists HIV follow-ups only for residents of the selected village" <|
                \_ ->
                    filterFollowUpsOfResidents residents (hivOf everyone)
                        |> .hiv
                        |> patientsOf
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "An HIV follow-up of a patient from another village is not listed, even when that patient is already loaded on the device" <|
                \_ ->
                    hivPane loadedDb (hivOf [ "outsider" ])
                        |> Expect.equal []
            , test "A resident's HIV follow-up is still listed" <|
                \_ ->
                    hivPane loadedDb (hivOf [ "resident" ])
                        |> Expect.equal [ "resident" ]
            , test "the HIV pane of a device with everyone loaded names the residents and not the outsider" <|
                \_ ->
                    hivPane loadedDb (hivOf everyone)
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "an HIV follow-up of a patient from another village is dropped when it is the only one" <|
                \_ ->
                    filterFollowUpsOfResidents residents (hivOf [ "outsider" ])
                        |> .hiv
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "no HIV follow-up is kept for a village without residents" <|
                \_ ->
                    filterFollowUpsOfResidents [] (hivOf everyone)
                        |> .hiv
                        |> Dict.isEmpty
                        |> Expect.equal True

            -- A follow up that asks for a first HIV encounter is stored
            -- against a placeholder encounter named "dummy", and is listed
            -- without an encounter being loaded. It is the shape that would
            -- show an outsider even on a device that never loaded them.
            , test "an HIV follow-up asking for a first encounter is listed for a resident and not for a patient from another village" <|
                \_ ->
                    let
                        followUps =
                            { noFollowUps
                                | hiv =
                                    Dict.fromList
                                        [ ( toEntityUuid "first", measurement resident (Just <| toEntityUuid "dummy") genericValue )
                                        , ( toEntityUuid "second", measurement outsider (Just <| toEntityUuid "dummy") genericValue )
                                        ]
                            }
                    in
                    hivPane loadedDb followUps
                        |> Expect.equal [ "resident" ]
            , test "the participants to load for the HIV pane are those of residents only" <|
                \_ ->
                    let
                        encounters =
                            filterFollowUpsOfResidents residents (hivOf everyone)
                                |> .hiv
                                |> Dict.values
                                |> List.filterMap .encounterId
                                |> EverySet.fromList
                    in
                    generateHIVParticipants encounters loadedDb
                        |> EverySet.toList
                        |> List.map fromEntityUuid
                        |> List.sort
                        |> Expect.equal [ "participant-resident", "participant-second-resident" ]
            ]
        , describe "The Acute Illness, Antenatal, Nutrition and Immunization panes keep listing residents only, as today"
            [ test "Acute Illness" <|
                \_ ->
                    patientsOf filtered.acuteIllness
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "Antenatal" <|
                \_ ->
                    patientsOf filtered.prenatal
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "Nutrition, from group sessions" <|
                \_ ->
                    patientsOf filtered.nutritionGroup
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "Nutrition, from Nutrition encounters" <|
                \_ ->
                    patientsOf filtered.nutritionIndividual
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "Nutrition, from Well Child encounters" <|
                \_ ->
                    patientsOf filtered.wellChild
                        |> Expect.equal [ "resident", "second-resident" ]
            , test "Immunization" <|
                \_ ->
                    patientsOf filtered.nextVisit
                        |> Expect.equal [ "resident", "second-resident" ]
            ]
        , test "all eight panes together hold residents only" <|
            \_ ->
                [ patientsOf filtered.acuteIllness
                , patientsOf filtered.hiv
                , patientsOf filtered.nextVisit
                , patientsOf filtered.nutritionGroup
                , patientsOf filtered.nutritionIndividual
                , patientsOf filtered.prenatal
                , patientsOf filtered.tuberculosis
                , patientsOf filtered.wellChild
                ]
                    |> List.concat
                    |> List.member "outsider"
                    |> Expect.equal False
        , test "nothing is listed when the health centre has no follow ups" <|
            \_ ->
                filterFollowUpsOfResidents residents noFollowUps
                    |> Expect.equal noFollowUps
        , test "follow ups of residents only come through unchanged" <|
            \_ ->
                let
                    followUps =
                        { noFollowUps
                            | acuteIllness = followUpsOf acuteIllnessValue [ "resident" ]
                            , hiv = followUpsOf genericValue [ "resident", "second-resident" ]
                            , tuberculosis = followUpsOf genericValue [ "second-resident" ]
                        }
                in
                filterFollowUpsOfResidents residents followUps
                    |> Expect.equal followUps
        ]


{-| The patients the given follow ups are of, one entry per follow up, sorted.
-}
patientsOf : Dict id { a | participantId : PersonId } -> List String
patientsOf =
    Dict.values >> List.map (.participantId >> fromEntityUuid) >> List.sort


noFollowUps : FollowUpMeasurements
noFollowUps =
    { nutritionGroup = Dict.empty
    , nutritionIndividual = Dict.empty
    , acuteIllness = Dict.empty
    , prenatal = Dict.empty
    , wellChild = Dict.empty
    , tuberculosis = Dict.empty
    , hiv = Dict.empty
    , traceContacts = Dict.empty
    , prenatalLabs = Dict.empty
    , ncdLabs = Dict.empty
    , nextVisit = Dict.empty
    }


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


{-| A follow up from an Acute Illness Tuberculosis suspicion starts a
Tuberculosis encounter. Once one started on or after the follow up date, the
entry is answered and must not be offered again.
-}
tuberculosisSuspectEntryTests : Test
tuberculosisSuspectEntryTests =
    let
        limitDate =
            Date.fromCalendarDate 2026 Time.Jun 15

        followUpDate =
            Date.fromCalendarDate 2026 Time.Jun 10

        personId =
            toEntityUuid "person"

        participantId =
            toEntityUuid "tuberculosis-participant"

        followUps =
            { nutritionGroup = Dict.empty
            , nutritionIndividual = Dict.empty
            , acuteIllness = Dict.empty
            , prenatal = Dict.empty
            , wellChild = Dict.empty
            , tuberculosis = Dict.empty
            , hiv = Dict.empty
            , traceContacts = Dict.empty
            , prenatalLabs = Dict.empty
            , ncdLabs = Dict.empty
            , nextVisit = Dict.empty
            }

        acuteIllnessFollowUp =
            Dict.singleton ( toEntityUuid "acute-illness-participant", personId )
                (AcuteIllnessFollowUpItem followUpDate
                    "Patient"
                    (Just <| toEntityUuid "acute-illness-encounter")
                    1
                    { options = EverySet.singleton OneMonth
                    , resolutionDate = Nothing
                    , diagnosis = Nothing
                    }
                )

        dbWithEncounterAt startDate =
            { emptyModelIndexedDb
                | individualParticipantsByPerson =
                    Dict.singleton personId
                        (RemoteData.Success <|
                            Dict.singleton participantId
                                (emptyIndividualEncounterParticipant (Date.fromCalendarDate 2026 Time.Jan 1)
                                    personId
                                    IndividualEncounterParticipant.TuberculosisEncounter
                                    (toEntityUuid "health-center")
                                )
                        )
                , tuberculosisEncountersByParticipant =
                    Dict.singleton participantId
                        (RemoteData.Success <|
                            Dict.singleton (toEntityUuid "tuberculosis-encounter")
                                (TuberculosisEncounter participantId startDate Nothing False Nothing)
                        )
            }

        entriesFor db =
            generateTuberculosisFollowUps limitDate db followUps acuteIllnessFollowUp
                |> Tuple.second
                |> Dict.keys
    in
    describe "Acute Illness Tuberculosis suspect entry"
        [ test "is offered when patient has no Tuberculosis participant" <|
            \_ ->
                entriesFor emptyModelIndexedDb
                    |> Expect.equal [ personId ]
        , test "is offered when the last Tuberculosis encounter predates the follow up" <|
            \_ ->
                entriesFor (dbWithEncounterAt (Date.fromCalendarDate 2026 Time.Jun 9))
                    |> Expect.equal [ personId ]
        , test "is dropped once a Tuberculosis encounter started on the follow up date" <|
            \_ ->
                entriesFor (dbWithEncounterAt followUpDate)
                    |> Expect.equal []
        , test "is dropped once a Tuberculosis encounter started after the follow up date" <|
            \_ ->
                entriesFor (dbWithEncounterAt limitDate)
                    |> Expect.equal []
        ]


{-| Starting a Tuberculosis encounter from Case Management reuses the open
participant, the one the Tuberculosis participant page opens.
-}
resolveOpenParticipantTests : Test
resolveOpenParticipantTests =
    let
        participant encounterType endDate =
            { encounterType = encounterType, endDate = endDate }

        ended =
            Just (Date.fromCalendarDate 2026 Time.Jun 1)
    in
    describe "resolveOpenParticipant"
        [ test "takes the open participant of given type" <|
            \_ ->
                Dict.fromList
                    [ ( "ended", participant IndividualEncounterParticipant.TuberculosisEncounter ended )
                    , ( "other-type", participant IndividualEncounterParticipant.HIVEncounter Nothing )
                    , ( "open", participant IndividualEncounterParticipant.TuberculosisEncounter Nothing )
                    ]
                    |> resolveOpenParticipant IndividualEncounterParticipant.TuberculosisEncounter
                    |> Expect.equal (Just "open")
        , test "finds none when every participant of given type has ended" <|
            \_ ->
                Dict.fromList
                    [ ( "ended", participant IndividualEncounterParticipant.TuberculosisEncounter ended )
                    , ( "other-type", participant IndividualEncounterParticipant.HIVEncounter Nothing )
                    ]
                    |> resolveOpenParticipant IndividualEncounterParticipant.TuberculosisEncounter
                    |> Expect.equal Nothing
        ]
