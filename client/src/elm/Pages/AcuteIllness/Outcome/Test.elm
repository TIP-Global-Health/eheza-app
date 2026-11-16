module Pages.AcuteIllness.Outcome.Test exposing (all)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model as AcuteIllnessEncounterModel
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , ChildNutritionSign(..)
        , MuacInCm(..)
        , RapidTestResult(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        )
import Backend.Model exposing (ModelIndexedDb, emptyModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Date
import EverySet exposing (EverySet)
import Expect exposing (Expectation)
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllness.Encounter.Utils exposing (getAcuteIllnessDiagnosisForParticipant)
import Pages.AcuteIllness.Outcome.Model exposing (emptyModel)
import Pages.AcuteIllness.Outcome.Utils exposing (generateOutcomeData)
import Pages.AcuteIllness.Outcome.View
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (SiteFeature(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)
import TestFixtures exposing (emptyAcuteIllnessMeasurements, testChild, testParticipant, testPerson)
import Time
import Translate exposing (translate)
import Translate.Model exposing (Language(..))



-- FIXTURES
--
-- An illness is a list of visits. Each visit is an encounter, carrying the
-- diagnosis that was saved for it, and the measurements taken at it. The
-- measurements are deliberately chosen so that working the diagnosis out
-- again from them, today, gives a different answer from the saved one. That
-- way a test tells the saved diagnosis apart from a recomputed one.


today : NominalDate
today =
    Date.fromCalendarDate 2020 Time.Jun 1


daysAgo : Int -> NominalDate
daysAgo days =
    Date.add Date.Days (negate days) today


illnessId : IndividualEncounterParticipantId
illnessId =
    toEntityUuid "illness"


type alias Visit =
    ( String, AcuteIllnessEncounterModel.AcuteIllnessEncounter, AcuteIllnessMeasurements )


{-| A visit that has ended on the day it started.
-}
visit : String -> AcuteIllnessEncounterType -> NominalDate -> Int -> AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> Visit
visit id encounterType startDate sequenceNumber diagnosis measurements =
    ( id
    , { participant = illnessId
      , startDate = startDate
      , endDate = Just startDate
      , sequenceNumber = sequenceNumber
      , encounterType = encounterType
      , diagnosis = diagnosis
      , deleted = False
      , shard = Nothing
      }
    , measurements
    )


chwVisit : String -> NominalDate -> Int -> AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> Visit
chwVisit id =
    visit id AcuteIllnessEncounterCHW


{-| The device database holding the given person and one illness with the
given visits, all of them loaded with their measurements.
-}
illnessDb : Person -> List Visit -> ModelIndexedDb
illnessDb person visits =
    let
        encounters =
            List.map (\( id, encounter, _ ) -> ( toEntityUuid id, encounter )) visits
                |> Dict.fromList

        participant =
            testParticipant (daysAgo 30) AcuteIllnessEncounter
    in
    { emptyModelIndexedDb
        | acuteIllnessEncounters = Dict.map (\_ encounter -> RemoteData.Success encounter) encounters
        , acuteIllnessMeasurements =
            List.map (\( id, _, measurements ) -> ( toEntityUuid id, RemoteData.Success measurements )) visits
                |> Dict.fromList
        , acuteIllnessEncountersByParticipant = Dict.singleton illnessId (RemoteData.Success encounters)
        , individualParticipants = Dict.singleton illnessId (RemoteData.Success participant)
        , people = Dict.singleton participant.person (RemoteData.Success person)
    }


{-| The diagnosis the outcome page shows under the patient's name.
-}
outcomeDiagnosisWith : EverySet SiteFeature -> Bool -> Person -> List Visit -> Maybe (Maybe AcuteIllnessDiagnosis)
outcomeDiagnosisWith features isChw person visits =
    generateOutcomeData today features illnessId isChw (illnessDb person visits)
        |> RemoteData.toMaybe
        |> Maybe.map (.diagnosis >> Maybe.map Tuple.second)


outcomeDiagnosis : Bool -> Person -> List Visit -> Maybe (Maybe AcuteIllnessDiagnosis)
outcomeDiagnosis =
    outcomeDiagnosisWith EverySet.empty


{-| The diagnosis the "Record outcome" button on the participant page shows.
-}
buttonDiagnosis : Person -> List Visit -> Maybe AcuteIllnessDiagnosis
buttonDiagnosis person visits =
    getAcuteIllnessDiagnosisForParticipant (illnessDb person visits) illnessId
        |> Maybe.map Tuple.second


{-| The outcome page, as rendered.
-}
outcomePage : Bool -> Person -> List Visit -> Query.Single Pages.AcuteIllness.Outcome.Model.Msg
outcomePage isChw person visits =
    Pages.AcuteIllness.Outcome.View.view English
        today
        EverySet.empty
        illnessId
        isChw
        (illnessDb person visits)
        emptyModel
        |> Query.fromHtml


expectAlertIcon : Bool -> Query.Single msg -> Expectation
expectAlertIcon shown page =
    page
        |> Query.findAll [ class "alerts" ]
        |> Query.count
            (Expect.equal
                (if shown then
                    1

                 else
                    0
                )
            )


diagnosisLabel : AcuteIllnessDiagnosis -> String
diagnosisLabel diagnosis =
    translate English (Translate.AcuteIllnessDiagnosis diagnosis)



-- MEASUREMENTS


wrap : value -> Maybe ( Restful.Endpoint.EntityUuid id, Backend.Measurement.Model.Measurement encounter value )
wrap =
    TestFixtures.wrapMeasurement today


{-| Every first-visit activity filled in, with a cough and the given
respiratory rate and body temperature. Child-only and nurse-only exams are
filled too, and are simply not read for patients who do not need them.
-}
coughMeasurements : Int -> Float -> AcuteIllnessMeasurements
coughMeasurements respiratoryRate bodyTemperature =
    { emptyAcuteIllnessMeasurements
        | symptomsGeneral = wrap (Dict.fromList [ ( NoSymptomsGeneral, 1 ) ])
        , symptomsRespiratory = wrap (Dict.fromList [ ( Cough, 1 ) ])
        , symptomsGI = wrap { signs = Dict.fromList [ ( NoSymptomsGI, 1 ) ], derivedSigns = EverySet.empty }
        , vitals =
            wrap
                { sys = Nothing
                , dia = Nothing
                , heartRate = Nothing
                , respiratoryRate = Just respiratoryRate
                , bodyTemperature = Just bodyTemperature
                , sysRepeated = Nothing
                , diaRepeated = Nothing
                }
        , acuteFindings = wrap { signsGeneral = EverySet.empty, signsRespiratory = EverySet.empty }
        , muac = wrap (MuacInCm 14)
        , nutrition = wrap (EverySet.singleton NormalChildNutrition)
        , coreExam = wrap { heart = EverySet.empty, lungs = EverySet.empty }
    }


{-| For an adult, a cough with a normal breathing rate and no fever: worked
out again, this reads as Simple Cold and Cough.
-}
coldMeasurements : AcuteIllnessMeasurements
coldMeasurements =
    coughMeasurements 20 36.6


{-| A cough with fever and a negative malaria test. For a nurse, worked out
again, this reads as suspected COVID-19, since no COVID test was taken.
-}
suspectedCovidMeasurements : AcuteIllnessMeasurements
suspectedCovidMeasurements =
    let
        measurements =
            coughMeasurements 20 38.5
    in
    { measurements | malariaTesting = wrap RapidTestNegative }


positiveMalariaTest : AcuteIllnessMeasurements
positiveMalariaTest =
    { emptyAcuteIllnessMeasurements | malariaTesting = wrap RapidTestPositive }



-- TESTS


{-| The outcome page and the "Record outcome" button show the same diagnosis
for the same illness, and it is the saved one, whatever the visits'
measurements would give if worked out again.
-}
sameAsButtonTest : Test
sameAsButtonTest =
    let
        expectSameAsButton isChw features person visits =
            outcomeDiagnosisWith features isChw person visits
                |> Expect.equal (Just (buttonDiagnosis person visits))
    in
    describe "the saved diagnosis, as on the participant page"
        [ test "The outcome page shows the same diagnosis as the \"Record outcome\" button on the participant page: the diagnosis saved at the illness's most recent visit that has one" <|
            \_ ->
                -- The first visit saved Fever of Unknown Origin, while its
                -- measurements, worked out again, read as Simple Cold and
                -- Cough.
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin coldMeasurements ]
                    |> Expect.equal (Just (Just DiagnosisFeverOfUnknownOrigin))
        , test "matches the button for a single CHW visit whose saved diagnosis differs from its measurements" <|
            \_ ->
                expectSameAsButton True
                    EverySet.empty
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin coldMeasurements ]
        , test "matches the button for a nurse whose visit's measurements now read as suspected COVID" <|
            \_ ->
                expectSameAsButton False
                    EverySet.empty
                    testPerson
                    [ visit "nurse" AcuteIllnessEncounterNurse (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin suspectedCovidMeasurements ]
        , test "matches the button on a site with Tuberculosis Management, where a long cough now reads as a Tuberculosis suspect" <|
            \_ ->
                let
                    -- A cough of more than 2 weeks is stored as 14 days.
                    longCough =
                        { coldMeasurements | symptomsRespiratory = wrap (Dict.fromList [ ( Cough, 14 ) ]) }
                in
                expectSameAsButton True
                    (EverySet.singleton FeatureTuberculosisManagement)
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisSimpleColdAndCough longCough ]
        , test "matches the button when the illness has several visits, listed latest first" <|
            \_ ->
                expectSameAsButton True
                    EverySet.empty
                    testPerson
                    [ chwVisit "followUp" (daysAgo 1) 1 DiagnosisMalariaUncomplicated emptyAcuteIllnessMeasurements
                    , chwVisit "first" (daysAgo 3) 1 DiagnosisSimpleColdAndCough coldMeasurements
                    ]
        , test "matches the button when the illness has several visits, listed earliest first" <|
            \_ ->
                expectSameAsButton True
                    EverySet.empty
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisSimpleColdAndCough coldMeasurements
                    , chwVisit "followUp" (daysAgo 1) 1 DiagnosisMalariaUncomplicated emptyAcuteIllnessMeasurements
                    ]
        , test "the most recent visit that has a diagnosis is the one shown, when a later visit saved none" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisSimpleColdAndCough coldMeasurements
                    , chwVisit "second" (daysAgo 2) 1 DiagnosisFeverOfUnknownOrigin emptyAcuteIllnessMeasurements
                    , chwVisit "third" (daysAgo 1) 1 NoAcuteIllnessDiagnosis emptyAcuteIllnessMeasurements
                    ]
                    |> Expect.equal (Just (Just DiagnosisFeverOfUnknownOrigin))
        , test "of two visits on the same day, the one with the higher sequence number is the most recent" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "later" (daysAgo 2) 2 DiagnosisFeverOfUnknownOrigin emptyAcuteIllnessMeasurements
                    , chwVisit "earlier" (daysAgo 2) 1 DiagnosisSimpleColdAndCough coldMeasurements
                    ]
                    |> Expect.equal (Just (Just DiagnosisFeverOfUnknownOrigin))
        , test "the outcome page names the saved diagnosis under the patient's name" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin coldMeasurements ]
                    |> Query.has [ text (diagnosisLabel DiagnosisFeverOfUnknownOrigin) ]
        ]


{-| A child's age decides the breathing-rate limit and which exams are
needed. The saved diagnosis was made at the child's age on the visit day,
and the outcome page shows it, not what today's age would give.
-}
ageLimitTest : Test
ageLimitTest =
    let
        -- 11 months old on the visit day, a month ago; 12 months old today.
        -- A breathing rate of 45 is normal under 12 months (Simple Cold and
        -- Cough), and raised from 12 months (Uncomplicated Respiratory
        -- Infection).
        turnedOneYear =
            testChild (Date.fromCalendarDate 2019 Time.May 20)

        -- 5 months old on the visit day; 6 months old today. MUAC is only
        -- taken from 6 months, so it was not taken at the visit; worked out
        -- again today, the visit lacks a needed exam and gives no diagnosis.
        turnedSixMonths =
            testChild (Date.fromCalendarDate 2019 Time.Nov 25)

        withoutMuac =
            let
                measurements =
                    coughMeasurements 45 36.6
            in
            { measurements | muac = Nothing }
    in
    describe "age limits"
        [ test "A child who crossed an age limit since the first visit (e.g. 12 months) still sees the saved diagnosis" <|
            \_ ->
                outcomeDiagnosis True
                    turnedOneYear
                    [ chwVisit "first" (Date.fromCalendarDate 2020 Time.May 1) 1 DiagnosisSimpleColdAndCough (coughMeasurements 45 36.6) ]
                    |> Expect.equal (Just (Just DiagnosisSimpleColdAndCough))
        , test "a child who turned 6 months since the visit, when MUAC became needed, still sees the saved diagnosis" <|
            \_ ->
                outcomeDiagnosis True
                    turnedSixMonths
                    [ chwVisit "first" (Date.fromCalendarDate 2020 Time.May 20) 1 DiagnosisSimpleColdAndCough withoutMuac ]
                    |> Expect.equal (Just (Just DiagnosisSimpleColdAndCough))
        , test "a child who crossed 12 months sees the same diagnosis as the \"Record outcome\" button" <|
            \_ ->
                let
                    visits =
                        [ chwVisit "first" (Date.fromCalendarDate 2020 Time.May 1) 1 DiagnosisSimpleColdAndCough (coughMeasurements 45 36.6) ]
                in
                outcomeDiagnosis True turnedOneYear visits
                    |> Expect.equal (Just (buttonDiagnosis turnedOneYear visits))
        ]


{-| A follow-up visit can change the diagnosis (to malaria, after a positive
test). The outcome page shows the changed diagnosis.
-}
followUpChangedDiagnosisTest : Test
followUpChangedDiagnosisTest =
    let
        first =
            chwVisit "first" (daysAgo 3) 1 DiagnosisSimpleColdAndCough coldMeasurements

        followUp =
            chwVisit "followUp" (daysAgo 1) 1 DiagnosisMalariaUncomplicated positiveMalariaTest
    in
    describe "follow-up visits"
        [ test "An illness whose follow-up visit changed the diagnosis shows the follow-up's diagnosis" <|
            \_ ->
                outcomeDiagnosis True testPerson [ first, followUp ]
                    |> Expect.equal (Just (Just DiagnosisMalariaUncomplicated))
        , test "the follow-up's diagnosis is shown whatever order the visits are stored in" <|
            \_ ->
                outcomeDiagnosis True testPerson [ followUp, first ]
                    |> Expect.equal (Just (Just DiagnosisMalariaUncomplicated))
        , test "the follow-up's diagnosis is still shown after a later follow-up that saved no diagnosis" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ first
                    , followUp
                    , chwVisit "secondFollowUp" today 1 NoAcuteIllnessDiagnosis emptyAcuteIllnessMeasurements
                    ]
                    |> Expect.equal (Just (Just DiagnosisMalariaUncomplicated))
        , test "a follow-up visit that saved no diagnosis keeps the first visit's" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin coldMeasurements
                    , chwVisit "followUp" (daysAgo 1) 1 NoAcuteIllnessDiagnosis emptyAcuteIllnessMeasurements
                    ]
                    |> Expect.equal (Just (Just DiagnosisFeverOfUnknownOrigin))
        , test "when a nurse took the illness over and saved a new diagnosis, the nurse's diagnosis is shown" <|
            \_ ->
                outcomeDiagnosis False
                    testPerson
                    [ first
                    , visit "nurse" AcuteIllnessEncounterNurse (daysAgo 1) 1 DiagnosisFeverOfUnknownOrigin emptyAcuteIllnessMeasurements
                    ]
                    |> Expect.equal (Just (Just DiagnosisFeverOfUnknownOrigin))
        , test "a nurse's subsequent visit that changed the diagnosis is shown" <|
            \_ ->
                outcomeDiagnosis False
                    testPerson
                    [ visit "nurse" AcuteIllnessEncounterNurse (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin suspectedCovidMeasurements
                    , visit "nurseSubsequent" AcuteIllnessEncounterNurseSubsequent (daysAgo 1) 1 DiagnosisMalariaUncomplicated positiveMalariaTest
                    ]
                    |> Expect.equal (Just (Just DiagnosisMalariaUncomplicated))
        ]


{-| `NoAcuteIllnessDiagnosis` is how a visit stores "no diagnosis". When no
visit of the illness saved one, the outcome page shows none, even if the
measurements would give one if worked out again.
-}
noSavedDiagnosisTest : Test
noSavedDiagnosisTest =
    describe "no saved diagnosis"
        [ test "An illness with no saved diagnosis shows none" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 NoAcuteIllnessDiagnosis coldMeasurements ]
                    |> Expect.equal (Just Nothing)
        , test "an illness whose single visit saved no diagnosis and took no measurements shows none" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 NoAcuteIllnessDiagnosis emptyAcuteIllnessMeasurements ]
                    |> Expect.equal (Just Nothing)
        , test "an illness with several visits, none of which saved a diagnosis, shows none" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 NoAcuteIllnessDiagnosis coldMeasurements
                    , chwVisit "followUp" (daysAgo 1) 1 NoAcuteIllnessDiagnosis positiveMalariaTest
                    ]
                    |> Expect.equal (Just Nothing)
        , test "for a nurse, an illness with no saved diagnosis shows none, even with measurements that read as suspected COVID" <|
            \_ ->
                outcomeDiagnosis False
                    testPerson
                    [ visit "nurse" AcuteIllnessEncounterNurse (daysAgo 3) 1 NoAcuteIllnessDiagnosis suspectedCovidMeasurements ]
                    |> Expect.equal (Just Nothing)
        , test "the outcome page names no diagnosis when none was saved" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 NoAcuteIllnessDiagnosis coldMeasurements ]
                    |> Query.hasNot [ text (diagnosisLabel DiagnosisSimpleColdAndCough) ]
        ]


{-| The red alert icon beside the patient's name is shown to a CHW when the
illness's saved diagnosis is suspected COVID-19. A CHW can no longer
diagnose COVID, so working it out again never gives suspected COVID for a
CHW; the saved diagnosis is the only way it can appear.
-}
covidAlertTest : Test
covidAlertTest =
    describe "the suspected-COVID alert icon"
        [ test "The suspected-COVID alert icon appears only when that saved diagnosis is suspected COVID" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisCovid19Suspect suspectedCovidMeasurements ]
                    |> expectAlertIcon True
        , test "the alert icon appears for a CHW when a nurse's visit saved suspected COVID" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisSimpleColdAndCough coldMeasurements
                    , visit "nurse" AcuteIllnessEncounterNurse (daysAgo 1) 1 DiagnosisCovid19Suspect suspectedCovidMeasurements
                    ]
                    |> expectAlertIcon True
        , test "no alert icon when the saved diagnosis is not suspected COVID" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisFeverOfUnknownOrigin suspectedCovidMeasurements ]
                    |> expectAlertIcon False
        , test "no alert icon when a follow-up changed a suspected COVID diagnosis to another one" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisCovid19Suspect suspectedCovidMeasurements
                    , chwVisit "followUp" (daysAgo 1) 1 DiagnosisMalariaUncomplicated positiveMalariaTest
                    ]
                    |> expectAlertIcon False
        , test "no alert icon when the saved diagnosis is a confirmed COVID diagnosis rather than a suspected one" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisSevereCovid19 suspectedCovidMeasurements ]
                    |> expectAlertIcon False
        , test "no alert icon when no diagnosis was saved" <|
            \_ ->
                outcomePage True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 NoAcuteIllnessDiagnosis suspectedCovidMeasurements ]
                    |> expectAlertIcon False
        , test "no alert icon for a nurse, even when the saved diagnosis is suspected COVID" <|
            \_ ->
                outcomePage False
                    testPerson
                    [ visit "nurse" AcuteIllnessEncounterNurse (daysAgo 3) 1 DiagnosisCovid19Suspect suspectedCovidMeasurements ]
                    |> expectAlertIcon False
        , test "the outcome data carries suspected COVID for a CHW when it was saved" <|
            \_ ->
                outcomeDiagnosis True
                    testPerson
                    [ chwVisit "first" (daysAgo 3) 1 DiagnosisCovid19Suspect suspectedCovidMeasurements ]
                    |> Expect.equal (Just (Just DiagnosisCovid19Suspect))
        ]


all : Test
all =
    describe "Acute Illness outcome page"
        [ sameAsButtonTest
        , ageLimitTest
        , followUpChangedDiagnosisTest
        , noSavedDiagnosisTest
        , covidAlertTest
        ]
