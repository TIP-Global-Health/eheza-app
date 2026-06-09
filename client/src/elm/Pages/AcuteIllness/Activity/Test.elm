module Pages.AcuteIllness.Activity.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model as AcuteIllnessEncounterModel
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AcuteFindingsGeneralSign(..)
        , AcuteFindingsRespiratorySign(..)
        , AcuteFindingsValue
        , AcuteIllnessMeasurements
        , CovidTestingValue
        , Gender(..)
        , HeartCPESign
        , LungsCPESign
        , RapidTestResult(..)
        , SymptomsGIDerivedSign
        , SymptomsGISign(..)
        , SymptomsGIValue
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , VitalsValue
        )
import Backend.Person.Model exposing (Person)
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllness.Activity.Utils
    exposing
        ( malariaDangerSignsPresent
        , mildGastrointestinalInfectionSymptomsPresent
        , nonBloodyDiarrheaAtSymptoms
        , resolveAcuteIllnessDiagnosis
        , resolveAcuteIllnessDiagnosisByMalariaRDT
        , respiratoryInfectionDangerSignsPresent
        , respiratoryRateElevatedByAge
        , respiratoryRateElevatedByAgeForCovid19
        , symptomMaxDuration
        )
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (SiteFeature(..))
import Test exposing (Test, describe, test)
import Time



-- FIXTURES


{-| A dummy date, used as `dateMeasured` for all measurement wrappers.
-}
dummyDate : NominalDate
dummyDate =
    Date.fromCalendarDate 2020 Time.Jan 1


{-| Wrap a measurement `value` into the full `Measurement` record shape that
the `AcuteIllnessMeasurements` fields require, paired with a dummy entity id.

Type annotation is intentionally omitted so the polymorphic record (with
`nurse`, `healthCenter`, `participantId`, `encounterId` left as free type
variables via `Nothing` / dummy ids) unifies with each concrete field type.

-}
wrapMeasurement value =
    Just
        ( toEntityUuid "dummy-id"
        , { dateMeasured = dummyDate
          , nurse = Nothing
          , healthCenter = Nothing
          , participantId = toEntityUuid "dummy-person"
          , deleted = False
          , encounterId = Nothing
          , value = value
          }
        )


emptyAcuteIllnessMeasurements : AcuteIllnessMeasurements
emptyAcuteIllnessMeasurements =
    { symptomsGeneral = Nothing
    , symptomsRespiratory = Nothing
    , symptomsGI = Nothing
    , vitals = Nothing
    , acuteFindings = Nothing
    , malariaTesting = Nothing
    , travelHistory = Nothing
    , exposure = Nothing
    , isolation = Nothing
    , hcContact = Nothing
    , call114 = Nothing
    , treatmentReview = Nothing
    , sendToHC = Nothing
    , medicationDistribution = Nothing
    , muac = Nothing
    , treatmentOngoing = Nothing
    , dangerSigns = Nothing
    , nutrition = Nothing
    , healthEducation = Nothing
    , followUp = Nothing
    , coreExam = Nothing
    , covidTesting = Nothing
    , contactsTracing = Nothing
    }



-- VALUE BUILDERS


symptomsGeneralWith : SymptomsGeneralSign -> Dict SymptomsGeneralSign Int
symptomsGeneralWith sign =
    Dict.fromList [ ( sign, 1 ) ]


symptomsGIValueWith : SymptomsGISign -> EverySet SymptomsGIDerivedSign -> SymptomsGIValue
symptomsGIValueWith sign derivedSigns =
    { signs = Dict.fromList [ ( sign, 1 ) ]
    , derivedSigns = derivedSigns
    }


symptomsRespiratoryWith : SymptomsRespiratorySign -> Dict SymptomsRespiratorySign Int
symptomsRespiratoryWith sign =
    Dict.fromList [ ( sign, 1 ) ]


acuteFindingsValueWith : EverySet AcuteFindingsGeneralSign -> EverySet AcuteFindingsRespiratorySign -> AcuteFindingsValue
acuteFindingsValueWith signsGeneral signsRespiratory =
    { signsGeneral = signsGeneral
    , signsRespiratory = signsRespiratory
    }



-- MEASUREMENT SETTERS


withSymptomsGeneral : SymptomsGeneralSign -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withSymptomsGeneral sign measurements =
    { measurements | symptomsGeneral = wrapMeasurement (symptomsGeneralWith sign) }


withSymptomsGI : SymptomsGISign -> EverySet SymptomsGIDerivedSign -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withSymptomsGI sign derivedSigns measurements =
    { measurements | symptomsGI = wrapMeasurement (symptomsGIValueWith sign derivedSigns) }


withSymptomsRespiratory : SymptomsRespiratorySign -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withSymptomsRespiratory sign measurements =
    { measurements | symptomsRespiratory = wrapMeasurement (symptomsRespiratoryWith sign) }


withAcuteFindings : EverySet AcuteFindingsGeneralSign -> EverySet AcuteFindingsRespiratorySign -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withAcuteFindings signsGeneral signsRespiratory measurements =
    { measurements | acuteFindings = wrapMeasurement (acuteFindingsValueWith signsGeneral signsRespiratory) }


withMalariaTesting : RapidTestResult -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withMalariaTesting result measurements =
    { measurements | malariaTesting = wrapMeasurement result }



-- ASSEMBLED DATA FIXTURES (for end-to-end resolveAcuteIllnessDiagnosis)


{-| The reference "current date" used for all end-to-end diagnosis tests.
With `testPerson` born 1985-01-01, age is ~425 months (>= 60), so MUAC and
Nutrition physical-exam tasks are NOT expected for this (adult) person.
-}
currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| An adult person. Everything except birthDate/gender is defaulted/empty.
-}
testPerson : Person
testPerson =
    { name = "Test Person"
    , firstName = "Test"
    , secondName = "Person"
    , nationalIdNumber = Nothing
    , hmisNumber = Nothing
    , avatarUrl = Nothing
    , birthDate = Just (Date.fromCalendarDate 1985 Time.Jan 1)
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


dummyEncounter : AcuteIllnessEncounterModel.AcuteIllnessEncounter
dummyEncounter =
    { participant = toEntityUuid "dummy-participant"
    , startDate = currentDate
    , endDate = Nothing
    , sequenceNumber = 1
    , encounterType = AcuteIllnessEncounterCHW
    , diagnosis = NoAcuteIllnessDiagnosis
    , deleted = False
    , shard = Nothing
    }


dummyParticipant : IndividualEncounterParticipant
dummyParticipant =
    { person = toEntityUuid "dummy-person"
    , encounterType = AcuteIllnessEncounter
    , startDate = currentDate
    , endDate = Nothing
    , eddDate = Nothing
    , dateConcluded = Nothing
    , outcome = Nothing
    , deliveryLocation = Nothing
    , newborn = Nothing
    , deleted = False
    , shard = Nothing
    }


{-| Build an `AssembledData` wrapping `testPerson` and the given measurements.
The `Bool` sets `initialEncounter`. All 8 fields not read by
`resolveAcuteIllnessDiagnosis` are filled with dummies.
-}
testAssembled : Bool -> AcuteIllnessMeasurements -> AssembledData
testAssembled initialEncounter measurements =
    { id = toEntityUuid "dummy-encounter"
    , encounter = dummyEncounter
    , participant = dummyParticipant
    , person = testPerson
    , measurements = measurements
    , previousEncountersData = []
    , firstInitialWithSubsequent = []
    , secondInitialWithSubsequent = []
    , initialEncounter = initialEncounter
    , diagnosis = Nothing
    }



-- ADDITIONAL VALUE BUILDERS / SETTERS


emptyVitalsValue : VitalsValue
emptyVitalsValue =
    { sys = Nothing
    , dia = Nothing
    , heartRate = Nothing
    , respiratoryRate = Nothing
    , bodyTemperature = Nothing
    , sysRepeated = Nothing
    , diaRepeated = Nothing
    }


{-| Set vitals to a value built from the given (Maybe) bodyTemperature,
respiratoryRate, sys and dia, leaving the rest as Nothing.
-}
withVitals : Maybe Float -> Maybe Int -> Maybe Float -> Maybe Float -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withVitals bodyTemperature respiratoryRate sys dia measurements =
    { measurements
        | vitals =
            wrapMeasurement
                { emptyVitalsValue
                    | bodyTemperature = bodyTemperature
                    , respiratoryRate = respiratoryRate
                    , sys = sys
                    , dia = dia
                }
    }


{-| Set symptomsRespiratory to a dict mapping the given sign to the given
duration value (e.g. `symptomMaxDuration` to denote "more than 2 weeks").
-}
withSymptomsRespiratoryDuration : SymptomsRespiratorySign -> Int -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withSymptomsRespiratoryDuration sign duration measurements =
    { measurements | symptomsRespiratory = wrapMeasurement (Dict.fromList [ ( sign, duration ) ]) }


{-| Set symptomsGeneral to a dict mapping the given two signs to value 1 each.
-}
withSymptomsGeneralPair : SymptomsGeneralSign -> SymptomsGeneralSign -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withSymptomsGeneralPair sign1 sign2 measurements =
    { measurements | symptomsGeneral = wrapMeasurement (Dict.fromList [ ( sign1, 1 ), ( sign2, 1 ) ]) }


withCoreExam : EverySet HeartCPESign -> EverySet LungsCPESign -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withCoreExam heart lungs measurements =
    { measurements | coreExam = wrapMeasurement { heart = heart, lungs = lungs } }


withCovidTesting : RapidTestResult -> AcuteIllnessMeasurements -> AcuteIllnessMeasurements
withCovidTesting result measurements =
    { measurements | covidTesting = wrapMeasurement (CovidTestingValue result Nothing) }



-- MANDATORY-ACTIVITIES GATE BASES


{-| Base measurements satisfying `mandatoryActivitiesCompletedFirstEncounter`
for a CHW: symptoms (general/respiratory/GI) present-but-empty, plus empty
vitals and empty acuteFindings.
-}
gateBaseChw : AcuteIllnessMeasurements
gateBaseChw =
    emptyAcuteIllnessMeasurements
        |> withSymptomsGeneral NoSymptomsGeneral
        |> withSymptomsRespiratory NoSymptomsRespiratory
        |> withSymptomsGI NoSymptomsGI EverySet.empty
        |> withVitals Nothing Nothing Nothing Nothing
        |> withAcuteFindings EverySet.empty EverySet.empty


{-| Like `gateBaseChw`, but also adds an empty coreExam, as required for a
nurse (`isChw = False`).
-}
gateBaseNurse : AcuteIllnessMeasurements
gateBaseNurse =
    gateBaseChw
        |> withCoreExam EverySet.empty EverySet.empty



-- TESTS


respiratoryRateElevatedByAgeTest : Test
respiratoryRateElevatedByAgeTest =
    describe "respiratoryRateElevatedByAge"
        [ test "2-12mo, rate 50 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 6) 50
                    |> Expect.equal True
        , test "2-12mo, rate 49 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 6) 49
                    |> Expect.equal False
        , test "just under 12mo, rate 50 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 11) 50
                    |> Expect.equal True
        , test "exactly 12mo, rate 45 -> elevated (code uses <60 branch, threshold 40; sheet ambiguous)" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 12) 45
                    |> Expect.equal True
        , test "exactly 12mo, rate 39 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 12) 39
                    |> Expect.equal False
        , test "24mo, rate 40 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 24) 40
                    |> Expect.equal True
        , test "24mo, rate 39 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 24) 39
                    |> Expect.equal False
        , test "just under 5y, rate 40 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 59) 40
                    |> Expect.equal True
        , test ">5y, rate 31 -> elevated (code flags >30; CHW sheet says no action - sheet stale)" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 72) 31
                    |> Expect.equal True
        , test ">5y, rate 30 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAge (Just 72) 30
                    |> Expect.equal False
        , test "unknown age -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAge Nothing 100
                    |> Expect.equal False
        ]


respiratoryRateElevatedByAgeForCovid19Test : Test
respiratoryRateElevatedByAgeForCovid19Test =
    describe "respiratoryRateElevatedByAgeForCovid19"
        [ test "<2mo, rate 60 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 1) 60
                    |> Expect.equal True
        , test "<2mo, rate 59 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 1) 59
                    |> Expect.equal False
        , test "2-12mo, rate 50 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 6) 50
                    |> Expect.equal True
        , test "2-12mo, rate 49 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 6) 49
                    |> Expect.equal False
        , test "24mo, rate 40 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 24) 40
                    |> Expect.equal True
        , test ">5y, rate 31 -> elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 72) 31
                    |> Expect.equal True
        , test ">5y, rate 30 -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 (Just 72) 30
                    |> Expect.equal False
        , test "unknown age -> not elevated" <|
            \_ ->
                respiratoryRateElevatedByAgeForCovid19 Nothing 100
                    |> Expect.equal False
        ]


malariaDangerSignsPresentTest : Test
malariaDangerSignsPresentTest =
    describe "malariaDangerSignsPresent"
        [ test "empty measurements -> False" <|
            \_ ->
                malariaDangerSignsPresent emptyAcuteIllnessMeasurements
                    |> Expect.equal False
        , test "symptomsGI Vomiting -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsGeneral NoSymptomsGeneral
                    |> withSymptomsGI Vomiting EverySet.empty
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> malariaDangerSignsPresent
                    |> Expect.equal True
        , test "symptomsGeneral Convulsions -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsGeneral SymptomsGeneralConvulsions
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> malariaDangerSignsPresent
                    |> Expect.equal True
        , test "acuteFindings signsGeneral Jaundice -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsGeneral NoSymptomsGeneral
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings (EverySet.singleton Jaundice) EverySet.empty
                    |> malariaDangerSignsPresent
                    |> Expect.equal True
        , test "acuteFindings signsRespiratory Stridor -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsGeneral NoSymptomsGeneral
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty (EverySet.singleton Stridor)
                    |> malariaDangerSignsPresent
                    |> Expect.equal True
        ]


respiratoryInfectionDangerSignsPresentTest : Test
respiratoryInfectionDangerSignsPresentTest =
    describe "respiratoryInfectionDangerSignsPresent"
        [ test "empty measurements -> False" <|
            \_ ->
                respiratoryInfectionDangerSignsPresent emptyAcuteIllnessMeasurements
                    |> Expect.equal False
        , test "symptomsRespiratory ShortnessOfBreath -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsRespiratory ShortnessOfBreath
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> respiratoryInfectionDangerSignsPresent
                    |> Expect.equal True
        , test "symptomsRespiratory StabbingChestPain -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsRespiratory StabbingChestPain
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> respiratoryInfectionDangerSignsPresent
                    |> Expect.equal True
        , test "acuteFindings signsRespiratory Stridor -> True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsRespiratory NoSymptomsRespiratory
                    |> withAcuteFindings EverySet.empty (EverySet.singleton Stridor)
                    |> respiratoryInfectionDangerSignsPresent
                    |> Expect.equal True
        ]


resolveAcuteIllnessDiagnosisByMalariaRDTTest : Test
resolveAcuteIllnessDiagnosisByMalariaRDTTest =
    describe "resolveAcuteIllnessDiagnosisByMalariaRDT"
        [ test "positive RDT, no danger signs -> MalariaUncomplicated" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withMalariaTesting RapidTestPositive
                    |> withSymptomsGeneral NoSymptomsGeneral
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> resolveAcuteIllnessDiagnosisByMalariaRDT
                    |> Expect.equal (Just DiagnosisMalariaUncomplicated)
        , test "positive RDT + malaria danger sign (Convulsions) -> MalariaComplicated" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withMalariaTesting RapidTestPositive
                    |> withSymptomsGeneral SymptomsGeneralConvulsions
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> resolveAcuteIllnessDiagnosisByMalariaRDT
                    |> Expect.equal (Just DiagnosisMalariaComplicated)
        , test "positive-and-pregnant RDT, no danger signs -> MalariaUncomplicatedAndPregnant" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withMalariaTesting RapidTestPositiveAndPregnant
                    |> withSymptomsGeneral NoSymptomsGeneral
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> resolveAcuteIllnessDiagnosisByMalariaRDT
                    |> Expect.equal (Just DiagnosisMalariaUncomplicatedAndPregnant)
        , test "negative RDT, no other signs -> FeverOfUnknownOrigin" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withMalariaTesting RapidTestNegative
                    |> withSymptomsRespiratory NoSymptomsRespiratory
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty EverySet.empty
                    |> resolveAcuteIllnessDiagnosisByMalariaRDT
                    |> Expect.equal (Just DiagnosisFeverOfUnknownOrigin)
        , test "negative RDT + respiratory danger sign (Stridor) -> RespiratoryInfectionComplicated" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withMalariaTesting RapidTestNegative
                    |> withSymptomsRespiratory NoSymptomsRespiratory
                    |> withSymptomsGI NoSymptomsGI EverySet.empty
                    |> withAcuteFindings EverySet.empty (EverySet.singleton Stridor)
                    |> resolveAcuteIllnessDiagnosisByMalariaRDT
                    |> Expect.equal (Just DiagnosisRespiratoryInfectionComplicated)
        , test "no malaria testing -> Nothing" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> resolveAcuteIllnessDiagnosisByMalariaRDT
                    |> Expect.equal Nothing
        ]


gastrointestinalSymptomsTest : Test
gastrointestinalSymptomsTest =
    describe "mild GI / non-bloody diarrhea symptoms"
        [ test "empty -> mildGI False" <|
            \_ ->
                mildGastrointestinalInfectionSymptomsPresent emptyAcuteIllnessMeasurements
                    |> Expect.equal False
        , test "empty -> nonBloodyDiarrhea False" <|
            \_ ->
                nonBloodyDiarrheaAtSymptoms emptyAcuteIllnessMeasurements
                    |> Expect.equal False
        , test "symptomsGI Nausea -> mildGI True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsGI Nausea EverySet.empty
                    |> mildGastrointestinalInfectionSymptomsPresent
                    |> Expect.equal True
        , test "symptomsGI NonBloodyDiarrhea -> nonBloodyDiarrhea True" <|
            \_ ->
                emptyAcuteIllnessMeasurements
                    |> withSymptomsGI NonBloodyDiarrhea EverySet.empty
                    |> nonBloodyDiarrheaAtSymptoms
                    |> Expect.equal True
        ]


{-| Run `resolveAcuteIllnessDiagnosis` end-to-end on an initial CHW encounter
with no site features.
-}
resolveChw : AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveChw measurements =
    resolveAcuteIllnessDiagnosis currentDate EverySet.empty True (testAssembled True measurements)


resolveAcuteIllnessDiagnosisNonCovidTest : Test
resolveAcuteIllnessDiagnosisNonCovidTest =
    describe "resolveAcuteIllnessDiagnosis - non-COVID (CHW, initial encounter)"
        [ test "fever + positive malaria RDT -> MalariaUncomplicated" <|
            \_ ->
                gateBaseChw
                    |> withVitals (Just 38) Nothing Nothing Nothing
                    |> withMalariaTesting RapidTestPositive
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisMalariaUncomplicated)
        , test "fever + positive malaria RDT + malaria danger sign (Convulsions) -> MalariaComplicated" <|
            \_ ->
                gateBaseChw
                    |> withVitals (Just 38) Nothing Nothing Nothing
                    |> withMalariaTesting RapidTestPositive
                    |> withSymptomsGeneral SymptomsGeneralConvulsions
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisMalariaComplicated)
        , test "fever + positive-and-pregnant malaria RDT -> MalariaUncomplicatedAndPregnant" <|
            \_ ->
                gateBaseChw
                    |> withVitals (Just 38) Nothing Nothing Nothing
                    |> withMalariaTesting RapidTestPositiveAndPregnant
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisMalariaUncomplicatedAndPregnant)
        , test "fever + negative malaria RDT, no other signs -> FeverOfUnknownOrigin" <|
            \_ ->
                gateBaseChw
                    |> withVitals (Just 38) Nothing Nothing Nothing
                    |> withMalariaTesting RapidTestNegative
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisFeverOfUnknownOrigin)
        , test "respiratory ShortnessOfBreath, no fever -> RespiratoryInfectionComplicated" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsRespiratory ShortnessOfBreath
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisRespiratoryInfectionComplicated)
        , test "GI BloodyDiarrhea, no fever -> GastrointestinalInfectionComplicated" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsGI BloodyDiarrhea EverySet.empty
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisGastrointestinalInfectionComplicated)
        , test "respiratory Cough + elevated respiratory rate (35), no fever -> RespiratoryInfectionUncomplicated" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsRespiratory Cough
                    |> withVitals Nothing (Just 35) Nothing Nothing
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisRespiratoryInfectionUncomplicated)
        , test "respiratory Cough + normal respiratory rate (25), no fever -> SimpleColdAndCough" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsRespiratory Cough
                    |> withVitals Nothing (Just 25) Nothing Nothing
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisSimpleColdAndCough)
        , test "GI NonBloodyDiarrhea, no fever, nothing else -> GastrointestinalInfectionUncomplicated" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsGI NonBloodyDiarrhea EverySet.empty
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisGastrointestinalInfectionUncomplicated)
        , test "GI Nausea, no fever, no diarrhea -> UndeterminedMoreEvaluationNeeded" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsGI Nausea EverySet.empty
                    |> resolveChw
                    |> Expect.equal (Just DiagnosisUndeterminedMoreEvaluationNeeded)
        , test "gate satisfied but no fever / no symptoms -> Nothing" <|
            \_ ->
                gateBaseChw
                    |> resolveChw
                    |> Expect.equal Nothing
        ]


resolveAcuteIllnessDiagnosisTuberculosisTest : Test
resolveAcuteIllnessDiagnosisTuberculosisTest =
    describe "resolveAcuteIllnessDiagnosis - tuberculosis (CHW, initial encounter, TB feature enabled)"
        [ test "respiratory Cough for more than 2 weeks -> TuberculosisSuspect" <|
            \_ ->
                gateBaseChw
                    |> withSymptomsRespiratoryDuration Cough symptomMaxDuration
                    |> (\measurements ->
                            resolveAcuteIllnessDiagnosis currentDate
                                (EverySet.singleton FeatureTuberculosisManagement)
                                True
                                (testAssembled True measurements)
                       )
                    |> Expect.equal (Just DiagnosisTuberculosisSuspect)
        ]


resolveAcuteIllnessDiagnosisCovidTest : Test
resolveAcuteIllnessDiagnosisCovidTest =
    let
        resolveNurse measurements =
            resolveAcuteIllnessDiagnosis currentDate EverySet.empty False (testAssembled True measurements)
    in
    describe "resolveAcuteIllnessDiagnosis - COVID (nurse, initial encounter)"
        [ test "negative malaria + fever + low BP (sys 80) + respiratory Cough + positive COVID test -> SevereCovid19" <|
            \_ ->
                gateBaseNurse
                    |> withMalariaTesting RapidTestNegative
                    |> withVitals (Just 38) Nothing (Just 80) (Just 70)
                    |> withSymptomsRespiratory Cough
                    |> withCovidTesting RapidTestPositive
                    |> resolveNurse
                    |> Expect.equal (Just DiagnosisSevereCovid19)
        , test "negative malaria + fever + normal BP (sys 110) + respiratory Cough + positive COVID test -> PneuminialCovid19" <|
            \_ ->
                gateBaseNurse
                    |> withMalariaTesting RapidTestNegative
                    |> withVitals (Just 38) Nothing (Just 110) (Just 70)
                    |> withSymptomsRespiratory Cough
                    |> withCovidTesting RapidTestPositive
                    |> resolveNurse
                    |> Expect.equal (Just DiagnosisPneuminialCovid19)
        , test "negative malaria + fever + normal BP + two general non-danger signs + no respiratory + no crackles + positive COVID test -> LowRiskCovid19" <|
            \_ ->
                gateBaseNurse
                    |> withMalariaTesting RapidTestNegative
                    |> withVitals (Just 38) Nothing (Just 110) (Just 70)
                    |> withSymptomsGeneralPair Chills NightSweats
                    |> withCovidTesting RapidTestPositive
                    |> resolveNurse
                    |> Expect.equal (Just DiagnosisLowRiskCovid19)
        ]


all : Test
all =
    describe "Acute Illness diagnosis tests"
        [ respiratoryRateElevatedByAgeTest
        , respiratoryRateElevatedByAgeForCovid19Test
        , malariaDangerSignsPresentTest
        , respiratoryInfectionDangerSignsPresentTest
        , resolveAcuteIllnessDiagnosisByMalariaRDTTest
        , gastrointestinalSymptomsTest
        , resolveAcuteIllnessDiagnosisNonCovidTest
        , resolveAcuteIllnessDiagnosisTuberculosisTest
        , resolveAcuteIllnessDiagnosisCovidTest
        ]
