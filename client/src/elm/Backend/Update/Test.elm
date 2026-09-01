module Backend.Update.Test exposing (all)

import App.Model
import Backend.AcuteIllnessEncounter.Model exposing (emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessDangerSign(..)
        , AcuteIllnessMeasurements
        , RapidTestResult(..)
        )
import Backend.Model
import Backend.Update exposing (generateSuspectedDiagnosisMsgsSubsequentEncounter)
import Date
import EverySet
import Expect
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (emptyAcuteIllnessMeasurements, testParticipant, testPerson, vitalsValueWith, wrapMeasurement)
import Time


all : Test
all =
    describe "Backend.Update"
        [ subsequentEncounterDiagnosisTests ]


{-| The diagnosis stored on a subsequent Acute Illness encounter follows the
measurements: it is set once they produce one, replaced when a corrected
measurement produces another, and cleared when they no longer support any.

The fixtures satisfy the mandatory-activities gate for an adult seen by a CHW:
danger signs and vitals recorded, no medication prescribed before (so Ongoing
Treatment is not expected), and the malaria RDT taken.

-}
subsequentEncounterDiagnosisTests : Test
subsequentEncounterDiagnosisTests =
    let
        currentDate =
            Date.fromCalendarDate 2026 Time.Jul 27

        measurementsWith rdtResult dangerSigns =
            { emptyAcuteIllnessMeasurements
                | dangerSigns = wrapMeasurement currentDate dangerSigns
                , vitals = wrapMeasurement currentDate (vitalsValueWith 120 80)
                , malariaTesting = wrapMeasurement currentDate rdtResult
            }

        assembledWith : AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> AssembledData
        assembledWith storedDiagnosis measurements =
            let
                encounter =
                    emptyAcuteIllnessEncounter (toEntityUuid "participant") currentDate 2 AcuteIllnessEncounterCHW Nothing
            in
            { id = toEntityUuid "encounter"
            , encounter = { encounter | diagnosis = storedDiagnosis }
            , participant = testParticipant currentDate Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            , person = testPerson
            , measurements = measurements
            , previousEncountersData = []
            , previousFirstInitialWithSubsequent = []
            , previousSecondInitialWithSubsequent = []
            , initialEncounter = False
            , diagnosis = Nothing
            }

        diagnosisWrites : AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> List AcuteIllnessDiagnosis
        diagnosisWrites storedDiagnosis measurements =
            generateSuspectedDiagnosisMsgsSubsequentEncounter currentDate EverySet.empty True (assembledWith storedDiagnosis measurements)
                |> List.filterMap
                    (\msg ->
                        case msg of
                            App.Model.MsgIndexedDb (Backend.Model.MsgAcuteIllnessEncounter _ (Backend.AcuteIllnessEncounter.Model.SetAcuteIllnessDiagnosis diagnosis)) ->
                                Just diagnosis

                            _ ->
                                Nothing
                    )

        noDangerSigns =
            EverySet.singleton NoAcuteIllnessDangerSign
    in
    describe "generateSuspectedDiagnosisMsgsSubsequentEncounter"
        [ test "a diagnosis is written once the measurements produce one" <|
            \_ ->
                diagnosisWrites NoAcuteIllnessDiagnosis (measurementsWith RapidTestPositive noDangerSigns)
                    |> Expect.equal [ DiagnosisMalariaUncomplicated ]
        , test "a corrected danger-sign set replaces the stored diagnosis" <|
            \_ ->
                diagnosisWrites DiagnosisMalariaComplicated (measurementsWith RapidTestPositive noDangerSigns)
                    |> Expect.equal [ DiagnosisMalariaUncomplicated ]
        , test "a corrected negative RDT clears the stored diagnosis" <|
            \_ ->
                diagnosisWrites DiagnosisMalariaUncomplicated (measurementsWith RapidTestNegative noDangerSigns)
                    |> Expect.equal [ NoAcuteIllnessDiagnosis ]
        , test "a diagnosis matching the measurements is not rewritten" <|
            \_ ->
                diagnosisWrites DiagnosisMalariaUncomplicated (measurementsWith RapidTestPositive noDangerSigns)
                    |> Expect.equal []
        , test "danger signs beside a positive RDT make the diagnosis complicated" <|
            \_ ->
                diagnosisWrites DiagnosisMalariaUncomplicated (measurementsWith RapidTestPositive (EverySet.singleton DangerSignConvulsions))
                    |> Expect.equal [ DiagnosisMalariaComplicated ]
        ]
