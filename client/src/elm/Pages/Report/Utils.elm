module Pages.Report.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Date
import Gizra.NominalDate exposing (NominalDate)
import Pages.Report.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)


hivResultNormal : TestReport -> Bool
hivResultNormal =
    testReportNormal


hivPCRResultNormal : HIVPCRResult -> Bool
hivPCRResultNormal =
    (==) ResultSuppressedViralLoad


syphilisResultNormal : TestResult -> Bool
syphilisResultNormal =
    testResultNormal


hepatitisBResultNormal : TestReport -> Bool
hepatitisBResultNormal =
    testReportNormal


malariaResultNormal : TestResult -> Bool
malariaResultNormal =
    testResultNormal


proteinResultNormal : ProteinValue -> Bool
proteinResultNormal =
    (==) Protein0


phResultNormal : PHValue -> Bool
phResultNormal value =
    not <| List.member value [ Ph40, Ph85 ]


glucoseResultNormal : GlucoseValue -> Bool
glucoseResultNormal =
    (==) Glucose0


leukocytesResultNormal : LeukocytesValue -> Bool
leukocytesResultNormal =
    (==) LeukocytesNegative


nitriteResultNormal : NitriteValue -> Bool
nitriteResultNormal =
    (==) NitriteNegative


urobilinogenResultNormal : UrobilinogenValue -> Bool
urobilinogenResultNormal value =
    List.member value [ Urobilinogen002, Urobilinogen10 ]


urineHaemoglobinValueResultNormal : HaemoglobinValue -> Bool
urineHaemoglobinValueResultNormal =
    (==) HaemoglobinNegative


ketoneResultNormal : KetoneValue -> Bool
ketoneResultNormal =
    (==) KetoneNegative


bilirubinResultNormal : BilirubinValue -> Bool
bilirubinResultNormal =
    (==) BilirubinNegative


randomBloodSugarResultNormal : Float -> Bool
randomBloodSugarResultNormal value =
    value >= 74 && value <= 110


hemoglobinResultNormal : Float -> Bool
hemoglobinResultNormal value =
    value >= 11 && value <= 16.5


rhesusResultsNormal : Rhesus -> Bool
rhesusResultsNormal =
    (==) RhesusPositive


creatinineResultNormal : Float -> Bool
creatinineResultNormal value =
    value >= 0.5 && value <= 1.3


bunResultNormal : Float -> Bool
bunResultNormal value =
    value >= 6 && value <= 24


altResultNormal : Float -> Bool
altResultNormal value =
    value >= 7 && value <= 56


astResultNormal : Float -> Bool
astResultNormal value =
    value >= 8 && value <= 33


pregnancyResultNormal : TestReport -> Bool
pregnancyResultNormal =
    testReportNormal


testReportNormal : TestReport -> Bool
testReportNormal report =
    case report of
        TestPerformed result ->
            testResultNormal result

        TestNotPerformedKnownAsPositive ->
            True


testResultNormal : TestResult -> Bool
testResultNormal =
    (==) TestNegative


getAcuteIllnessEncountersForParticipant :
    ModelIndexedDb
    -> IndividualEncounterParticipantId
    -> List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
getAcuteIllnessEncountersForParticipant db participantId =
    Dict.get participantId db.acuteIllnessEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
        |> List.sortWith (\( _, e1 ) ( _, e2 ) -> compareAcuteIllnessEncountersDesc e1 e2)


getAcuteIllnessDiagnosisForEncounters : List ( AcuteIllnessEncounterId, AcuteIllnessEncounter ) -> Maybe ( NominalDate, AcuteIllnessDiagnosis )
getAcuteIllnessDiagnosisForEncounters encounters =
    List.filterMap
        (\( _, encounter ) ->
            if encounter.diagnosis /= NoAcuteIllnessDiagnosis then
                Just ( encounter.startDate, encounter.diagnosis )

            else
                Nothing
        )
        encounters
        -- We know that encounters are sorted DESC, so the one at
        -- head is the most recent.
        |> List.head


compareAcuteIllnessEncountersDesc :
    { a | startDate : NominalDate, sequenceNumber : Int }
    -> { a | startDate : NominalDate, sequenceNumber : Int }
    -> Order
compareAcuteIllnessEncountersDesc data1 data2 =
    compareAcuteIllnessEncounters data2 data1


compareAcuteIllnessEncounters :
    { a | startDate : NominalDate, sequenceNumber : Int }
    -> { a | startDate : NominalDate, sequenceNumber : Int }
    -> Order
compareAcuteIllnessEncounters data1 data2 =
    case Date.compare data1.startDate data2.startDate of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare data1.sequenceNumber data2.sequenceNumber


diagnosisEntryStatusToString : PaneEntryStatus -> String
diagnosisEntryStatusToString status =
    case status of
        StatusOngoing ->
            "ongoing"

        StatusResolved ->
            "resolved"
