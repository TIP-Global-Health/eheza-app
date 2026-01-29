module Pages.Report.Utils exposing (altResultNormal, astResultNormal, bilirubinResultNormal, bloodSmearResultNormal, bunResultNormal, compareAcuteIllnessEncounters, compareAcuteIllnessEncountersDesc, creatinineResultNormal, diagnosisEntryStatusToString, getAcuteIllnessDiagnosisForEncounters, getAcuteIllnessEncountersForParticipant, getRandomBloodSugarResultValue, glucoseResultNormal, hba1cResultNormal, hdlCholesterolResultNormal, hemoglobinResultNormal, hepatitisBResultNormal, hivPCRResultNormal, hivResultNormal, ketoneResultNormal, ldlCholesterolResultNormal, leukocytesResultNormal, malariaResultNormal, nitriteResultNormal, partnerHIVResultNormal, phResultNormal, pregnancyResultNormal, proteinResultNormal, randomBloodSugarResultFromValue, randomBloodSugarResultNormal, rhesusResultsNormal, syphilisResultNormal, testReportNormal, testResultNormal, totalCholesterolResultNormal, triglyceridesResultNormal, urineHaemoglobinValueResultNormal, urobilinogenResultNormal)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Utils exposing (bloodSmearResultNotSet, testPerformedByExecutionNote)
import Pages.Report.Model exposing (PaneEntryStatus(..), RandomBloodSugarResult(..), TestReport(..))


hivResultNormal : TestReport -> Bool
hivResultNormal =
    testReportNormal


hivPCRResultNormal : HIVPCRResult -> Bool
hivPCRResultNormal =
    (==) ResultSuppressedViralLoad


partnerHIVResultNormal : TestResult -> Bool
partnerHIVResultNormal =
    testResultNormal


syphilisResultNormal : TestResult -> Bool
syphilisResultNormal =
    testResultNormal


hepatitisBResultNormal : TestReport -> Bool
hepatitisBResultNormal =
    testReportNormal


malariaResultNormal : TestResult -> Bool
malariaResultNormal =
    testResultNormal


bloodSmearResultNormal : BloodSmearResult -> Bool
bloodSmearResultNormal value =
    bloodSmearResultNotSet value
        || (value == BloodSmearNegative)


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


randomBloodSugarResultNormal : RandomBloodSugarResult -> Bool
randomBloodSugarResultNormal result =
    case result of
        TestRunBeforeMeal value ->
            value >= 74 && value <= 126

        TestRunAfterMeal value ->
            value >= 74 && value < 200


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


hba1cResultNormal : Float -> Bool
hba1cResultNormal value =
    value < 6


totalCholesterolResultNormal : Float -> Bool
totalCholesterolResultNormal value =
    value < 200


ldlCholesterolResultNormal : Float -> Bool
ldlCholesterolResultNormal value =
    value >= 130 && value <= 160


hdlCholesterolResultNormal : Float -> Bool
hdlCholesterolResultNormal value =
    value >= 40 && value <= 60


triglyceridesResultNormal : Float -> Bool
triglyceridesResultNormal value =
    value >= 54 && value <= 150


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
    Backend.NutritionEncounter.Utils.getAcuteIllnessEncountersForParticipant db participantId
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


getRandomBloodSugarResultValue : RandomBloodSugarResult -> Float
getRandomBloodSugarResultValue result =
    case result of
        TestRunBeforeMeal value ->
            value

        TestRunAfterMeal value ->
            value


randomBloodSugarResultFromValue : RandomBloodSugarTestValue encounterId -> Maybe ( NominalDate, Maybe RandomBloodSugarResult )
randomBloodSugarResultFromValue value =
    if testPerformedByExecutionNote value.executionNote then
        Maybe.map2
            (\executionDate testPrerequisites ->
                let
                    result =
                        if EverySet.member PrerequisiteFastFor12h testPrerequisites then
                            Maybe.map TestRunBeforeMeal value.sugarCount

                        else
                            Maybe.map TestRunAfterMeal value.sugarCount
                in
                ( executionDate, result )
            )
            value.executionDate
            value.testPrerequisites

    else
        Nothing
