module TestFixtures exposing
    ( emptyAcuteIllnessMeasurements
    , emptyTuberculosisMeasurements
    , emptyWellChildMeasurements
    , expectDoseInHistory
    , expectSaveEnabled
    , expectYesAnswerSends
    , labsEntry
    , testChild
    , testParticipant
    , testPerson
    , testSyncManagerModel
    , urineGlucoseValue
    , vaccinationQuestionPrevious
    , vaccinationQuestionToday
    , vitalsValueWith
    , wrapMeasurement
    )

{-| Fixtures shared by the unit-test modules (the `*/Test.elm` files).
-}

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType)
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , Gender(..)
        , GlucoseValue
        , LaboratoryTest
        , LabsResultsValue
        , Measurement
        , TestExecutionNote(..)
        , TuberculosisMeasurements
        , UrineDipstickTestValue
        , VitalsValue
        , WellChildMeasurements
        )
import Backend.Person.Model exposing (Person)
import Date
import EverySet
import Expect exposing (Expectation)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (Html, div)
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..), SyncInfoStatus(..))
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, containing, tag, text)
import Time


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


{-| `testPerson` named as a child, born on the given date. The birth date is
an argument because every test needs a specific age.
-}
testChild : NominalDate -> Person
testChild birthDate =
    { testPerson | name = "Test Child", secondName = "Child", birthDate = Just birthDate }


{-| An `IndividualEncounterParticipant` of the given encounter type, started
on the given date. Everything else is defaulted/empty.
-}
testParticipant : NominalDate -> IndividualEncounterType -> IndividualEncounterParticipant
testParticipant startDate encounterType =
    { person = toEntityUuid "dummy-person"
    , encounterType = encounterType
    , startDate = startDate
    , endDate = Nothing
    , eddDate = Nothing
    , dateConcluded = Nothing
    , outcome = Nothing
    , deliveryLocation = Nothing
    , newborn = Nothing
    , deleted = False
    , shard = Nothing
    }


{-| Wrap a measurement `value` into the full `Measurement` record shape that
the per-encounter measurement collections require, paired with a dummy entity
id and the given `dateMeasured`.

The signature is polymorphic in the id tag, encounter type, and value, so it
unifies with each concrete measurements field type.

-}
wrapMeasurement : NominalDate -> value -> Maybe ( EntityUuid id, Measurement encounter value )
wrapMeasurement dateMeasured value =
    Just
        ( toEntityUuid "dummy-id"
        , { dateMeasured = dateMeasured
          , nurse = Nothing
          , healthCenter = Nothing
          , participantId = toEntityUuid "dummy-person"
          , deleted = False
          , encounterId = Nothing
          , value = value
          }
        )


{-| Vitals with the given systolic/diastolic blood pressure. Respiratory rate
is left unset so the prenatal anemia-complication path, which keys off an
elevated respiratory rate, stays inert. Everything else is unset too.
-}
vitalsValueWith : Float -> Float -> VitalsValue
vitalsValueWith sys dia =
    { sys = Just sys
    , dia = Just dia
    , heartRate = Nothing
    , respiratoryRate = Nothing
    , bodyTemperature = Nothing
    , sysRepeated = Nothing
    , diaRepeated = Nothing
    }


{-| A urine dipstick test value with the given glucose reading; all other
fields are defaulted.
-}
urineGlucoseValue : GlucoseValue -> UrineDipstickTestValue
urineGlucoseValue glucose =
    { testVariant = Nothing
    , executionNote = TestNoteRunToday
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , protein = Nothing
    , ph = Nothing
    , glucose = Just glucose
    , leukocytes = Nothing
    , nitrite = Nothing
    , urobilinogen = Nothing
    , haemoglobin = Nothing
    , ketone = Nothing
    , bilirubin = Nothing
    }


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


emptyTuberculosisMeasurements : TuberculosisMeasurements
emptyTuberculosisMeasurements =
    { diagnostics = Nothing
    , dot = Nothing
    , followUp = Nothing
    , healthEducation = Nothing
    , medication = Nothing
    , referral = Nothing
    , symptomReview = Nothing
    , treatmentReview = Nothing
    }


emptyWellChildMeasurements : WellChildMeasurements
emptyWellChildMeasurements =
    { pregnancySummary = Nothing
    , symptomsReview = Nothing
    , vitals = Nothing
    , height = Nothing
    , muac = Nothing
    , nutrition = Nothing
    , photo = Nothing
    , weight = Nothing
    , contributingFactors = Nothing
    , healthEducation = Nothing
    , followUp = Nothing
    , sendToHC = Nothing
    , headCircumference = Nothing
    , ecd = Nothing
    , albendazole = Nothing
    , mebendezole = Nothing
    , vitaminA = Nothing
    , nextVisit = Nothing
    , bcgImmunisation = Nothing
    , dtpImmunisation = Nothing
    , dtpStandaloneImmunisation = Nothing
    , hpvImmunisation = Nothing
    , ipvImmunisation = Nothing
    , mrImmunisation = Nothing
    , opvImmunisation = Nothing
    , pcv13Immunisation = Nothing
    , rotarixImmunisation = Nothing
    , ncda = Nothing
    , feeding = Nothing
    , hygiene = Nothing
    , foodSecurity = Nothing
    , caring = Nothing
    }


{-| A sync manager model with no health centers and nothing synced.
-}
testSyncManagerModel : SyncManager.Model.Model
testSyncManagerModel =
    SyncManager.Model.emptyModel
        { syncInfoGeneral =
            { lastFetchedRevisionId = 0
            , lastSuccesfulContact = 0
            , remainingToUpload = 0
            , remainingToDownload = 0
            , deviceName = ""
            , status = NotAvailable
            , rollbarToken = ""
            , site = SiteUnknown
            , features = EverySet.empty
            }
        , syncInfoAuthorities = Nothing
        , batchSize = 100
        , syncSpeed =
            { idle = 3000
            , cycle = 50
            , offline = 10000
            }
        }


{-| A labs entry with the given performed and completed tests, open for 30
days from the given date.
-}
labsEntry : NominalDate -> List LaboratoryTest -> List LaboratoryTest -> LabsResultsValue
labsEntry date performed completed =
    { performedTests = EverySet.fromList performed
    , completedTests = EverySet.fromList completed
    , resolutionDate = Date.add Date.Days 30 date
    , patientNotified = False
    , reviewState = Nothing
    , testsWithFollowUp = Nothing
    }


{-| Part of the English text of the immunisation form's "administered
previously?" question. Well Child and Prenatal word it alike:
"Did the child / patient receive any <vaccine> immunizations prior to today
that are not recorded above?"
-}
vaccinationQuestionPrevious : String
vaccinationQuestionPrevious =
    "immunizations prior to today"


{-| Part of the English text of the immunisation form's "will receive
today?" question: "Will the child / patient receive the <vaccine>
immunization today?"
-}
vaccinationQuestionToday : String
vaccinationQuestionToday =
    "immunization today"


{-| The immunisation form's dose history holds an entry for the given dose
number (1 for the first dose), dated on the given day.
-}
expectDoseInHistory : Int -> NominalDate -> List (Html msg) -> Expectation
expectDoseInHistory doseNumber date inputs =
    Query.fromHtml (div [] inputs)
        |> Query.find [ class "history" ]
        |> Query.findAll
            [ class "history-entry"
            , containing [ class "dose", text (String.fromInt doseNumber) ]
            , containing [ class "date", text (formatDDMMYYYY date) ]
            ]
        |> Query.count (Expect.equal 1)


{-| The hosts enable Save once every active task of the form is completed.
The triple is what `vaccinationFormDynamicContentAndTasks` returns:
inputs, tasks completed, tasks active.
-}
expectSaveEnabled : ( List (Html msg), Int, Int ) -> Expectation
expectSaveEnabled ( _, completed, active ) =
    Expect.equal active completed


{-| Tapping "Yes" (the first of the two answers) on the n-th yes/no question
of the form (0 for the first) sends the given message. The message carries the dose the question is
about.
-}
expectYesAnswerSends : Int -> msg -> List (Html msg) -> Expectation
expectYesAnswerSends questionIndex msg inputs =
    Query.fromHtml (div [] inputs)
        |> Query.findAll [ class "yes-no" ]
        |> Query.index questionIndex
        |> Query.findAll [ tag "label" ]
        |> Query.first
        |> Event.simulate Event.click
        |> Event.expect msg
