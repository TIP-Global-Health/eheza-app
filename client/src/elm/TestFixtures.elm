module TestFixtures exposing
    ( testChild
    , testParticipant
    , testPerson
    , urineGlucoseValue
    , vitalsValueWith
    , wrapMeasurement
    )

{-| Fixtures shared by the unit-test modules (the `*/Test.elm` files).
-}

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType)
import Backend.Measurement.Model
    exposing
        ( Gender(..)
        , GlucoseValue
        , Measurement
        , TestExecutionNote(..)
        , UrineDipstickTestValue
        , VitalsValue
        )
import Backend.Person.Model exposing (Person)
import Date
import Gizra.NominalDate exposing (NominalDate)
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
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
