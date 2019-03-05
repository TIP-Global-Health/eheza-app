module Backend.Decoder exposing (decodeRevision)

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)
import Backend.Measurement.Decoder exposing (decodeAttendance, decodeCounselingSession, decodeFamilyPlanning, decodeHeight, decodeMuac, decodeNutrition, decodeParticipantConsent, decodePhoto, decodeWeight)
import Backend.Model exposing (..)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Session.Decoder exposing (decodeSession)
import Json.Decode exposing (..)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid)


decodeRevision : Decoder Revision
decodeRevision =
    field "type" string
        |> andThen
            (\s ->
                -- Some of these aren't implemented yet, because they need
                -- to be converted from ID to UUUID references first.
                case s of
                    "attendance" ->
                        decodeWithUuid AttendanceRevision decodeAttendance

                    "catchment_area" ->
                        decodeWithUuid CatchmentAreaRevision decodeCatchmentArea

                    "child" ->
                        decodeWithUuid ChildRevision decodeChild

                    "clinic" ->
                        decodeWithUuid ClinicRevision decodeClinic

                    "counseling_schedule" ->
                        decodeWithUuid CounselingScheduleRevision decodeCounselingSchedule

                    "counseling_session" ->
                        decodeWithUuid CounselingSessionRevision decodeCounselingSession

                    "counseling_topic" ->
                        decodeWithUuid CounselingTopicRevision decodeCounselingTopic

                    "health_center" ->
                        decodeWithUuid HealthCenterRevision decodeHealthCenter

                    "mother" ->
                        decodeWithUuid MotherRevision decodeMother

                    "session" ->
                        decodeWithUuid SessionRevision decodeSession

                    "nurse" ->
                        decodeWithUuid NurseRevision decodeNurse

                    "family_planning" ->
                        decodeWithUuid FamilyPlanningRevision decodeFamilyPlanning

                    "height" ->
                        decodeWithUuid HeightRevision decodeHeight

                    "muac" ->
                        decodeWithUuid MuacRevision decodeMuac

                    "nutrition" ->
                        decodeWithUuid ChildNutritionRevision decodeNutrition

                    "participant_consent" ->
                        decodeWithUuid ParticipantConsentRevision decodeParticipantConsent

                    "participant_form" ->
                        decodeWithUuid ParticipantFormRevision decodeParticipantForm

                    "photo" ->
                        decodeWithUuid PhotoRevision decodePhoto

                    "weight" ->
                        decodeWithUuid WeightRevision decodeWeight

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized type"
            )


decodeWithUuid : (EntityUuid a -> b -> Revision) -> Decoder b -> Decoder Revision
decodeWithUuid tag =
    map2 tag (field "uuid" decodeEntityUuid)
