module Backend.Decoder exposing (decodeRevision)

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)
import Backend.Measurement.Decoder exposing (decodeAttendance, decodeCounselingSession, decodeFamilyPlanning, decodeFbf, decodeHeight, decodeLactation, decodeMuac, decodeNutrition, decodeParticipantConsent, decodePhoto, decodeWeight)
import Backend.Model exposing (..)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)
import Backend.Relationship.Decoder exposing (decodeRelationship)
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

                    "child_fbf" ->
                        decodeWithUuid ChildFbfRevision decodeFbf

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

                    "mother_fbf" ->
                        decodeWithUuid MotherFbfRevision decodeFbf

                    "person" ->
                        decodeWithUuid PersonRevision decodePerson

                    "relationship" ->
                        decodeWithUuid RelationshipRevision decodeRelationship

                    "pmtct_participant" ->
                        decodeWithUuid PmtctParticipantRevision decodePmtctParticipant

                    "session" ->
                        decodeWithUuid SessionRevision decodeSession

                    "nurse" ->
                        decodeWithUuid NurseRevision decodeNurse

                    "family_planning" ->
                        decodeWithUuid FamilyPlanningRevision decodeFamilyPlanning

                    "lactation" ->
                        decodeWithUuid LactationRevision decodeLactation

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
