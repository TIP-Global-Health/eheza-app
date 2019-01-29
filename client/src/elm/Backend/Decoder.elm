module Backend.Decoder exposing (decodeRevision)

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)
import Backend.Measurement.Decoder exposing (decodeFamilyPlanning, decodeHeight, decodeMuac, decodeNutrition, decodePhoto, decodeWeight)
import Backend.Model exposing (..)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Nurse.Decoder exposing (decodeNurse)
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
                    "catchment_area" ->
                        decodeWithUuid CatchmentAreaRevision decodeCatchmentArea

                    "child" ->
                        decodeWithUuid ChildRevision decodeChild

                    "clinic" ->
                        decodeWithUuid ClinicRevision decodeClinic

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
