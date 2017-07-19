module Pages.Patient.Utils exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Date exposing (Date)
import Patient.Model exposing (Patient, PatientId, PatientType(..), PatientsDict)


{-| Model update helper which applies the provided date to the specified activity type field in the
provided patient while ensuring proper Mother / Child and activity mapping type safety
-}
updateActivityDate : Date -> ActivityType -> Patient -> Patient
updateActivityDate date activityType patient =
    case ( activityType, patient.info ) of
        ( Child childActivityType, PatientChild child ) ->
            let
                activityDates =
                    child.activityDates

                updatedActivityDates =
                    case childActivityType of
                        ChildPicture ->
                            { activityDates | childPicture = Just date }

                        Height ->
                            { activityDates | height = Just date }

                        Muac ->
                            { activityDates | muac = Just date }

                        NutritionSigns ->
                            { activityDates | nutritionSigns = Just date }

                        ProgressReport ->
                            { activityDates | progressReport = Just date }

                        Weight ->
                            { activityDates | weight = Just date }
            in
                { info = PatientChild { child | activityDates = updatedActivityDates } }

        ( Mother motherActivityType, PatientMother mother ) ->
            let
                activityDates =
                    mother.activityDates

                updatedActivityDates =
                    case motherActivityType of
                        Aheza ->
                            { activityDates | aheza = Just date }

                        Attendance ->
                            { activityDates | attendance = Just date }

                        Education ->
                            { activityDates | education = Just date }

                        FamilyPlanning ->
                            { activityDates | familyPlanning = Just date }

                        Hiv ->
                            { activityDates | hiv = Just date }

                        MotherPicture ->
                            { activityDates | motherPicture = Just date }
            in
                { info = PatientMother { mother | activityDates = updatedActivityDates } }

        -- This should never be reached as it would imply a Child activity being applied to a Mother or vica versa
        ( _, _ ) ->
            patient
