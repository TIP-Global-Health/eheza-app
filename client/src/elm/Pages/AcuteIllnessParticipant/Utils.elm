module Pages.AcuteIllnessParticipant.Utils exposing (isAcuteIllnessActive)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)


{-| Preganancy is considered 'active' if it does not have it's end
date set, and it's EDD date is not set, or, EDD date is less than
3 month overdue.
-}
isAcuteIllnessActive : NominalDate -> IndividualEncounterParticipant -> Bool
isAcuteIllnessActive currentDate session =
    isNothing session.endDate
