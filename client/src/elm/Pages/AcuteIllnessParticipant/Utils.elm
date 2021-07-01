module Pages.AcuteIllnessParticipant.Utils exposing (isAcuteIllnessActive)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)


{-| Illness is considered 'active' if it does not have it's end
date or outcome set.
-}
isAcuteIllnessActive : NominalDate -> IndividualEncounterParticipant -> Bool
isAcuteIllnessActive currentDate session =
    isNothing session.endDate || isNothing session.outcome
