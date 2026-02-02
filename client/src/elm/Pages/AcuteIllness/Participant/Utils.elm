module Pages.AcuteIllness.Participant.Utils exposing (isAcuteIllnessActive, noPursueAcuteIllnessDiagnoses)

import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Maybe.Extra exposing (isNothing)


{-| Illness is considered 'active' if it does not have it's end
date or outcome set.
-}
isAcuteIllnessActive : IndividualEncounterParticipant -> Bool
isAcuteIllnessActive session =
    isNothing session.endDate || isNothing session.outcome


{-| Diagnoses that do not require subsequent encounters.
-}
noPursueAcuteIllnessDiagnoses : List AcuteIllnessDiagnosis
noPursueAcuteIllnessDiagnoses =
    [ DiagnosisFeverOfUnknownOrigin, DiagnosisTuberculosisSuspect ]
