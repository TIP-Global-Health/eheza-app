module Backend.PatientRecord.Utils exposing (progressReportInitiatorFromUrlFragment, progressReportInitiatorToUrlFragment)

import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


progressReportInitiatorToUrlFragment : PatientRecordInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorParticipantDirectory ->
            "participant-directory"

        InitiatorPatientRecord personId ->
            "patient-record-" ++ fromEntityUuid personId


progressReportInitiatorFromUrlFragment : String -> Maybe PatientRecordInitiator
progressReportInitiatorFromUrlFragment s =
    case s of
        "participant-directory" ->
            Just InitiatorParticipantDirectory

        _ ->
            if String.startsWith "patient-record-" s then
                String.dropLeft 15 s
                    |> toEntityUuid
                    |> InitiatorPatientRecord
                    |> Just

            else
                Nothing
