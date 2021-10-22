module Pages.AcuteIllnessActivity.Fetch exposing (fetch)

import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.AcuteIllnessActivity.Model exposing (Model)
import Pages.AcuteIllnessEncounter.Fetch


fetch : AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> List MsgIndexedDb
fetch id activity db model =
    let
        fetchSearchedParticipantsMsgs =
            case activity of
                AcuteIllnessNextSteps ->
                    let
                        trimmed =
                            Maybe.withDefault "" model.nextStepsData.contactsTracingForm.search
                                |> String.trim
                    in
                    if String.isEmpty trimmed then
                        []

                    else
                        [ FetchPeopleByName trimmed ]

                _ ->
                    []
    in
    Pages.AcuteIllnessEncounter.Fetch.fetch id db ++ fetchSearchedParticipantsMsgs
