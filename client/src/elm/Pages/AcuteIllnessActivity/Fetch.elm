module Pages.AcuteIllnessActivity.Fetch exposing (fetch)

import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.AcuteIllnessActivity.Model exposing (ContactsTracingFormState(..), Model)
import Pages.AcuteIllnessEncounter.Fetch


fetch : AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> List MsgIndexedDb
fetch id activity db model =
    let
        fetchSearchedParticipantsMsgs =
            case activity of
                AcuteIllnessNextSteps ->
                    let
                        form =
                            model.nextStepsData.contactsTracingForm
                    in
                    case form.state of
                        ContactsTracingFormSearchParticipants searchData ->
                            let
                                trimmed =
                                    Maybe.withDefault "" searchData.search
                                        |> String.trim
                            in
                            if String.isEmpty trimmed then
                                []

                            else
                                [ FetchPeopleByName trimmed ]

                        _ ->
                            []

                _ ->
                    []
    in
    Pages.AcuteIllnessEncounter.Fetch.fetch id db ++ fetchSearchedParticipantsMsgs
