module Pages.AcuteIllnessActivity.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.AcuteIllnessActivity.Model exposing (ContactsTracingFormState(..), Model)
import Pages.AcuteIllnessEncounter.Fetch
import RemoteData


fetch : AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> List MsgIndexedDb
fetch id activity db model =
    let
        fetchContactsTracingMsgs =
            case activity of
                AcuteIllnessNextSteps ->
                    let
                        form =
                            model.nextStepsData.contactsTracingForm

                        fetchTracedPatientsMsg =
                            Dict.get id db.acuteIllnessMeasurements
                                |> Maybe.andThen RemoteData.toMaybe
                                |> Maybe.andThen (.contactsTracing >> getMeasurementValueFunc)
                                |> Maybe.map (List.map (.personId >> FetchPerson))
                                |> Maybe.withDefault []

                        fetchSearchMsg =
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
                    in
                    fetchTracedPatientsMsg ++ fetchSearchMsg

                _ ->
                    []
    in
    Pages.AcuteIllnessEncounter.Fetch.fetch id db ++ fetchContactsTracingMsgs
