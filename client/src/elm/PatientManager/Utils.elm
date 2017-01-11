module PatientManager.Utils
    exposing
        ( getPatient
        , wrapPatientsDict
        , unwrapPatientsDict
        )

import Dict exposing (Dict)
import Patient.Model exposing (Patient, PatientId, PatientsDict)
import PatientManager.Model as PatientManager
import RemoteData exposing (RemoteData(..), WebData)


getPatient : PatientId -> PatientManager.Model -> WebData Patient
getPatient id model =
    Dict.get id model.patients
        |> Maybe.withDefault NotAsked


wrapPatientsDict : PatientsDict -> Dict PatientId (WebData Patient)
wrapPatientsDict =
    Dict.map (\_ patient -> Success patient)


unwrapPatientsDict : Dict PatientId (WebData Patient) -> PatientsDict
unwrapPatientsDict wrappedPatientsDict =
    wrappedPatientsDict
        |> Dict.foldl
            (\patientId wrappedPatient accum ->
                case wrappedPatient of
                    Success patient ->
                        ( patientId, patient ) :: accum

                    _ ->
                        accum
            )
            []
        |> Dict.fromList
