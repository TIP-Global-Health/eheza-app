module PatientManager.Utils
    exposing
        ( getChildren
        , getMother
        , getPatient
        , wrapPatientsDict
        , unwrapPatientsDict
        )

import Child.Model exposing (Child, ChildId)
import Dict exposing (Dict)
import Mother.Model exposing (Mother, MotherId)
import Patient.Model exposing (Patient, PatientId, PatientType(..), PatientsDict)
import PatientManager.Model as PatientManager
import RemoteData exposing (RemoteData(..), WebData)


getChildren : Mother -> PatientManager.Model -> List (WebData Child)
getChildren mother model =
    List.map
        (\childId ->
            getPatient childId model |> getChild
        )
        mother.children


getChild : WebData Patient -> WebData Child
getChild patientWebData =
    case patientWebData of
        Success patient ->
            case patient.info of
                PatientChild child ->
                    Success child

                _ ->
                    NotAsked

        _ ->
            Loading


getMother : MotherId -> PatientManager.Model -> WebData Mother
getMother motherId model =
    case getPatient motherId model of
        Success patient ->
            case patient.info of
                PatientMother mother ->
                    Success mother

                _ ->
                    NotAsked

        _ ->
            Loading


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
