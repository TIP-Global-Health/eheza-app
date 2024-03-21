module Pages.EducationSession.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EverySet exposing (EverySet)
import Pages.EducationSession.Model exposing (Model)
import RemoteData


fetch : EducationSessionId -> ModelIndexedDb -> Model -> List MsgIndexedDb
fetch id db model =
    let
        fetchSessionParticipants =
            Dict.get id db.educationSessions
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map (.participants >> EverySet.toList >> FetchPeople >> List.singleton)
                |> Maybe.withDefault []

        fetchPeopleByName =
            let
                trimmed =
                    Maybe.withDefault "" model.search
                        |> String.trim
            in
            if String.isEmpty trimmed then
                []

            else
                [ FetchPeopleByName trimmed ]
    in
    [ FetchVillages, FetchEducationSession id ]
        ++ fetchSessionParticipants
        ++ fetchPeopleByName
