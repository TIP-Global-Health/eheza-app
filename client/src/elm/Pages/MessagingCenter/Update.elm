module Pages.MessagingCenter.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Utils exposing (resilienceRoleFromString)
import Backend.Person.Utils
    exposing
        ( educationLevelFromInt
        , genderFromString
        , maritalStatusFromString
        , ubudeheFromInt
        )
import Gizra.NominalDate exposing (NominalDate)
import Pages.MessagingCenter.Model exposing (..)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetRole value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | role = resilienceRoleFromString value }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBirthDate date ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | birthDate = Just date }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBirthDateSelectorState state ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | dateSelectorPopupState = state }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetGender value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | gender = genderFromString value }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetEducationLevel value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form
                        | educationLevel =
                            String.toInt value
                                |> Maybe.andThen educationLevelFromInt
                    }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetUbudehe value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form
                        | ubudehe =
                            String.toInt value
                                |> Maybe.andThen ubudeheFromInt
                    }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetMaritalStatus value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | maritalStatus = maritalStatusFromString value }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )
