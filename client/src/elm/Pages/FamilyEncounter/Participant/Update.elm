module Pages.FamilyEncounter.Participant.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.FamilyEncounter.Model
import Backend.IndividualEncounterParticipant.Model
import Backend.Model
import Backend.Person.Form exposing (PersonForm)
import Backend.Person.Model exposing (ParticipantDirectoryOperation(..), Person)
import Backend.Village.Utils exposing (getVillageById)
import Components.PatientsSearchForm.Update
import Date
import Form
import GeoLocation.Model exposing (ReverseGeoInfo)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.FamilyEncounter.Participant.Model exposing (Model, Msg(..), maxChildren)
import Pages.Page exposing (Page(..), UserPage(..))
import SyncManager.Model exposing (Site)


update :
    NominalDate
    -> Site
    -> ReverseGeoInfo
    -> HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Backend.Model.ModelIndexedDb
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        MsgPatientsSearchForm subMsg ->
            let
                ( searchForm, cmd ) =
                    Components.PatientsSearchForm.Update.update subMsg model.searchForm
            in
            ( { model | searchForm = searchForm }
            , Cmd.map MsgPatientsSearchForm cmd
            , []
            )

        SelectMother personId person ->
            ( { model
                | selectedMother = Just (ExistingMother personId person)
                , registrationMode = SearchMode
              }
            , Cmd.none
            , []
            )

        DeselectMother ->
            ( { model | selectedMother = Nothing }
            , Cmd.none
            , []
            )

        RegisterNewMother ->
            let
                motherForm =
                    { form = Backend.Person.Form.emptyCreateForm site
                    , dateSelectorPopupState = Nothing
                    }
            in
            ( { model
                | selectedMother = Just NewMother
                , registrationMode = RegisterMotherMode
                , motherForm = Just motherForm
              }
            , Cmd.none
            , []
            )

        BackToSearch ->
            ( { model
                | registrationMode = SearchMode
                , motherForm = Nothing
                , childForm = Nothing
              }
            , Cmd.none
            , []
            )

        MsgMotherForm subMsg ->
            case model.motherForm of
                Just motherFormData ->
                    let
                        newForm =
                            Form.update
                                (Backend.Person.Form.validatePerson site Nothing (CreatePerson Nothing) (Just currentDate))
                                subMsg
                                motherFormData.form

                        ( appMsgs, extraMsgs ) =
                            case subMsg of
                                Form.Submit ->
                                    let
                                        formWithDefaults =
                                            Backend.Person.Form.applyDefaultValuesForPerson
                                                currentDate
                                                site
                                                reverseGeoInfo
                                                (Maybe.andThen (getVillageById db) maybeVillageId)
                                                isChw
                                                Nothing
                                                (CreatePerson Nothing)
                                                (Backend.Person.Model.IndividualEncounterOrigin Backend.IndividualEncounterParticipant.Model.FamilyEncounter)
                                                motherFormData.form
                                    in
                                    Form.getOutput formWithDefaults
                                        |> Maybe.map
                                            (\person ->
                                                let
                                                    personWithShard =
                                                        { person | shard = Just selectedHealthCenter }
                                                in
                                                ( [ Backend.Model.PostPerson
                                                        Nothing
                                                        (Backend.Person.Model.IndividualEncounterOrigin Backend.IndividualEncounterParticipant.Model.FamilyEncounter)
                                                        personWithShard
                                                        |> App.Model.MsgIndexedDb
                                                  ]
                                                , []
                                                )
                                            )
                                        |> Maybe.withDefault ( [], [] )

                                _ ->
                                    ( [], [] )
                    in
                    ( { model | motherForm = Just { motherFormData | form = newForm } }
                    , Cmd.none
                    , appMsgs
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        MsgChildForm subMsg ->
            case model.childForm of
                Just childFormData ->
                    let
                        newForm =
                            Form.update
                                (Backend.Person.Form.validatePerson site Nothing (CreatePerson Nothing) (Just currentDate))
                                subMsg
                                childFormData.form

                        ( appMsgs, extraMsgs ) =
                            case subMsg of
                                Form.Submit ->
                                    let
                                        formWithDefaults =
                                            Backend.Person.Form.applyDefaultValuesForPerson
                                                currentDate
                                                site
                                                reverseGeoInfo
                                                (Maybe.andThen (getVillageById db) maybeVillageId)
                                                isChw
                                                Nothing
                                                (CreatePerson Nothing)
                                                (Backend.Person.Model.IndividualEncounterOrigin Backend.IndividualEncounterParticipant.Model.FamilyEncounter)
                                                childFormData.form
                                    in
                                    Form.getOutput formWithDefaults
                                        |> Maybe.map
                                            (\person ->
                                                let
                                                    personWithShard =
                                                        { person | shard = Just selectedHealthCenter }
                                                in
                                                ( [ Backend.Model.PostPerson
                                                        Nothing
                                                        (Backend.Person.Model.IndividualEncounterOrigin Backend.IndividualEncounterParticipant.Model.FamilyEncounter)
                                                        personWithShard
                                                        |> App.Model.MsgIndexedDb
                                                  ]
                                                , []
                                                )
                                            )
                                        |> Maybe.withDefault ( [], [] )

                                _ ->
                                    ( [], [] )
                    in
                    ( { model | childForm = Just { childFormData | form = newForm } }
                    , Cmd.none
                    , appMsgs
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        AddChild personId person ->
            if List.length model.selectedChildren >= maxChildren then
                ( model, Cmd.none, [] )

            else if List.member personId model.selectedChildren then
                ( model, Cmd.none, [] )

            else
                ( { model | selectedChildren = model.selectedChildren ++ [ personId ] }
                , Cmd.none
                , []
                )

        RemoveChild personId ->
            ( { model | selectedChildren = List.filter (\id -> id /= personId) model.selectedChildren }
            , Cmd.none
            , []
            )

        RegisterNewChild ->
            let
                childForm =
                    { form = Backend.Person.Form.emptyCreateForm site
                    , dateSelectorPopupState = Nothing
                    }
            in
            ( { model
                | registrationMode = RegisterChildMode
                , childForm = Just childForm
              }
            , Cmd.none
            , []
            )

        DropZoneCompleteMotherPhoto file ->
            case model.motherForm of
                Just motherFormData ->
                    ( { model
                        | motherForm =
                            Just
                                { motherFormData
                                    | form = Form.update Backend.Person.Form.emptyValidation (Form.Input "photo" Form.Text (Form.Field.String file.url)) motherFormData.form
                                }
                      }
                    , Cmd.none
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        DropZoneCompleteChildPhoto file ->
            case model.childForm of
                Just childFormData ->
                    ( { model
                        | childForm =
                            Just
                                { childFormData
                                    | form = Form.update Backend.Person.Form.emptyValidation (Form.Input "photo" Form.Text (Form.Field.String file.url)) childFormData.form
                                }
                      }
                    , Cmd.none
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        DateSelectedMother date ->
            case model.motherForm of
                Just motherFormData ->
                    ( { model
                        | motherForm =
                            Just
                                { motherFormData
                                    | form = Form.update Backend.Person.Form.emptyValidation (Form.Input "birthDate" Form.Text (Form.Field.String <| Date.toIsoString date)) motherFormData.form
                                    , dateSelectorPopupState = Nothing
                                }
                      }
                    , Cmd.none
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        DateSelectedChild date ->
            case model.childForm of
                Just childFormData ->
                    ( { model
                        | childForm =
                            Just
                                { childFormData
                                    | form = Form.update Backend.Person.Form.emptyValidation (Form.Input "birthDate" Form.Text (Form.Field.String <| Date.toIsoString date)) childFormData.form
                                    , dateSelectorPopupState = Nothing
                                }
                      }
                    , Cmd.none
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        SetMotherDateSelectorState state ->
            case model.motherForm of
                Just motherFormData ->
                    ( { model | motherForm = Just { motherFormData | dateSelectorPopupState = state } }
                    , Cmd.none
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        SetChildDateSelectorState state ->
            case model.childForm of
                Just childFormData ->
                    ( { model | childForm = Just { childFormData | dateSelectorPopupState = state } }
                    , Cmd.none
                    , []
                    )

                Nothing ->
                    ( model, Cmd.none, [] )

        CreateEncounter ->
            case model.selectedMother of
                Just (ExistingMother motherId motherPerson) ->
                    let
                        -- Create IndividualEncounterParticipant for mother
                        individualParticipant =
                            Backend.IndividualEncounterParticipant.Model.emptyIndividualEncounterParticipant
                                currentDate
                                motherId
                                Backend.IndividualEncounterParticipant.Model.FamilyEncounter
                                selectedHealthCenter
                    in
                    ( model
                    , Cmd.none
                    , [ Backend.Model.PostIndividualEncounterParticipant
                            Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                            individualParticipant
                            |> App.Model.MsgIndexedDb
                      ]
                    )

                _ ->
                    ( model, Cmd.none, [] )
