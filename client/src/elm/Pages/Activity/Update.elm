module Pages.Activity.Update exposing (updateChild, updateMother)

import Activity.Model exposing (ChildActivity(..), CompletedAndPending)
import Activity.Utils
import App.Ports exposing (bindDropZone)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (MeasurementData, MotherMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import LocalData
import Measurement.Model
import Measurement.Update
import Pages.Activity.Model exposing (ChildUpdateReturns, Model, MotherUpdateReturns, Msg(..), Tab(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (matchFilter, normalizeFilter)
import SyncManager.Model exposing (SiteFeature)
import ZScore.Model


{-| Ideally, these would be more generic, but it's easier to have
a separate `updateChild` and `updateMother` for the moment. This
is similar to the code in `Pages.Participant.Update`.
-}
updateChild :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> Msg PersonId Measurement.Model.MsgChild
    -> Model PersonId
    -> EditableSession
    -> ChildActivity
    -> Maybe Measurement.Model.ModelChild
    -> ModelIndexedDb
    -> ChildUpdateReturns
updateChild currentDate zscores features msg model session activity childForm db =
    case msg of
        GoBackToActivitiesPage sessionId ->
            ChildUpdateReturns
                { model | filter = "" }
                Cmd.none
                childForm
                Nothing
                (Just <| UserPage <| SessionPage sessionId ActivitiesPage)

        MsgMeasurement subMsg ->
            childForm
                |> Maybe.map
                    (\form ->
                        let
                            ( subModel, subCmd, outMsg ) =
                                Measurement.Update.updateChild subMsg form
                        in
                        ChildUpdateReturns
                            model
                            (Cmd.map MsgMeasurement subCmd)
                            (Just subModel)
                            outMsg
                            Nothing
                    )
                |> Maybe.withDefault (ChildUpdateReturns model Cmd.none Nothing Nothing Nothing)

        SetFilter filter ->
            let
                updatedModel =
                    { model | filter = filter }

                outMsg =
                    if List.member activity [ Height, Muac, Weight ] |> not then
                        Nothing

                    else
                        let
                            participants =
                                calculateChildrenParticipants currentDate zscores features session activity db updatedModel
                        in
                        case model.selectedTab of
                            Pending ->
                                participants.pending
                                    |> Dict.toList
                                    |> List.head
                                    |> Maybe.map (Tuple.first >> Measurement.Model.FetchIndividualNutritionData)

                            Completed ->
                                participants.completed
                                    |> Dict.toList
                                    |> List.head
                                    |> Maybe.map (Tuple.first >> Measurement.Model.FetchIndividualNutritionData)
            in
            ChildUpdateReturns
                updatedModel
                Cmd.none
                Nothing
                outMsg
                Nothing

        SetSelectedParticipant val ->
            ChildUpdateReturns
                { model | selectedParticipant = val }
                Cmd.none
                Nothing
                (Maybe.map Measurement.Model.FetchIndividualNutritionData val)
                Nothing

        SetSelectedTab val ->
            let
                cmd =
                    -- When switching tabs at ChildPicture activity, bind
                    -- DropZone to be able to take pictures.
                    if activity == ChildPicture then
                        bindDropZone ()

                    else
                        Cmd.none

                outMsg =
                    if List.member activity [ Height, Muac, Weight ] |> not then
                        Nothing

                    else
                        case val of
                            Pending ->
                                Nothing

                            Completed ->
                                calculateChildrenParticipants currentDate zscores features session activity db model
                                    |> .completed
                                    |> Dict.toList
                                    |> List.head
                                    |> Maybe.map (Tuple.first >> Measurement.Model.FetchIndividualNutritionData)
            in
            ChildUpdateReturns
                { model | selectedTab = val }
                cmd
                Nothing
                outMsg
                Nothing


{-| Given child activity, this function calculates which children should be presented at Todo tab (yet to complete the activity),
and which should be presented at Completed tab (the ones that have already completed the activity).
It also takes into consideration the 'name' filter that exists on activity page.
-}
calculateChildrenParticipants :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> EditableSession
    -> ChildActivity
    -> ModelIndexedDb
    -> Model PersonId
    -> CompletedAndPending (Dict PersonId Person)
calculateChildrenParticipants currentDate zscores features session activity db model =
    let
        applyNameFilter { pending, completed } =
            { pending = Dict.filter filterParticipantNames pending
            , completed = Dict.filter filterParticipantNames completed
            }

        filterParticipantNames _ participant =
            participant.name
                |> matchFilter filter

        filter =
            normalizeFilter model.filter
    in
    session.checkedIn
        |> LocalData.unwrap
            { completed = Dict.empty
            , pending = Dict.empty
            }
            (Activity.Utils.summarizeChildActivity currentDate zscores features activity session.offlineSession False db >> applyNameFilter)


updateMother :
    Msg PersonId Measurement.Model.MsgMother
    -> Model PersonId
    -> Maybe Measurement.Model.ModelMother
    -> MeasurementData MotherMeasurements
    -> MotherUpdateReturns
updateMother msg model motherForm measurements =
    case msg of
        GoBackToActivitiesPage sessionId ->
            MotherUpdateReturns
                model
                Cmd.none
                Nothing
                Nothing
                (Just <| UserPage <| SessionPage sessionId ActivitiesPage)

        MsgMeasurement subMsg ->
            motherForm
                |> Maybe.map
                    (\form ->
                        let
                            ( subModel, subCmd, outMsg ) =
                                Measurement.Update.updateMother measurements subMsg form
                        in
                        MotherUpdateReturns
                            model
                            (Cmd.map MsgMeasurement subCmd)
                            (Just subModel)
                            outMsg
                            Nothing
                    )
                |> Maybe.withDefault (MotherUpdateReturns model Cmd.none Nothing Nothing Nothing)

        SetFilter filter ->
            MotherUpdateReturns
                { model | filter = filter }
                Cmd.none
                Nothing
                Nothing
                Nothing

        SetSelectedParticipant val ->
            MotherUpdateReturns
                { model | selectedParticipant = val }
                Cmd.none
                Nothing
                Nothing
                Nothing

        SetSelectedTab val ->
            MotherUpdateReturns
                { model | selectedTab = val }
                Cmd.none
                Nothing
                Nothing
                Nothing
