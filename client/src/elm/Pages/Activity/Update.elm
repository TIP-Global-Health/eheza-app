module Pages.Activity.Update exposing (updateChild, updateMother)

import Activity.Model exposing (ChildActivity(..))
import App.Ports exposing (bindDropZone)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (MeasurementData, MotherMeasurements)
import Measurement.Model
import Measurement.Update
import Pages.Activity.Model exposing (ChildUpdateReturns, Model, MotherUpdateReturns, Msg(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import SyncManager.Model exposing (Site)


{-| Ideally, these would be more generic, but it's easier to have
a separate `updateChild` and `updateMother` for the moment. This
is similar to the code in `Pages.Participant.Update`.
-}
updateChild :
    Site
    -> Msg PersonId Measurement.Model.MsgChild
    -> Model PersonId
    -> ChildActivity
    -> Maybe Measurement.Model.ModelChild
    -> ChildUpdateReturns
updateChild site msg model activity childForm =
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
                                Measurement.Update.updateChild site subMsg form
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
            ChildUpdateReturns
                { model | filter = filter }
                Cmd.none
                Nothing
                Nothing
                Nothing

        SetSelectedParticipant val ->
            ChildUpdateReturns
                { model | selectedParticipant = val }
                Cmd.none
                Nothing
                Nothing
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
            in
            ChildUpdateReturns
                { model | selectedTab = val }
                cmd
                Nothing
                Nothing
                Nothing


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
