module Pages.Activity.Update exposing (updateChild, updateMother)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (MeasurementData, MotherMeasurements)
import Measurement.Model
import Measurement.Update
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))


{-| Ideally, these would be more generic, but it's easier to have
a separate `updateChild` and `updateMother` for the moment. This
is similar to the code in `Pages.Participant.Update`.
-}
updateChild :
    Msg ChildId Measurement.Model.MsgChild
    -> Model ChildId
    -> Maybe Measurement.Model.ModelChild
    -> ( Model ChildId, Cmd (Msg ChildId Measurement.Model.MsgChild), Maybe Measurement.Model.ModelChild, Maybe Measurement.Model.OutMsgChild, Maybe Page )
updateChild msg model childForm =
    case msg of
        GoBackToActivitiesPage sessionId ->
            ( { model | filter = "" }
            , Cmd.none
            , childForm
            , Nothing
            , Just <| UserPage <| SessionPage sessionId ActivitiesPage
            )

        MsgMeasurement subMsg ->
            childForm
                |> Maybe.map
                    (\form ->
                        let
                            ( subModel, subCmd, outMsg ) =
                                Measurement.Update.updateChild subMsg form
                        in
                        ( model
                        , Cmd.map MsgMeasurement subCmd
                        , Just subModel
                        , outMsg
                        , Nothing
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none, Nothing, Nothing, Nothing )

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , Nothing
            , Nothing
            , Nothing
            )

        SetSelectedParticipant val ->
            ( { model | selectedParticipant = val }
            , Cmd.none
            , Nothing
            , Nothing
            , Nothing
            )

        SetSelectedTab val ->
            ( { model | selectedTab = val }
            , Cmd.none
            , Nothing
            , Nothing
            , Nothing
            )


updateMother :
    Msg MotherId Measurement.Model.MsgMother
    -> Model MotherId
    -> Maybe Measurement.Model.ModelMother
    -> MeasurementData MotherMeasurements
    -> ( Model MotherId, Cmd (Msg MotherId Measurement.Model.MsgMother), Maybe Measurement.Model.ModelMother, Maybe Measurement.Model.OutMsgMother, Maybe Page )
updateMother msg model motherForm measurements =
    case msg of
        GoBackToActivitiesPage sessionId ->
            ( model, Cmd.none, Nothing, Nothing, Just <| UserPage <| SessionPage sessionId ActivitiesPage )

        MsgMeasurement subMsg ->
            motherForm
                |> Maybe.map
                    (\form ->
                        let
                            ( subModel, subCmd, outMsg ) =
                                Measurement.Update.updateMother measurements subMsg form
                        in
                        ( model
                        , Cmd.map MsgMeasurement subCmd
                        , Just subModel
                        , outMsg
                        , Nothing
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none, Nothing, Nothing, Nothing )

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , Nothing
            , Nothing
            , Nothing
            )

        SetSelectedParticipant val ->
            ( { model | selectedParticipant = val }
            , Cmd.none
            , Nothing
            , Nothing
            , Nothing
            )

        SetSelectedTab val ->
            ( { model | selectedTab = val }
            , Cmd.none
            , Nothing
            , Nothing
            , Nothing
            )
