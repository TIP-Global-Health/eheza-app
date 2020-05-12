module Pages.Activity.Update exposing (updateChild, updateMother)

import Activity.Model exposing (ChildActivity(..), CompletedAndPending)
import Activity.Utils
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (MeasurementData, MotherMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import Gizra.NominalDate exposing (NominalDate)
import LocalData
import Measurement.Model
import Measurement.Update
import Pages.Activity.Model exposing (ChildUpdateReturns, Model, MotherUpdateReturns, Msg(..), Tab(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (matchFilter, normalizeFilter)


{-| Ideally, these would be more generic, but it's easier to have
a separate `updateChild` and `updateMother` for the moment. This
is similar to the code in `Pages.Participant.Update`.
-}
updateChild :
    NominalDate
    -> Msg PersonId Measurement.Model.MsgChild
    -> Model PersonId
    -> EditableSession
    -> ChildActivity
    -> Maybe Measurement.Model.ModelChild
    -> ChildUpdateReturns
updateChild currentDate msg model session activity childForm =
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

                participants =
                    calculateChildrenParticipants currentDate session activity updatedModel

                outMsg =
                    if List.member activity [ Height, Muac, Weight ] |> not then
                        Nothing

                    else
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
                outMsg =
                    if List.member activity [ Height, Muac, Weight ] |> not then
                        Nothing

                    else
                        case val of
                            Pending ->
                                Nothing

                            Completed ->
                                calculateChildrenParticipants currentDate session activity model
                                    |> .completed
                                    |> Dict.toList
                                    |> List.head
                                    |> Maybe.map (Tuple.first >> Measurement.Model.FetchIndividualNutritionData)
            in
            ChildUpdateReturns
                { model | selectedTab = val }
                Cmd.none
                Nothing
                outMsg
                Nothing


calculateChildrenParticipants : NominalDate -> EditableSession -> ChildActivity -> Model PersonId -> CompletedAndPending (Dict PersonId Person)
calculateChildrenParticipants currentDate session activity model =
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
            (Activity.Utils.summarizeChildActivity currentDate activity session.offlineSession False >> applyNameFilter)


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
