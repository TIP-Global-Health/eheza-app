module Pages.NextSteps.Update exposing (update)

import Activity.Model exposing (Activity)
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model
    exposing
        ( ContributingFactorsSign(..)
        )
import Backend.Session.Model
import Gizra.Update exposing (sequenceExtra)
import Measurement.Model
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.Page exposing (SessionPage(..))
import Pages.Session.Model
import Pages.Utils exposing (setMultiSelectInputValue)


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : PersonId -> Activity -> Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update childId activity msg model =
    let
        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActiveSessionPage (ActivityPage activity) ]
    in
    case msg of
        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        SetActiveSessionPage page ->
            ( model
            , Cmd.none
            , [ Pages.Session.Model.SetActiveSessionPage page ]
            )

        SetActiveNextStepsTask task ->
            ( { model | activeTask = Just task }
            , Cmd.none
            , []
            )

        SetReferToHealthCenter value ->
            let
                form =
                    model.sendToHCForm

                updatedForm =
                    { form | referToHealthCenter = Just value, reasonForNotSendingToHC = Nothing }
            in
            ( { model | sendToHCForm = updatedForm }
            , Cmd.none
            , []
            )

        SetHandReferralForm value ->
            let
                form =
                    model.sendToHCForm

                updatedForm =
                    { form | handReferralForm = Just value }
            in
            ( { model | sendToHCForm = updatedForm }
            , Cmd.none
            , []
            )

        SetReasonForNonReferral value ->
            let
                form =
                    model.sendToHCForm

                updatedForm =
                    { form | reasonForNotSendingToHC = Just value }
            in
            ( { model | sendToHCForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveSendToHC valueId value nextTask ->
            let
                saveMsg =
                    Measurement.Model.SaveSendToHC valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                extraMsgs =
                    generateNextStepsMsgs nextTask
            in
            ( model
            , Cmd.none
            , [ saveMsg ]
            )
                |> sequenceExtra (update childId activity) extraMsgs

        SetProvidedEducationForDiagnosis value ->
            let
                form =
                    model.healthEducationForm

                updatedForm =
                    { form | educationForDiagnosis = Just value, reasonForNotProvidingHealthEducation = Nothing }
            in
            ( { model | healthEducationForm = updatedForm }
            , Cmd.none
            , []
            )

        SetReasonForNotProvidingHealthEducation value ->
            let
                form =
                    model.healthEducationForm

                updatedForm =
                    { form | reasonForNotProvidingHealthEducation = Just value }
            in
            ( { model | healthEducationForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveHealthEducation valueId value nextTask ->
            let
                saveMsg =
                    Measurement.Model.SaveHealthEducation valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                extraMsgs =
                    generateNextStepsMsgs nextTask
            in
            ( model
            , Cmd.none
            , [ saveMsg ]
            )
                |> sequenceExtra (update childId activity) extraMsgs

        SetContributingFactorsSign sign ->
            let
                form =
                    model.contributingFactorsForm

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoContributingFactorsSign
                        sign
                        form
            in
            ( { model | contributingFactorsForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveContributingFactors valueId value nextTask ->
            let
                saveMsg =
                    Measurement.Model.SaveContributingFactors valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                extraMsgs =
                    generateNextStepsMsgs nextTask
            in
            ( model
            , Cmd.none
            , [ saveMsg ]
            )
                |> sequenceExtra (update childId activity) extraMsgs

        SetFollowUpOption option ->
            let
                form =
                    model.followUpForm

                updatedForm =
                    { form | option = Just option }
            in
            ( { model | followUpForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveFollowUp valueId value nextTask ->
            let
                saveMsg =
                    Measurement.Model.SaveFollowUp valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                extraMsgs =
                    generateNextStepsMsgs nextTask
            in
            ( model
            , Cmd.none
            , [ saveMsg ]
            )
                |> sequenceExtra (update childId activity) extraMsgs
