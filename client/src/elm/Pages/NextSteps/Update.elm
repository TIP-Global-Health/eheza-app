module Pages.NextSteps.Update exposing (update)

import Activity.Model exposing (Activity)
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , ContributingFactorsSign(..)
        )
import Backend.Session.Model
import Measurement.Model
import Measurement.Utils exposing (contributingFactorsFormWithDefault, followUpFormWithDefault, healthEducationFormWithDefault, sendToHCFormWithDefault)
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Session.Model
import RemoteData exposing (RemoteData(..))


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : PersonId -> Activity -> Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update childId activity msg model =
    case msg of
        SetRedirectPage page ->
            ( model
            , Cmd.none
            , [ Pages.Session.Model.SetActivePage page ]
            )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

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

        SetReasonForNotSendingToHC value ->
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

        SaveSendToHC valueId value nextTask_ ->
            let
                saveMsg =
                    Measurement.Model.SaveSendToHC valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                ( backToOriginMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ Pages.Session.Model.SetActiveSessionPage (ActivityPage activity) ]
                            , Nothing
                            )
            in
            ( { model | activeTask = nextTask }
            , Cmd.none
            , saveMsg :: backToOriginMsg
            )

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

        SaveHealthEducation valueId value nextTask_ ->
            let
                saveMsg =
                    Measurement.Model.SaveHealthEducation valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                ( backToOriginMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ Pages.Session.Model.SetActiveSessionPage (ActivityPage activity) ]
                            , Nothing
                            )
            in
            ( { model | activeTask = nextTask }
            , Cmd.none
            , saveMsg :: backToOriginMsg
            )

        SetContributingFactorsSign sign ->
            let
                form =
                    model.contributingFactorsForm

                updatedForm =
                    case form.signs of
                        Just signs ->
                            if List.member sign signs then
                                let
                                    updatedSigns =
                                        if List.length signs == 1 then
                                            Nothing

                                        else
                                            signs |> List.filter ((/=) sign) |> Just
                                in
                                { form | signs = updatedSigns }

                            else
                                case sign of
                                    NoContributingFactorsSign ->
                                        { form | signs = Just [ sign ] }

                                    _ ->
                                        let
                                            updatedSigns =
                                                case signs of
                                                    [ NoContributingFactorsSign ] ->
                                                        Just [ sign ]

                                                    _ ->
                                                        Just (sign :: signs)
                                        in
                                        { form | signs = updatedSigns }

                        Nothing ->
                            { form | signs = Just [ sign ] }
            in
            ( { model | contributingFactorsForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveContributingFactors valueId value nextTask_ ->
            let
                saveMsg =
                    Measurement.Model.SaveContributingFactors valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                ( backToOriginMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ Pages.Session.Model.SetActiveSessionPage (ActivityPage activity) ]
                            , Nothing
                            )
            in
            ( { model | activeTask = nextTask }
            , Cmd.none
            , saveMsg :: backToOriginMsg
            )

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

        SaveFollowUp valueId value nextTask_ ->
            let
                saveMsg =
                    Measurement.Model.SaveFollowUp valueId value
                        |> Backend.Session.Model.MeasurementOutMsgChild childId
                        |> Pages.Session.Model.MsgSession

                ( backToOriginMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ Pages.Session.Model.SetActiveSessionPage (ActivityPage activity) ]
                            , Nothing
                            )
            in
            ( { model | activeTask = nextTask }
            , Cmd.none
            , saveMsg :: backToOriginMsg
            )
