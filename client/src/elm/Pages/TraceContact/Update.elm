module Pages.TraceContact.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (SymptomsGISign(..), SymptomsGeneralSign(..), SymptomsRespiratorySign(..), TraceOutcome(..))
import Backend.Model
import Backend.TraceContact.Model
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.AcuteIllnessActivity.Types exposing (SymptomsTask(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.TraceContact.Model exposing (..)


update : NominalDate -> AcuteIllnessTraceContactId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    let
        generateSymptomsReviewMsgs nextTask =
            Maybe.map (\task -> [ SetActiveSymptomsTask task ]) nextTask
                |> Maybe.withDefault [ GenerateRecommendation ]

        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetTraceContactStep step ->
            ( { model | step = step }
            , Cmd.none
            , []
            )

        SetContactInitiated value ->
            case model.step of
                StepInitiateContact data ->
                    let
                        updatedData =
                            { data | contactInitiated = Just value }
                    in
                    ( { model | step = StepInitiateContact updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SetNoContactReason value ->
            case model.step of
                StepInitiateContact data ->
                    let
                        updatedData =
                            { data | noContactReason = Just value }
                    in
                    ( { model | step = StepInitiateContact updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SaveStepInitiateContact contact ->
            case model.step of
                StepInitiateContact data ->
                    let
                        ( extraMsgs, appMsgs ) =
                            if data.contactInitiated == Just True then
                                ( [ SetTraceContactStep <| StepRecordSymptoms emptyStepRecordSymptomsData ]
                                , []
                                )

                            else
                                let
                                    ( outcome, resolutionDate ) =
                                        case data.noContactReason of
                                            Just ReasonNoAnswer ->
                                                ( Nothing, contact.resolutionDate )

                                            Just ReasonWrongContactInfo ->
                                                ( Just OutcomeWrongContactInfo, currentDate )

                                            Just ReasonDeclinedFollowUp ->
                                                ( Just OutcomeDeclinedFollowUp, currentDate )

                                            Nothing ->
                                                ( Nothing, contact.resolutionDate )

                                    updated =
                                        { contact | lastFollowUpDate = Just currentDate, traceOutcome = outcome, resolutionDate = resolutionDate }
                                in
                                ( [ SetActivePage <| UserPage GlobalCaseManagementPage ]
                                , [ Backend.TraceContact.Model.EditTraceContact updated
                                        |> Backend.Model.MsgTraceContact id
                                        |> App.Model.MsgIndexedDb
                                  ]
                                )
                    in
                    ( model
                    , Cmd.none
                    , appMsgs
                    )
                        |> sequenceExtra (update currentDate id) extraMsgs

                _ ->
                    noChange

        SetActiveSymptomsTask task ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        updatedData =
                            { data | activeTask = Just task }
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        ToggleSymptomsGeneralSign sign ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        updatedSigns =
                            toggleSymptomsSign SymptomsGeneral sign NoSymptomsGeneral data.symptomsGeneralForm.signs

                        form =
                            data.symptomsGeneralForm

                        updatedForm =
                            { form | signs = updatedSigns }

                        updatedData =
                            { data | symptomsGeneralForm = updatedForm }
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        ToggleSymptomsRespiratorySign sign ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        updatedSigns =
                            toggleSymptomsSign SymptomsRespiratory sign NoSymptomsRespiratory data.symptomsRespiratoryForm.signs

                        form =
                            data.symptomsRespiratoryForm

                        updatedForm =
                            { form | signs = updatedSigns }

                        updatedData =
                            { data | symptomsRespiratoryForm = updatedForm }
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        ToggleSymptomsGISign sign ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        updatedSigns =
                            toggleSymptomsSign SymptomsGI sign NoSymptomsGI data.symptomsGIForm.signs

                        form =
                            data.symptomsGIForm

                        updatedForm =
                            { form | signs = updatedSigns }

                        updatedData =
                            { data | symptomsGIForm = updatedForm }
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SaveSymptomsGeneral nextTask ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        form =
                            data.symptomsGeneralForm

                        updatedForm =
                            { form | completed = True }

                        updatedData =
                            { data | symptomsGeneralForm = updatedForm }

                        extraMsgs =
                            generateSymptomsReviewMsgs nextTask

                        appMsgs =
                            -- @todo
                            []
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )
                        |> sequenceExtra (update currentDate id) extraMsgs

                _ ->
                    noChange

        SaveSymptomsRespiratory nextTask ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        form =
                            data.symptomsRespiratoryForm

                        updatedForm =
                            { form | completed = True }

                        updatedData =
                            { data | symptomsRespiratoryForm = updatedForm }

                        extraMsgs =
                            generateSymptomsReviewMsgs nextTask

                        appMsgs =
                            -- @todo
                            []
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )
                        |> sequenceExtra (update currentDate id) extraMsgs

                _ ->
                    noChange

        SaveSymptomsGI nextTask ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        form =
                            data.symptomsGIForm

                        updatedForm =
                            { form | completed = True }

                        updatedData =
                            { data | symptomsGIForm = updatedForm }

                        extraMsgs =
                            generateSymptomsReviewMsgs nextTask

                        appMsgs =
                            -- @todo
                            []
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )
                        |> sequenceExtra (update currentDate id) extraMsgs

                _ ->
                    noChange

        SetRecordSymptomsPopupState state ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        updatedData =
                            { data | popupState = state }
                    in
                    ( { model | step = StepRecordSymptoms updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        GenerateRecommendation ->
            case model.step of
                StepRecordSymptoms data ->
                    let
                        symptomsFound noneSign symptomsSet =
                            if EverySet.isEmpty symptomsSet then
                                False

                            else if EverySet.toList symptomsSet == [ noneSign ] then
                                False

                            else
                                True

                        popupState =
                            if
                                symptomsFound NoSymptomsGeneral data.symptomsGeneralForm.signs
                                    || symptomsFound NoSymptomsRespiratory data.symptomsRespiratoryForm.signs
                                    || symptomsFound NoSymptomsGI data.symptomsGIForm.signs
                            then
                                StateSymptomsFound

                            else
                                StateSymptomsNotFound
                    in
                    ( model
                    , Cmd.none
                    , []
                    )
                        |> sequenceExtra (update currentDate id) [ SetRecordSymptomsPopupState <| Just popupState ]

                _ ->
                    noChange


toggleSymptomsSign : SymptomsTask -> a -> a -> EverySet a -> EverySet a
toggleSymptomsSign task sign noneSign signs =
    if sign == noneSign then
        EverySet.singleton sign

    else
        let
            signs_ =
                EverySet.remove noneSign signs
        in
        if EverySet.member sign signs_ then
            EverySet.remove sign signs_

        else
            EverySet.insert sign signs_
