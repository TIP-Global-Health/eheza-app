module Measurement.Update exposing (updateChild, updateMother)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , ContributingFactorsSign(..)
        , FamilyPlanningSign(..)
        , ImageUrl(..)
        , LactationSign(..)
        , MeasurementData
        , MotherMeasurements
        , MuacInCm(..)
        , WeightInGrm(..)
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (currentValues, mapMeasurementData)
import EverySet
import Measurement.Model exposing (ModelChild, ModelMother, MsgChild(..), MsgMother(..), OutMsgChild(..), OutMsgMother(..), emptyParticipantFormProgress)
import Pages.Utils exposing (setMultiSelectInputValue)


{-| The strategy used here, for the moment, is that the `model` tracks the UI,
(so, for instance, strings instead of floats, and changing on every kepress),
whereas the `editableMeasurements` tracks the underlying data. The only thing
which we can change **directly** here is our own model. If we want to change
the "real" data, we have to return an `OutMsg` to be processed elsehwere (for
instance, by actually writing the data to local storage).
-}
updateChild : MsgChild -> ModelChild -> ( ModelChild, Cmd MsgChild, Maybe OutMsgChild )
updateChild msg model =
    case msg of
        UpdateHeight val ->
            ( { model | height = val }
            , Cmd.none
            , Nothing
            )

        UpdateMuac val ->
            ( { model | muac = val }
            , Cmd.none
            , Nothing
            )

        SelectCounselingTopic selected topicId ->
            let
                counseling =
                    if selected then
                        Maybe.map (Tuple.mapSecond (EverySet.insert topicId)) model.counseling

                    else
                        Maybe.map (Tuple.mapSecond (EverySet.remove topicId)) model.counseling
            in
            ( { model | counseling = counseling }
            , Cmd.none
            , Nothing
            )

        SelectNutritionSign selected sign ->
            let
                nutrition =
                    model.nutrition

                updatedSigns =
                    if selected then
                        case sign of
                            NormalChildNutrition ->
                                -- If the user checks `None`, then we want that
                                -- to be the only sign.
                                EverySet.singleton sign

                            _ ->
                                -- If the user checks something else, then also
                                -- make sure that `None` is unchecekd
                                nutrition.signs
                                    |> EverySet.insert sign
                                    |> EverySet.remove NormalChildNutrition

                    else
                        -- We're allowing `NoFamilyPanning` itself to be
                        -- un-checked here.  That probably makes sense ...  it
                        -- would mean that we haven't actually answered this
                        -- question ... that is, that we don't know the answer,
                        -- whereas `NoFamilyPlanning` being checked means that
                        -- we do know the answer, and it's that there aren't
                        -- any signs.
                        EverySet.remove sign nutrition.signs

                updatedNutrition =
                    { nutrition | signs = updatedSigns }
            in
            ( { model | nutrition = updatedNutrition }
            , Cmd.none
            , Nothing
            )

        SendOutMsgChild outMsg ->
            let
                newModel =
                    case outMsg of
                        SavePhoto _ _ ->
                            -- When we save a photo, we blank our local record
                            -- of the unsaved photo URL. We're saving the photo
                            -- locally, and when we succeed, we'll see it in
                            -- the view at the URL it gets.
                            { model | photo = Nothing }

                        _ ->
                            model
            in
            ( newModel
            , Cmd.none
            , Just outMsg
            )

        SetDistributedAmountForChild string ->
            let
                amount =
                    String.toFloat string
                        |> Maybe.andThen
                            (\number ->
                                if number < 0 then
                                    Nothing

                                else
                                    Just number
                            )

                fbfForm =
                    model.fbfForm
                        |> (\form -> { form | distributedAmount = amount })
            in
            ( { model | fbfForm = fbfForm }
            , Cmd.none
            , Nothing
            )

        SetDistributoinNoticeForChild notice ->
            let
                fbfForm =
                    model.fbfForm
                        |> (\form ->
                                if form.distributionNotice == Just notice then
                                    { form | distributionNotice = Nothing }

                                else
                                    { form | distributionNotice = Just notice }
                           )
            in
            ( { model | fbfForm = fbfForm }
            , Cmd.none
            , Nothing
            )

        UpdateWeight val ->
            ( { model | weight = val }
            , Cmd.none
            , Nothing
            )

        DropZoneComplete result ->
            ( { model | photo = Just (ImageUrl result.url) }
            , Cmd.none
            , Nothing
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
            , Nothing
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
            , Nothing
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
            , Nothing
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
            , Nothing
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
            , Nothing
            )

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
            , Nothing
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
            , Nothing
            )

        SetUpdateANCVisits value ->
            let
                form =
                    model.ncdaData.form

                updatedForm =
                    { form | updateANCVisits = Just value, ancVisitsDates = Just EverySet.empty }

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        ToggleANCVisitDate date ->
            let
                form =
                    model.ncdaData.form

                updatedANCVisitsDates =
                    Maybe.map
                        (\set ->
                            if EverySet.member date set then
                                EverySet.remove date set

                            else
                                EverySet.insert date set
                        )
                        form.ancVisitsDates
                        |> Maybe.withDefault (EverySet.singleton date)

                updateANCVisits =
                    if EverySet.isEmpty updatedANCVisitsDates then
                        Just False

                    else
                        form.updateANCVisits

                updatedForm =
                    { form | ancVisitsDates = Just updatedANCVisitsDates, updateANCVisits = updateANCVisits }

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetNCDABoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.ncdaData.form

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetBirthWeight string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | birthWeight = String.toFloat string |> Maybe.map WeightInGrm
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetChildReceivesVitaminA value ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | childReceivesVitaminA = Just value })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetStuntingLevel value ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | stuntingLevel = Just value })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetWeight string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | weight = String.toFloat string |> Maybe.map WeightInKg
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetMuac string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | muac = String.toFloat string |> Maybe.map MuacInCm
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetNCDAHelperState state ->
            let
                updatedData =
                    model.ncdaData
                        |> (\data -> { data | helperState = state })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )

        SetNCDAFormStep step ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | step = Just step })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , Nothing
            )


updateMother : MeasurementData MotherMeasurements -> MsgMother -> ModelMother -> ( ModelMother, Cmd MsgMother, Maybe OutMsgMother )
updateMother measurements msg model =
    case msg of
        SelectFamilyPlanningSign selected sign ->
            let
                signsUpdated =
                    if selected then
                        case sign of
                            NoFamilyPlanning ->
                                -- If the user checks `NoFamilyPlanning`, then
                                -- we want that to be the only sign.
                                EverySet.singleton sign

                            _ ->
                                -- If the user checks something else, then also
                                -- make sure that `NoFamilyPlanning` is
                                -- unchecekd
                                model.familyPlanningSigns
                                    |> EverySet.insert sign
                                    |> EverySet.remove NoFamilyPlanning

                    else
                        -- We're allowing `NoFamilyPanning` itself to be
                        -- un-checked here.  That probably makes sense ...  it
                        -- would mean that we haven't actually answered this
                        -- question ... that is, that we don't know the answer,
                        -- whereas `NoFamilyPlanning` being checked means that
                        -- we do know the answer, and it's that there aren't
                        -- any signs.
                        EverySet.remove sign model.familyPlanningSigns
            in
            ( { model | familyPlanningSigns = signsUpdated }
            , Cmd.none
            , Nothing
            )

        SelectLactationSign sign value ->
            let
                formUpdated =
                    case sign of
                        Breastfeeding ->
                            model.lactationForm
                                |> (\form -> { form | breastfeeding = Just value })

                        NoLactationSigns ->
                            model.lactationForm
            in
            ( { model | lactationForm = formUpdated }
            , Cmd.none
            , Nothing
            )

        SetCounselorSigned formId signed ->
            let
                updated =
                    Dict.get formId model.participantConsent.progress
                        |> Maybe.withDefault emptyParticipantFormProgress
                        |> (\progress -> Dict.insert formId { progress | counselorSigned = signed } model.participantConsent.progress)
            in
            (\consent ->
                ( { model | participantConsent = { consent | progress = updated } }
                , Cmd.none
                , Nothing
                )
            )
                model.participantConsent

        SetDistributedAmountForMother string ->
            let
                amount =
                    String.toFloat string
                        |> Maybe.andThen
                            (\number ->
                                if number < 0 then
                                    Nothing

                                else
                                    Just number
                            )

                fbfForm =
                    model.fbfForm
                        |> (\form -> { form | distributedAmount = amount })
            in
            ( { model | fbfForm = fbfForm }
            , Cmd.none
            , Nothing
            )

        SetDistributoinNoticeForMother notice ->
            let
                fbfForm =
                    model.fbfForm
                        |> (\form ->
                                if form.distributionNotice == Just notice then
                                    { form | distributionNotice = Nothing }

                                else
                                    { form | distributionNotice = Just notice }
                           )
            in
            ( { model | fbfForm = fbfForm }
            , Cmd.none
            , Nothing
            )

        SetParticipantSigned formId signed ->
            let
                updated =
                    Dict.get formId model.participantConsent.progress
                        |> Maybe.withDefault emptyParticipantFormProgress
                        |> (\progress -> Dict.insert formId { progress | participantSigned = signed } model.participantConsent.progress)
            in
            (\consent ->
                ( { model | participantConsent = { consent | progress = updated } }
                , Cmd.none
                , Nothing
                )
            )
                model.participantConsent

        ViewParticipantForm formId ->
            (\consent ->
                ( { model | participantConsent = { consent | view = formId } }
                , Cmd.none
                , Nothing
                )
            )
                model.participantConsent

        SendOutMsgMother outMsg ->
            let
                -- TODO: For the moment, we're just assuming that the save into
                -- the local cache succeeds ... we don't do any error checking.
                -- Once we do, this mechanism would transition to the handling
                -- for the RemoteData that represents the state of the save &
                -- the possible error message.
                updated =
                    case outMsg of
                        SaveCompletedForm _ formId _ ->
                            case model.participantConsent.view of
                                Just currentFormId ->
                                    if formId == currentFormId then
                                        -- If we're looking at this form, then
                                        -- either look at the next form, or
                                        -- nothing, if all completed already.
                                        selectNextForm measurements formId model

                                    else
                                        -- If we're already looking at a
                                        -- different form, stay there.
                                        model

                                Nothing ->
                                    -- If we weren't looking at a specific
                                    -- form, then no change needed.
                                    model

                        _ ->
                            model
            in
            ( updated
            , Cmd.none
            , Just outMsg
            )


selectNextForm : MeasurementData MotherMeasurements -> ParticipantFormId -> ModelMother -> ModelMother
selectNextForm measurements formId model =
    let
        completedFormIds =
            -- TODO: Note in the last step we treat the current formId as
            -- completed ...  once we're actually doing error checking on the
            -- save to the cache, we will need to adjust that (to take into
            -- account whether the save succeeded or not).
            measurements
                |> mapMeasurementData .consent
                |> currentValues
                |> List.map (Tuple.second >> .value >> .formId)
                |> EverySet.fromList
                |> EverySet.insert formId

        expectedFormIds =
            model.participantConsent.expected
                |> Dict.keys
                |> EverySet.fromList

        remaining =
            EverySet.diff expectedFormIds completedFormIds
                |> EverySet.toList
                |> List.head
    in
    model.participantConsent
        |> (\consent ->
                { model
                    | participantConsent =
                        { consent | view = remaining }
                }
           )
