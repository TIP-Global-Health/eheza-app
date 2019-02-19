port module Measurement.Update exposing (updateChild, updateMother)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..), MeasurementData, MotherMeasurements, PhotoValue)
import Backend.Measurement.Utils exposing (currentValues, mapMeasurementData)
import Config.Model exposing (BackendUrl)
import EveryDict
import EveryDictList
import EverySet exposing (EverySet)
import Measurement.Model exposing (..)


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
                nutritionSignsUpdated =
                    if selected then
                        case sign of
                            None ->
                                -- If the user checks `None`, then we want that
                                -- to be the only sign.
                                EverySet.singleton sign

                            _ ->
                                -- If the user checks something else, then also
                                -- make sure that `None` is unchecekd
                                model.nutritionSigns
                                    |> EverySet.insert sign
                                    |> EverySet.remove None

                    else
                        -- We're allowing `NoFamilyPanning` itself to be
                        -- un-checked here.  That probably makes sense ...  it
                        -- would mean that we haven't actually answered this
                        -- question ... that is, that we don't know the answer,
                        -- whereas `NoFamilyPlanning` being checked means that
                        -- we do know the answer, and it's that there aren't
                        -- any signs.
                        EverySet.remove sign model.nutritionSigns
            in
            ( { model | nutritionSigns = nutritionSignsUpdated }
            , Cmd.none
            , Nothing
            )

        SendOutMsgChild outMsg ->
            ( model
            , Cmd.none
            , Just outMsg
            )

        UpdateWeight val ->
            ( { model | weight = val }
            , Cmd.none
            , Nothing
            )

        DropZoneComplete result ->
            -- The `fid` being Nothing signifies that we haven't uploaded this to
            -- the backend yet, so we don't know what file ID the backend will
            -- ultimately give it.
            ( { model
                | photo =
                    Just
                        { url = result.url
                        , fid = Nothing
                        }
              }
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

        SetCounselorSigned formId signed ->
            let
                updated =
                    EveryDict.get formId model.participantConsent.progress
                        |> Maybe.withDefault emptyParticipantFormProgress
                        |> (\progress -> EveryDict.insert formId { progress | counselorSigned = signed } model.participantConsent.progress)
            in
            (\consent ->
                ( { model | participantConsent = { consent | progress = updated } }
                , Cmd.none
                , Nothing
                )
            )
                model.participantConsent

        SetParticipantSigned formId signed ->
            let
                updated =
                    EveryDict.get formId model.participantConsent.progress
                        |> Maybe.withDefault emptyParticipantFormProgress
                        |> (\progress -> EveryDict.insert formId { progress | participantSigned = signed } model.participantConsent.progress)
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
                        SaveCompletedForm formId _ ->
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

                        SaveFamilyPlanningSigns _ ->
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
                |> EveryDictList.keys
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


{-| Send new photo of a child to the backend.
-}
postPhoto : BackendUrl -> String -> ChildId -> ModelChild -> ( ModelChild, Cmd MsgChild )
postPhoto backendUrl accessToken childId model =
    -- TODO: Re-implement
    ( model, Cmd.none )



{-
   case model.photo of
       ( Nothing, _ ) ->
           -- This shouldn't happen, but in case we don't have a file ID, we won't issue
           -- a POST request.
           ( model, Cmd.none )

       ( Just fileId, _ ) ->
           let
               command =
                   HttpBuilder.post (backendUrl ++ "/api/photos")
                       |> withQueryParams [ ( "access_token", accessToken ) ]
                       -- TODO: Fix up types to avoid `toEntityUuid`
                       |> withJsonBody (encodePhoto (toEntityUuid childId) fileId)
                       |> sendWithHandler decodePhotoFromResponse HandlePhotoSave
           in
               ( { model | status = Loading }
               , command
               )
-}
