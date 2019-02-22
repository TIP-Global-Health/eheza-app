module Backend.Session.Update exposing (update)

import Backend.Endpoints exposing (sessionEndpoint)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Backend.Session.Encoder exposing (..)
import Backend.Session.Model exposing (..)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, toCmd, withoutDecoder)


update : SessionId -> Msg -> Model -> ( Model, Cmd Msg )
update sessionId msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        CloseSession ->
            ( { model | closeSessionRequest = Loading }
            , object [ encodeClosed True ]
                |> sw.patchAny sessionEndpoint sessionId
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleClosedSession)
            )

        HandleClosedSession data ->
            ( { model | closeSessionRequest = data }
            , Cmd.none
            )

        MeasurementOutMsgChild childId subMsg ->
            ( model, Cmd.none )

        MeasurementOutMsgMother motherId subMsg ->
            ( model, Cmd.none )



{- We reach this when the user hits "Save" upon editing something in the measurement
   form. So, we want to change the appropriate edit ...
-}
{-
   makeChildEdit : NominalDate -> ChildId -> OutMsgChild -> SessionId -> EditableSession -> EditableSession
   makeChildEdit currentDate childId outMsg sessionId session =
       -- Clearly, there will be a function that could be abstracted to make
       -- this less verbose, but I shall leave that for the future.
       let
           data =
               getChildMeasurementData childId session
       in
       case outMsg of
           SaveHeight height ->
               let
                   backend =
                       mapMeasurementData .height .height data
                           |> backendValue

                   edit =
                       case backend of
                           -- TODO: Could do a comparison to possibly return to `Unedited`
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , id = id
                                   , edited = { value | value = height }
                                   }

                           Nothing ->
                               Created
                                   { participantId = childId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = height
                                   }
               in
               mapChildEdits (\edits -> { edits | height = edit }) childId session

           SaveWeight weight ->
               let
                   backend =
                       mapMeasurementData .weight .weight data
                           |> backendValue

                   edit =
                       case backend of
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , id = id
                                   , edited = { value | value = weight }
                                   }

                           Nothing ->
                               Created
                                   { participantId = childId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = weight
                                   }
               in
               mapChildEdits (\edits -> { edits | weight = edit }) childId session

           SaveMuac muac ->
               let
                   backend =
                       mapMeasurementData .muac .muac data
                           |> backendValue

                   edit =
                       case backend of
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , id = id
                                   , edited = { value | value = muac }
                                   }

                           Nothing ->
                               Created
                                   { participantId = childId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = muac
                                   }
               in
               mapChildEdits (\edits -> { edits | muac = edit }) childId session

           SaveCounselingSession timing topics ->
               let
                   backend =
                       mapMeasurementData .counselingSession .counseling data
                           |> backendValue

                   edit =
                       case backend of
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , id = id
                                   , edited = { value | value = ( timing, topics ) }
                                   }

                           Nothing ->
                               Created
                                   { participantId = childId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = ( timing, topics )
                                   }
               in
               mapChildEdits (\edits -> { edits | counseling = edit }) childId session

           SaveChildNutritionSigns nutrition ->
               let
                   backend =
                       mapMeasurementData .nutrition .nutrition data
                           |> backendValue

                   edit =
                       case backend of
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , id = id
                                   , edited = { value | value = nutrition }
                                   }

                           Nothing ->
                               Created
                                   { participantId = childId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = nutrition
                                   }
               in
               mapChildEdits (\edits -> { edits | nutrition = edit }) childId session

           SavePhoto photo ->
               let
                   backend =
                       mapMeasurementData .photo .photo data
                           |> backendValue

                   edit =
                       case backend of
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , edited = { value | value = photo }
                                   , id = id
                                   }

                           Nothing ->
                               Created
                                   { participantId = childId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = photo
                                   }
               in
               mapChildEdits (\edits -> { edits | photo = edit }) childId session
-}
{- We reach this when the user hits "Save" upon editing something in the measurement
   form. So, we want to change the appropriate edit ...
-}
{-
   makeMotherEdit : Maybe NurseId -> NominalDate -> MotherId -> OutMsgMother -> SessionId -> EditableSession -> EditableSession
   makeMotherEdit user currentDate motherId outMsg sessionId session =
       let
           data =
               getMotherMeasurementData motherId session
       in
       case outMsg of
           SaveFamilyPlanningSigns signs ->
               let
                   backend =
                       mapMeasurementData .familyPlanning .familyPlanning data
                           |> backendValue

                   edit =
                       case backend of
                           Just ( id, value ) ->
                               Edited
                                   { backend = value
                                   , id = id
                                   , edited = { value | value = signs }
                                   }

                           Nothing ->
                               Created
                                   { participantId = motherId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value = signs
                                   }
               in
               mapMotherEdits (\edits -> { edits | familyPlanning = edit }) motherId session

           SaveCompletedForm formId language ->
               -- In this case, so far, we don't allow for updates. So, for the
               -- moment, the only things we have to do is create things. Also,
               -- we necessarily will have a current user in this case, though
               -- we'd need to restructure to convince the compiler of that.
               case user of
                   Just userId ->
                       let
                           edit =
                               Created
                                   { participantId = motherId
                                   , sessionId = Just sessionId
                                   , dateMeasured = currentDate
                                   , value =
                                       { formId = formId
                                       , language = language
                                       }
                                   }
                       in
                       mapMotherEdits
                           (\edits -> { edits | consent = edit :: edits.consent })
                           motherId
                           session

                   Nothing ->
                       session
-}
