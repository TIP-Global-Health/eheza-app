module Backend.HomeVisitEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Encoder exposing (encodeHomeVisitEncounter)
import Backend.HomeVisitEncounter.Model exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> HomeVisitEncounterId -> Maybe HomeVisitEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseHomeVisitEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeHomeVisitEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull homeVisitEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedHomeVisitEncounter)
                        )
                    )

        HandleClosedHomeVisitEncounter data ->
            ( { model | closeHomeVisitEncounter = data }
            , Cmd.none
            )

        SaveFeeding personId valueId value ->
            ( { model | saveFeeding = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionFeedingEndpoint HandleSavedFeeding
            )

        HandleSavedFeeding data ->
            ( { model | saveFeeding = data }
            , Cmd.none
            )

        SaveHygiene personId valueId value ->
            ( { model | saveHygiene = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionHygieneEndpoint HandleSavedHygiene
            )

        HandleSavedHygiene data ->
            ( { model | saveHygiene = data }
            , Cmd.none
            )

        SaveFoodSecurity personId valueId value ->
            ( { model | saveFoodSecurity = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionFoodSecurityEndpoint HandleSavedFoodSecurity
            )

        HandleSavedFoodSecurity data ->
            ( { model | saveFoodSecurity = data }
            , Cmd.none
            )
