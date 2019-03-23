module Backend.Session.Form exposing (SessionForm, clinicId, clinicIdState, closed, closedState, emptyForm, scheduledDate, scheduledDateState, training, trainingState, validateClinicId, validateSession)

{-| These are things for `Session` forms which won't vary from one view to
another. So, they may as well be defined here.

Anything which varies from one view to another can be defined under `Pages`.

-}

import Backend.Entities exposing (..)
import Backend.Session.Model exposing (..)
import Form exposing (..)
import Form.Init exposing (..)
import Form.Validate exposing (..)
import Gizra.NominalDate exposing (NominalDateRange)
import Restful.Endpoint exposing (toEntityUuid)
import Translate exposing (ValidationError(..))
import Utils.NominalDate exposing (setNominalDateRange, validateNominalDateRange)


type alias SessionForm =
    Form ValidationError Session



-- Field names, so the compiler can help us a bit with typos


clinicId : String
clinicId =
    "clinic_id"


clinicIdState : SessionForm -> FieldState ValidationError String
clinicIdState =
    getFieldAsString clinicId


closed : String
closed =
    "closed"


closedState : SessionForm -> FieldState ValidationError Bool
closedState =
    getFieldAsBool closed


training : String
training =
    "training"


trainingState : SessionForm -> FieldState ValidationError Bool
trainingState =
    getFieldAsBool training


scheduledDate : String
scheduledDate =
    "scheduled_date"


scheduledDateState : SessionForm -> { start : FieldState ValidationError String, end : FieldState ValidationError String }
scheduledDateState form =
    -- Should probably split this out into Utils.NominalDate
    { start = getFieldAsString (scheduledDate ++ ".start") form
    , end = getFieldAsString (scheduledDate ++ ".end") form
    }


{-| An empty session form, i.e. for creation.
-}
emptyForm : (ClinicId -> Bool) -> NominalDateRange -> SessionForm
emptyForm knownClinic initialDates =
    -- No default for `clinic_id`.
    Form.initial
        [ setBool closed False
        , setBool training False
        , setNominalDateRange scheduledDate initialDates
        ]
        (validateSession knownClinic)


{-| To validate a session, we need to know whether the clinicID is valid.
That depends on the real world, so we ask for a function to determine
that.

For field names, we mostly track the JSON decoder.

-}
validateSession : (ClinicId -> Bool) -> Validation ValidationError Session
validateSession knownClinic =
    succeed Session
        |> andMap (field scheduledDate validateNominalDateRange)
        |> andMap (field clinicId (validateClinicId knownClinic))
        |> andMap (field closed bool)
        |> andMap (field training bool)


validateClinicId : (ClinicId -> Bool) -> Validation ValidationError ClinicId
validateClinicId knownClinic =
    string
        |> map toEntityUuid
        |> andThen
            (\id ->
                if knownClinic id then
                    succeed id

                else
                    fail (customError UnknownPMTCTGroup)
            )
