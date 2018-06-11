module Backend.Session.Form exposing (..)

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
import Restful.Endpoint exposing (toEntityId)
import Translate exposing (ValidationError(..))
import Utils.NominalDate exposing (validateNominalDateRange, setNominalDateRange)


type alias SessionForm =
    Form ValidationError Session



-- Field names, so the compiler can help us a bit with typos


clinicId : String
clinicId =
    "clinic_id"


closed : String
closed =
    "closed"


training : String
training =
    "training"


scheduledDate : String
scheduledDate =
    "scheduled_date"


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
    int
        |> map toEntityId
        |> andThen
            (\id ->
                if knownClinic id then
                    succeed id
                else
                    fail (customError UnknownClinic)
            )
