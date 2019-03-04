module Backend.Entities exposing (CatchmentAreaUuid, CatchmentAreaUuidType(..), ChildId, ChildIdType(..), ChildNutritionId, ChildNutritionIdType(..), ChildNutritionUuid, ChildNutritionUuidType(..), ChildUuid, ChildUuidType(..), ClinicId, ClinicIdType(..), ClinicUuid, ClinicUuidType(..), CounselingScheduleId, CounselingScheduleIdType(..), CounselingScheduleUuid, CounselingScheduleUuidType(..), CounselingSessionId, CounselingSessionIdType(..), CounselingSessionUuid, CounselingSessionUuidType(..), CounselingTopicId, CounselingTopicIdType(..), CounselingTopicUuid, CounselingTopicUuidType(..), FamilyPlanningId, FamilyPlanningIdType(..), FamilyPlanningUuid, FamilyPlanningUuidType(..), GeoLocationId, GeoLocationIdType(..), HealthCenterUuid, HealthCenterUuidType(..), HeightId, HeightIdType(..), HeightUuid, HeightUuidType(..), MotherId, MotherIdType(..), MotherUuid, MotherUuidType(..), MuacId, MuacIdType(..), MuacUuid, MuacUuidType(..), NurseUuid, NurseUuidType(..), ParticipantConsentId, ParticipantConsentIdType(..), ParticipantConsentUuid, ParticipantConsentUuidType(..), ParticipantFormId, ParticipantFormIdType(..), ParticipantFormUuid, ParticipantFormUuidType(..), PhotoId, PhotoIdType(..), PhotoUuid, PhotoUuidType(..), SessionId, SessionIdType(..), SessionUuid, SessionUuidType(..), UserId, UserIdType(..), WeightId, WeightIdType(..), WeightUuid, WeightUuidType(..))

{-|


## Why are all the ID types here?

It's nice to have type-safe IDs for backend entities, but it tends
to lead to circular imports if you put the ID types in the "usual"
place alongside the data-type itself.

One typical case where the circular references arise is where a "child"
entity has a reference to its "parent". So, for instance:

  - the various measurements have a `sessionId` to refer to the session the
    measurement was taken in.

  - but the `OfflineSession` also has a `DictList` of all its measurements

Now, you could imagine avoiding this in one way or another. For instance, you
could imagine not storing the `sessionId` in the measurement, but instead
tracking it separately. But that would be awkward in its own way -- that is, it
would be awkward if `Measurement.Model` couldn't refer to the `SessionId`,
since each of the measurements really does have one -- we get it from the
backend, and send it to the backend.

So, it seems simpler to just have one ID type here for each of our backend
entities.

The way this is implemented is inspired by
[johneshf/elm-tagged](http://package.elm-lang.org/packages/joneshf/elm-tagged/latest).
You might want to start with something like:

    type ChildId
        = ChildId Int

    type ChildNutritionId
        = ChildNutritionId Int

But then the type-checker doesn't actually know that these two types are related
in some way -- for instance, that both are IDs. For instance, to extract the
`Int` you have to do two different things.

What we do instead is have a "unityfing" `EntityId` type (from `Restful.Endpoint`),
which takes what we call a "phantom" type variable -- a type variable that
isn't actually used for any data. This gives us all the type-safety we need at
compile time, but lets us have a single way of actually getting the `Int` when
we need it.

-}

import Restful.Endpoint exposing (EntityId(..), EntityUuid(..))



{-
    The rest of this are the phantom types for each entity we're using.
    This would benefit from some kind of code-generation step, since you
    could generate the code below easily from a list of base types.

    We create the type aliases so that we can just say

        ChildId

    most of the time, rather than the more verbose

        EntityId ChildId

    There are some possibly-attractive variations on this.

    - In a way, it would be nice to do something like Yesod does, with
      a `Key Child` and `Value Child` ... that is, it would be nice if
     `Child` itself would be the phantom type, rather than `ChildIdType`.
     But then our circular import problem would be much worse -- we'd have
     to keep all the actual definitions of the various entity records in
     a single file, which doesn't seem desirable. (It is, in effect, what
     Yesod does, seeing as you define all the entities in one file, typically).

   - Another alternative would be to also explicitly tie the `Child` type in
     `Backend.Child.Model` to the phantom type here, so that we have a kind of
     association between the `ChildId` type and the `Child` type ... that
     is, both refer to the same phantom type. That is probably desirable,
     since it helps enforce that we're using the right kind of key with
     the right kind of value -- I'll play with that at some point and see
     how it can be made to work.

-}


type alias CatchmentAreaUuid =
    EntityUuid CatchmentAreaUuidType


type CatchmentAreaUuidType
    = CatchmentAreaUuidType


type alias ChildUuid =
    EntityUuid ChildUuidType


type ChildUuidType
    = ChildUuidType


type alias ChildNutritionUuid =
    EntityUuid ChildNutritionUuidType


type ChildNutritionUuidType
    = ChildNutritionUuidType


type alias ClinicUuid =
    EntityUuid ClinicUuidType


type ClinicUuidType
    = ClinicUuidType


type alias CounselingScheduleUuid =
    EntityUuid CounselingScheduleUuidType


type CounselingScheduleUuidType
    = CounselingScheduleUuidType


type alias CounselingSessionUuid =
    EntityUuid CounselingSessionUuidType


type CounselingSessionUuidType
    = CounselingSessionUuidType


type alias CounselingTopicUuid =
    EntityUuid CounselingTopicUuidType


type CounselingTopicUuidType
    = CounselingTopicUuidType


type alias FamilyPlanningUuid =
    EntityUuid FamilyPlanningUuidType


type FamilyPlanningUuidType
    = FamilyPlanningUuidType


type alias HealthCenterUuid =
    EntityUuid HealthCenterUuidType


type HealthCenterUuidType
    = HealthCenterUuidType


type alias HeightUuid =
    EntityUuid HeightUuidType


type HeightUuidType
    = HeightUuidType


type alias MotherUuid =
    EntityUuid MotherUuidType


type MotherUuidType
    = MotherUuidType


type alias MuacUuid =
    EntityUuid MuacUuidType


type MuacUuidType
    = MuacUuidType


type NurseUuidType
    = NurseUuidType


type alias NurseUuid =
    EntityUuid NurseUuidType


type alias ParticipantConsentUuid =
    EntityUuid ParticipantConsentUuidType


type ParticipantConsentUuidType
    = ParticipantConsentUuidType


type alias ParticipantFormUuid =
    EntityUuid ParticipantFormUuidType


type ParticipantFormUuidType
    = ParticipantFormUuidType


type alias PhotoUuid =
    EntityUuid PhotoUuidType


type PhotoUuidType
    = PhotoUuidType


type alias SessionUuid =
    EntityUuid SessionUuidType


type SessionUuidType
    = SessionUuidType


type alias WeightUuid =
    EntityUuid WeightUuidType


type WeightUuidType
    = WeightUuidType


type alias UserId =
    EntityId UserIdType


type UserIdType
    = UserIdType


type alias ChildId =
    EntityId ChildIdType


type ChildIdType
    = ChildIdType


type alias ChildNutritionId =
    EntityId ChildNutritionIdType


type ChildNutritionIdType
    = ChildNutritionIdType


type alias ClinicId =
    EntityId ClinicIdType


type ClinicIdType
    = ClinicIdType


type alias CounselingScheduleId =
    EntityId CounselingScheduleIdType


type CounselingScheduleIdType
    = CounselingScheduleIdType


type alias CounselingSessionId =
    EntityId CounselingSessionIdType


type CounselingSessionIdType
    = CounselingSessionIdType


type alias CounselingTopicId =
    EntityId CounselingTopicIdType


type CounselingTopicIdType
    = CounselingTopicIdType


type alias FamilyPlanningId =
    EntityId FamilyPlanningIdType


type FamilyPlanningIdType
    = FamilyPlanningIdType


type alias GeoLocationId =
    EntityId GeoLocationIdType


type GeoLocationIdType
    = GeoLocationIdType


type alias HeightId =
    EntityId HeightIdType


type HeightIdType
    = HeightIdType


type alias MotherId =
    EntityId MotherIdType


type MotherIdType
    = MotherIdType


type alias MuacId =
    EntityId MuacIdType


type MuacIdType
    = MuacIdType


type alias ParticipantConsentId =
    EntityId ParticipantConsentIdType


type ParticipantConsentIdType
    = ParticipantConsentIdType


type alias ParticipantFormId =
    EntityId ParticipantFormIdType


type ParticipantFormIdType
    = ParticipantFormIdType


type alias PhotoId =
    EntityId PhotoIdType


type PhotoIdType
    = PhotoIdType


type alias SessionId =
    EntityId SessionIdType


type SessionIdType
    = SessionIdType


type alias WeightId =
    EntityId WeightIdType


type WeightIdType
    = WeightIdType
