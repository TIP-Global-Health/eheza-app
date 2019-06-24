module Backend.Entities exposing (AttendanceId, AttendanceUuidType(..), CatchmentAreaId, CatchmentAreaUuidType(..), ChildNutritionId, ChildNutritionUuidType(..), ClinicId, ClinicUuidType(..), CounselingScheduleId, CounselingScheduleUuidType(..), CounselingSessionId, CounselingSessionUuidType(..), CounselingTopicId, CounselingTopicUuidType(..), FamilyPlanningId, FamilyPlanningUuidType(..), HealthCenterId, HealthCenterUuidType(..), HeightId, HeightUuidType(..), MuacId, MuacUuidType(..), NurseId, NurseUuidType(..), ParticipantConsentId, ParticipantConsentUuidType(..), ParticipantFormId, ParticipantFormUuidType(..), PersonId, PersonUuidType(..), PhotoId, PhotoUuidType(..), PmtctParticipantId, PmtctParticipantUuidType(..), PrenatalEncounterId(..), PrenatalEncounterIdType(..), PrenatalParticipantId, PrenatalParticipantIdType(..), RelationshipId, RelationshipUuidType(..), SessionId, SessionUuidType(..), WeightId, WeightUuidType(..))

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

import Restful.Endpoint exposing (EntityUuid(..))



{-
    The rest of this are the phantom types for each entity we're using.
    This would benefit from some kind of code-generation step, since you
    could generate the code below easily from a list of base types.

    We create the type aliases so that we can just say

        ChildId

    most of the time, rather than the more verbose

        EntityUuid ChildId

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


type alias CatchmentAreaId =
    EntityUuid CatchmentAreaUuidType


type CatchmentAreaUuidType
    = CatchmentAreaUuidType


type alias ChildNutritionId =
    EntityUuid ChildNutritionUuidType


type ChildNutritionUuidType
    = ChildNutritionUuidType


type alias ClinicId =
    EntityUuid ClinicUuidType


type ClinicUuidType
    = ClinicUuidType


type alias CounselingScheduleId =
    EntityUuid CounselingScheduleUuidType


type CounselingScheduleUuidType
    = CounselingScheduleUuidType


type alias CounselingSessionId =
    EntityUuid CounselingSessionUuidType


type CounselingSessionUuidType
    = CounselingSessionUuidType


type alias CounselingTopicId =
    EntityUuid CounselingTopicUuidType


type CounselingTopicUuidType
    = CounselingTopicUuidType


type alias AttendanceId =
    EntityUuid AttendanceUuidType


type AttendanceUuidType
    = AttendanceUuidType


type alias FamilyPlanningId =
    EntityUuid FamilyPlanningUuidType


type FamilyPlanningUuidType
    = FamilyPlanningUuidType


type alias HealthCenterId =
    EntityUuid HealthCenterUuidType


type HealthCenterUuidType
    = HealthCenterUuidType


type alias HeightId =
    EntityUuid HeightUuidType


type HeightUuidType
    = HeightUuidType


type alias MuacId =
    EntityUuid MuacUuidType


type MuacUuidType
    = MuacUuidType


type alias NurseId =
    EntityUuid NurseUuidType


type NurseUuidType
    = NurseUuidType


type alias ParticipantConsentId =
    EntityUuid ParticipantConsentUuidType


type ParticipantConsentUuidType
    = ParticipantConsentUuidType


type alias ParticipantFormId =
    EntityUuid ParticipantFormUuidType


type ParticipantFormUuidType
    = ParticipantFormUuidType


type alias PersonId =
    EntityUuid PersonUuidType


type PersonUuidType
    = PersonUuidType


type alias PhotoId =
    EntityUuid PhotoUuidType


type PhotoUuidType
    = PhotoUuidType


type alias PrenatalParticipantId =
    EntityUuid PrenatalParticipantIdType


type PrenatalParticipantIdType
    = PrenatalParticipantIdType


type PrenatalEncounterId
    = EntityUuid PrenatalEncounterIdType


type PrenatalEncounterIdType
    = PrenatalEncounterIdType


type alias PmtctParticipantId =
    EntityUuid PmtctParticipantUuidType


type PmtctParticipantUuidType
    = PmtctParticipantUuidType


type alias RelationshipId =
    EntityUuid RelationshipUuidType


type RelationshipUuidType
    = RelationshipUuidType


type alias SessionId =
    EntityUuid SessionUuidType


type SessionUuidType
    = SessionUuidType


type alias WeightId =
    EntityUuid WeightUuidType


type WeightUuidType
    = WeightUuidType
