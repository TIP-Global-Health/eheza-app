module Backend.Entities exposing (ChildId, ChildIdType(..), ChildNutritionId, ChildNutritionIdType(..), ClinicId, ClinicIdType(..), FamilyPlanningId, FamilyPlanningIdType(..), GeoLocationId, GeoLocationIdType(..), HeightId, HeightIdType(..), MotherId, MotherIdType(..), MuacId, MuacIdType(..), PhotoId, PhotoIdType(..), SessionId, SessionIdType(..), WeightId, WeightIdType(..))

{-|


## Why are all the ID types here?

It's nice to have type-safe IDs for backend entities, but it tends
to lead to circular imports if you put the ID types in the "usual"
place alongisde the data-type itself.

One typical case where the cirular references arise is where a "child"
entity has a reference to its "parent". So, for instance:

  - the various measurements have a `sessionId` to refer to the session the
    measurement was taken in.

  - but the `OfflineSession` also has a `DictList` of all its measurmeents

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

import Restful.Endpoint exposing (EntityId(..))



{-
    The rest of this are the phantom types for each entity we're using.
    This would benefit from some kind of code-generation step, since you
    could generate the code below easily from a list of base types.

    We create the type aliases so that we can just say

        ChildId

    most of the time, rather than the more verbose

        EntityId ChildId

    In fact, here's a simple code-generation process which you can adapt
    to your preferred code-editor. Here's the list of base types:

        Child
        ChildNutrition
        Clinic
        FamilyPlanning
        Height
        Mother
        Muac
        Photo
        Session
        Weight

    Now, to create a new one, use the following process:

    1. Add it to the list above.
    2. Delete everything below this comment.
    3. Copy the list above below this comment.
    4. Use search-and-replace to generate the code.

    For vim, here's the pattern I use:

    s/ *\(.*\)/type alias \1Id = EntityId \1IdType\rtype \1IdType=\1IdType/

    Then, just save it and let elm-format make it pretty.

    Eventually, you could imagine just having a file with a list
    of our entity types, and a little script to generate the code below.

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
