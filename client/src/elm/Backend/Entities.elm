module Backend.Entities exposing (..)

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

What we do instead is have a "unityfing" `NodeId` type (from `Drupal.Restful`),
which takes what we call a "phantom" type variable -- a type variable that
isn't actually used for any data. This gives us all the type-safety we need at
compile time, but lets us have a single way of actually getting the `Int` when
we need it.

-}

import Drupal.Restful exposing (NodeId(..))


{-
   The rest of this are the phantom types for each entity we're using.
   This would benefit from some kind of code-generation step, since you
   could generate the code below easily from a list of base types.

   We create the type aliases so that we can just say

       ChildId

   most of the time, rather than the more verbose

       NodeId ChildId

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

   s/ *\(.*\)/type alias \1Id = NodeId \1IdType\rtype \1IdType=\1IdType/

   Then, just save it and let elm-format make it pretty.

   Eventually, you could imagine just having a file with a list
   of our entity types, and a little script to generate the code below.
-}


type alias ChildId =
    NodeId ChildIdType


type ChildIdType
    = ChildIdType


type alias ChildNutritionId =
    NodeId ChildNutritionIdType


type ChildNutritionIdType
    = ChildNutritionIdType


type alias ClinicId =
    NodeId ClinicIdType


type ClinicIdType
    = ClinicIdType


type alias FamilyPlanningId =
    NodeId FamilyPlanningIdType


type FamilyPlanningIdType
    = FamilyPlanningIdType


type alias HeightId =
    NodeId HeightIdType


type HeightIdType
    = HeightIdType


type alias MotherId =
    NodeId MotherIdType


type MotherIdType
    = MotherIdType


type alias MuacId =
    NodeId MuacIdType


type MuacIdType
    = MuacIdType


type alias PhotoId =
    NodeId PhotoIdType


type PhotoIdType
    = PhotoIdType


type alias SessionId =
    NodeId SessionIdType


type SessionIdType
    = SessionIdType


type alias WeightId =
    NodeId WeightIdType


type WeightIdType
    = WeightIdType
