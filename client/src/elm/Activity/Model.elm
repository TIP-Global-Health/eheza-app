module Activity.Model exposing (Activity(..), ChildActivity(..), CompletedAndPending, MotherActivity(..), SummaryByActivity, SummaryByParticipant, emptySummaryByActivity, emptySummaryByParticipant)

{-| This module provides types relating to the UI for presenting activities.

So, we end up with values that represent one activity or another. In fact,
these more or less represent a data-level proxy for the various types in
`Backend.Measurement.Model`.

Basically, this represents a way to take our "basic" data and organize it
in a way that is suitable for the UI we want to present.

-}

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)


type Activity
    = ChildActivity ChildActivity
    | MotherActivity MotherActivity


type ChildActivity
    = ChildFbf
    | ChildPicture --| Counseling
    | ContributingFactors
    | FollowUp
    | HealthEducation
    | Height
    | Muac
    | NutritionSigns
    | SendToHC
    | Weight
    | NCDA


{-| So far, it seems simpler not to have a separate `CaregiverActivityType`.
Once we have some caregiver activities, they are very likely to be a subset
of the mother activities, rather than an entirely different type. Also, we
show mothers and caregivers in very similar ways in the UI.
-}
type MotherActivity
    = FamilyPlanning
    | Lactation
    | MotherFbf
    | ParticipantConsent


{-| This is basically a tuple, but it's nice to have meaningful names for the
fields. We parameterize because sometimes we are tracking which activities are
completed/pending (for a participant), and sometimes tracking which
participants are completed/pending (for an activity).

Note that whether an activity is considered pending for a participant
depends on whether that activity is expected for the participant or not.
For instance, "caregivers" currently have no expected activities (so
nothing will be pending or completed). And, counseling activities for
children will depend on the date of the session, so for some children
they will neither pending nor completed.

-}
type alias CompletedAndPending value =
    { completed : value
    , pending : value
    }


{-| This type summarizes, for each activity, which participants have
completed the activity and which participants are pending. Essentially,
this converts from our "basic" facts to facts that are organized in
a way that is more useful for the UI we present.
-}
type alias SummaryByActivity =
    { children : Dict ChildActivity (CompletedAndPending (Dict PersonId Person))
    , mothers : Dict MotherActivity (CompletedAndPending (Dict PersonId Person))
    }


emptySummaryByActivity : SummaryByActivity
emptySummaryByActivity =
    { children = Dict.empty
    , mothers = Dict.empty
    }


{-| Like SummaryByActivity, but organized by Participant instead.
So, for each participant, what activities have been completed,
and what activities are still pending?
-}
type alias SummaryByParticipant =
    { children : Dict PersonId (CompletedAndPending (List ChildActivity))
    , mothers : Dict PersonId (CompletedAndPending (List MotherActivity))
    }


emptySummaryByParticipant : SummaryByParticipant
emptySummaryByParticipant =
    { children = Dict.empty
    , mothers = Dict.empty
    }
