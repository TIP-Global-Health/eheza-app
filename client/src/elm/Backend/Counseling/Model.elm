module Backend.Counseling.Model exposing (CounselingSchedule, CounselingTiming(..), CounselingTopic, EveryCounselingSchedule)

{-| This module represents the idea of CounselingTopics and a
CounselingSchedule. The CounselingSchedule records which topics are expected
at certain moments in the process.

Which topics were actually addressed is recorded by a `CounsellingSession`,
which is modeled in `Backed.Measurement.Model`, since it shares much in common
with the other measurements.

-}

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Translate.Model exposing (TranslationSet)


{-| For the moment, the only information we track about the topic itself
is the text for the checkboxes, in both languages.
-}
type alias CounselingTopic =
    TranslationSet String


{-| The different stages for counseling sessions.

The `Before...` stage is the stage at which we warn the mother that the next
session will have that kind of counseling session. It fits quite nicely into
the UI if we just conceive of that as a counseling session itself. (That is, it
is less work than making it something special). And, it makes sense from a
data-modeling point of view, because these are mutuall exclusive possibilities
(we'd never show more than one of these in a session).

-}
type CounselingTiming
    = Entry
    | BeforeMidpoint
    | MidPoint
    | BeforeExit
    | Exit


{-| A single counseling schedule entity.
-}
type alias CounselingSchedule =
    { timing : CounselingTiming
    , topics : List CounselingTopicId
    }


{-| This combines all the `CounselingSchedule` entities we receive from the
backend into one dictionary, where the keys are the timing, and the values are
a dict-list of topicIDs and topics.

(We're not really interested in the CounselingSchedulieId itself, because we're
not modifying the schedule on the client, at least for now.

We won't plan to have more than one `CounselingSchedule` entity on the backend
for each `CounselingTiming`. If it turns out that we do, we'll just combine
them on the client.

-}
type alias EveryCounselingSchedule =
    Dict CounselingTiming (Dict CounselingTopicId CounselingTopic)
