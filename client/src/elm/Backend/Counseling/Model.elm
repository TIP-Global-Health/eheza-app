module Backend.Counseling.Model exposing (..)

{-| This module represents the idea of CounselingTopics and a
CounselingSchedule. The CounselingSchedule records which topics are expected
at certain moments in the process.

Which topics were actually addressed is recorded by a `CounsellingSession`,
which is modeled in `Backed.Measurement.Model`, since it shares much in common
with the other measurements.

-}

import Backend.Entities exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)


{-| For the moment, the only information we track about the topic itself
is the text for the checkboxes, in both languages.
-}
type alias CounselingTopic =
    { english : String
    , kinyarwanda : Maybe String
    }


{-| The different stages counseling.
-}
type CounselingTiming
    = Entry
    | MidPoint
    | Exit


{-| A single counseling schedule entity, as sent from the backend.
-}
type alias CounselingSchedule =
    { timing : CounselingTiming
    , topics : EveryDictList CounselingTopicId CounselingTopic
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
    EveryDict CounselingTiming (EveryDictList CounselingTopicId CounselingTopic)
