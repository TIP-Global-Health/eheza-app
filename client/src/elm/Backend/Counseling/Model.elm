module Backend.Counseling.Model exposing (..)

{-| This module represents the idea of CounselingTopics and a
CounselingSchedule. The CounselingSchedule records which topics are expected
at certain moments in the process.

Which topics were actually addressed is recorded by a `CounsellingSession`,
which is modeled in `Backed.Measurement.Model`, since it shares much in common
with the other measurements.

-}

import Backend.Entities exposing (..)


{-| For the moment, the only information we track about the topic itself
is the text for the checkboxes, in both languages.
-}
type alias CounselingTopic =
    { english : String
    , kinyarwanda : String
    }


{-| The different stages counseling.
-}
type CounselingTiming
    = Entry
    | MidPoint
    | Exit


{-| Reflects a scheduling object from the backend.
-}
type alias CounselingSchedule =
    { timing : CounselingTiming
    , topics : List CounselingTopicId
    }
