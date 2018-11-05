module Backend.ParticipantConsent.Encoder exposing (encodeParticipantForm)

import Backend.ParticipantConsent.Model exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)


encodeParticipantForm : ParticipantForm -> List ( String, Value )
encodeParticipantForm form =
    [ ( "label", string form.title.english )
    , ( "kinyarwanda_title", maybe string form.title.kinyarwanda )
    , ( "body", object [ ( "value", string form.body.english.raw ) ] )
    , ( "kinyarwanda_body", object [ ( "value", maybe string (Maybe.map .raw form.body.kinyarwanda) ) ] )
    ]
