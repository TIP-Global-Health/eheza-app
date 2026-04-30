module Backend.ParticipantConsent.Encoder exposing (encodeParticipantForm)

import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Json.Encode exposing (Value, bool, string)
import Json.Encode.Extra exposing (maybe)


encodeParticipantForm : ParticipantForm -> List ( String, Value )
encodeParticipantForm form =
    [ ( "label", string form.title.english )
    , ( "kinyarwanda_title", maybe string form.title.kinyarwanda )
    , ( "body", string form.body.english.raw )
    , ( "kinyarwanda_body", maybe string (Maybe.map .raw form.body.kinyarwanda) )
    , ( "deleted", bool False )
    , ( "type", string "participant_form" )
    ]
