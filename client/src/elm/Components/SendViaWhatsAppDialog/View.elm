module Components.SendViaWhatsAppDialog.View exposing (view)

-- import App.Model exposing (Msg(..))

import Backend.Entities exposing (..)
import Components.SendViaWhatsAppDialog.Model exposing (..)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> Model -> Html Msg
view language currentDate model =
    viewModal <|
        Maybe.map (viewDialog language currentDate) model.state


viewDialog : Language -> NominalDate -> DialogState -> Html Msg
viewDialog language currentDate state =
    div [ class "ui tiny active modal send-via-whatsapp" ]
        [ div [ class "content" ]
            [ p [] [ text <| translate language Translate.SendViaWhatsAppConsentQuestion ]
            , p [] [ text <| translate language Translate.SendViaWhatsAppNoticeOfNonRespobsibility ]
            ]
        , div
            [ class "actions" ]
            [ div [ class "two ui buttons" ]
                [ button
                    [ class "ui velvet fluid button"
                    , onClick <| SetState Nothing
                    ]
                    [ text <| translate language Translate.No ]
                , button
                    [ class "ui primary fluid button"

                    -- , onClick confirmAction
                    ]
                    [ text <| translate language Translate.Yes ]
                ]
            ]
        ]
