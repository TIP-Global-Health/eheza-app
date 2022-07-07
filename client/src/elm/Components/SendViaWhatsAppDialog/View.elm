module Components.SendViaWhatsAppDialog.View exposing (view)

-- import App.Model exposing (Msg(..))

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Components.SendViaWhatsAppDialog.Model exposing (..)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Utils exposing (viewTextInput)
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> ( PersonId, Person ) -> Model -> Html Msg
view language currentDate ( personId, person ) model =
    viewModal <|
        Maybe.map (viewDialog language currentDate ( personId, person )) model.state


viewDialog : Language -> NominalDate -> ( PersonId, Person ) -> DialogState -> Html Msg
viewDialog language currentDate ( personId, person ) state =
    let
        content =
            case state of
                Consent ->
                    viewConsent language currentDate person

                PhoneVerification phoneNumber ->
                    viewPhoneVerification language currentDate phoneNumber

                PhoneInput inputValue ->
                    viewPhoneInput language currentDate inputValue
    in
    div [ class "ui tiny active modal send-via-whatsapp" ]
        content


viewConsent : Language -> NominalDate -> Person -> List (Html Msg)
viewConsent language currentDate person =
    let
        nextState =
            Maybe.map
                (\number ->
                    if not <| String.isEmpty number then
                        PhoneVerification number

                    else
                        PhoneInput ""
                )
                person.telephoneNumber
                |> Maybe.withDefault (PhoneInput "")
    in
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppConsentQuestion ]
        , p [] [ text <| translate language Translate.SendViaWhatsAppNoticeOfNonRespobsibility ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState Nothing
                ]
                [ text <| translate language Translate.No ]
            , button
                [ class "ui primary fluid button"
                , onClick <| SetState <| Just nextState
                ]
                [ text <| translate language Translate.Yes ]
            ]
        ]
    ]


viewPhoneVerification : Language -> NominalDate -> String -> List (Html Msg)
viewPhoneVerification language currentDate phoneNumber =
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppPhoneVerificationHeader ]
        , p [] [ text phoneNumber ]
        , p [] [ text <| translate language Translate.SendViaWhatsAppPhoneVerificationQuestion ]
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState <| Just (PhoneInput "")
                ]
                [ text <| translate language Translate.No ]
            , button
                [ class "ui primary fluid button"

                -- , onClick <| SetState <| Just nextState
                ]
                [ text <| translate language Translate.Yes ]
            ]
        ]
    ]


viewPhoneInput : Language -> NominalDate -> String -> List (Html Msg)
viewPhoneInput language currentDate inputValue =
    [ div [ class "content" ]
        [ p [] [ text <| translate language Translate.SendViaWhatsAppPhoneInputHeader ]
        , viewTextInput language inputValue SetInputNumber Nothing (Just "phone-number")
        ]
    , div [ class "actions" ]
        [ div [ class "two ui buttons" ]
            [ button
                [ class "ui velvet fluid button"
                , onClick <| SetState Nothing
                ]
                [ text <| translate language Translate.Cancel ]
            , button
                [ class "ui primary fluid button"

                -- , onClick <| SetState <| Just nextState
                ]
                [ text <| translate language Translate.Continue ]
            ]
        ]
    ]
