module Pages.WellBeing.View exposing (..)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole(..))
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Time exposing (posixToMillis)
import Translate exposing (Language, TranslationId, translate, translateText)


view : Language -> Time.Posix -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentTime nurseId nurse db model =
    let
        currentDate =
            fromLocalDateTime currentTime

        numberOfUnreadMessages =
            resolveNumberOfUnreadMessages currentTime currentDate nurseId nurse db

        header =
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.Wellbeing
                    ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage PinCodePage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]

        resilienceMessageButton =
            button
                [ class "ui primary button"
                , onClick <| SetActivePage <| UserPage MessagingCenterPage
                ]
                [ span [ class "text" ] [ text <| translate language Translate.ResilienceMessage ]
                , span [ class "counter" ] [ text <| String.fromInt numberOfUnreadMessages ]
                ]
    in
    div [ class "page-activity well-being" ]
        [ header
        , resilienceMessageButton
        ]
