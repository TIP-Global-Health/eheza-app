module Pages.GuideMessage.View exposing (..)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Time
import Translate exposing (Language, translate, translateText)


view : Language -> Time.Posix -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentTime nurseId nurse db model =
    let
        header =
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.GuideMessage
                    ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage <| UserPage WellbeingPage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
    div [ class "page-activity guideMessage" ]
        [ header
        , viewGuide language
        ]


viewGuide : Language -> Html Msg
viewGuide language =
    div [ class "guide" ]
        [ p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection1Title ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection1Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection1Bullet2 ]
            ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection2Title ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection2Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection2Bullet2 ]
            ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection3Title ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet3 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet4 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet5 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet6 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet7 ]
            ]
        , p [ class "title note" ] [ text <| translate language Translate.ResilienceGuideSection3Note ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection4Title ]
        , p [] [ text <| translate language Translate.ResilienceGuideSection4Text ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection6Title ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection5Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection5Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection5Bullet3 ]
            ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection6Title ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection6Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection6Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection6Bullet3 ]
            , li []
                [ text <| translate language Translate.ResilienceGuideSection6Bullet4
                , ul []
                    [ li [] [ text <| translate language Translate.ResilienceGuideSection6Bullet5 ]
                    , li [] [ text <| translate language Translate.ResilienceGuideSection6Bullet6 ]
                    , li [] [ text <| translate language Translate.ResilienceGuideSection6Bullet7 ]
                    ]
                ]
            ]
        , p [ class "title note" ] [ text <| translate language Translate.ResilienceGuideSection6Note ]
        ]
