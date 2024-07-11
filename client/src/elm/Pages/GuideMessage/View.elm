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
        , p [ class "note" ] [ text <| translate language Translate.ResilienceGuideSection1TitleNote1 ]
        , p [] [ text <| translate language Translate.ResilienceGuideSection1TitleNote2 ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection1Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection1Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection1Bullet3 ]
            ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection2Title ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection2Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection2Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection2Bullet3 ]
            ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection3Title ]
        , p [] [ text <| translate language Translate.ResilienceGuideSection3Note ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection3Bullet3 ]
            ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceGuideSection4Title ]
        , p [] [ text <| translate language Translate.ResilienceGuideSection4Text ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceGuideSection4Bullet1 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection4Bullet2 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection4Bullet3 ]
            , li [] [ text <| translate language Translate.ResilienceGuideSection4Bullet4 ]
            ]
        ]
