module Pages.MessagingConsent.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole(..))
import Backend.ResilienceMessage.Model exposing (ReasonForNotConsenting(..), ResilienceCategory(..), ResilienceMessage, ResilienceMessageOrder(..))
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBoolInput, viewCheckBoxSelectInput, viewQuestionLabel, viewSaveAction)
import Time
import Translate exposing (Language, translate, translateText)


view : Language -> Time.Posix -> NurseId -> Nurse -> Model -> Html Msg
view language currentTime nurseId nurse model =
    let
        currentDate =
            fromLocalDateTime currentTime

        numberOfUnreadMessages =
            resolveNumberOfUnreadMessages currentTime currentDate nurse

        header =
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.ResilienceMessage
                    , span [ class "counter" ] [ text <| String.fromInt numberOfUnreadMessages ]
                    ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage <| UserPage WellbeingPage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
    div [ class "page-activity messaging-consent" ]
        [ header
        , viewConsentForm language nurseId nurse model.consentForm
        ]


viewConsentForm : Language -> NurseId -> Nurse -> ConsentForm -> Html Msg
viewConsentForm language nurseId nurse form =
    div [ class "consent" ]
        [ p [ class "title" ] [ text <| translate language Translate.ResilienceConsentTitle ]
        , p [ class "title" ] [ text <| translate language Translate.ResilienceConsentSubTitle ]
        , p [ class "greeting" ] [ text <| translate language Translate.ResilienceConsentGreeting ]
        , p [] [ text <| translate language Translate.ResilienceConsentParagraph1 ]
        , ul []
            [ li [] [ text <| translate language Translate.ResilienceConsentBullet1 ]
            , li [] [ text <| translate language Translate.ResilienceConsentBullet2 ]
            , li [] [ text <| translate language Translate.ResilienceConsentBullet3 ]
            , li [] [ text <| translate language Translate.ResilienceConsentBullet4 ]
            , li [] [ text <| translate language Translate.ResilienceConsentBullet5 ]
            ]
        , p [] [ text <| translate language Translate.ResilienceConsentParagraph2 ]
        , p [] [ text <| translate language Translate.ResilienceConsentParagraph3 ]
        , p [] [ text <| translate language Translate.ResilienceConsentParagraph4 ]
        , p [ class "greeting" ] [ text <| translate language Translate.ResilienceConsentSubTitle2 ]
        , p [] [ text <| translate language Translate.ResilienceConsentParagraph5 ]
        , div [ class "full content" ]
            [ div [ class "ui form" ]
                [ viewQuestionLabel language Translate.ResilienceConsentQuestion
                , viewBoolInput
                    language
                    form.agreesToParticipate
                    SetConsentAgree
                    "consent-agree"
                    Nothing
                ]
            , case form.agreesToParticipate of
                Just False ->
                    div [ class "ui form" ]
                        [ viewQuestionLabel language Translate.WhyNot
                        , viewCheckBoxSelectInput language
                            [ ManyOtherCommitments
                            , NoDedicatedTimeForTheProgram
                            , ProgramNotAddressingMyStressors
                            , DontWantToBeSeenAsStruggling
                            , TriedSimilarProgramBefore
                            , NotInterestedInProgram
                            ]
                            []
                            form.reasonsToNotConsent
                            SelectConsentReason
                            Translate.ReasonForNotConsenting
                        ]

                _ ->
                    emptyNode
            ]
        , viewSaveAction
            language
            (SaveConsent nurseId nurse)
            (form.agreesToParticipate == Nothing)
        ]
