module Pages.PrenatalActivity.View exposing (view)

import AllDict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Date.Extra as Date exposing (Interval(Year))
import DateSelector.SelectorDropdown
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import PrenatalActivity.Model exposing (PrenatalActivity(..))
import PrenatalActivity.Utils exposing (getActivityIcon, getAllActivities)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityId)
import Time.Date exposing (date)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (script, tabItem, thumbnailImage, viewLoading)
import Utils.WebData exposing (viewError, viewWebData)


view : Language -> NominalDate -> PersonId -> PrenatalActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        content =
            AllDict.get id db.people
                |> unwrap
                    []
                    (RemoteData.toMaybe
                        >> unwrap
                            []
                            (\mother ->
                                [ div [ class "ui unstackable items" ] <|
                                    viewMotherAndMeasurements language currentDate mother
                                        ++ viewContent language currentDate id activity model
                                ]
                            )
                    )
    in
    div [ class "page-prenatal-activity" ] <|
        viewHeader language id activity
            :: content


viewHeader : Language -> PersonId -> PrenatalActivity -> Html Msg
viewHeader language motherId activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.PrenatalActivitiesTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> PersonId -> PrenatalActivity -> Model -> List (Html Msg)
viewContent language currentDate motherId activity model =
    case activity of
        PregnancyDating ->
            viewPregnancyDatingContent language currentDate motherId model.pregnancyDatingForm

        _ ->
            []


viewPregnancyDatingContent : Language -> NominalDate -> PersonId -> PregnancyDatingForm -> List (Html Msg)
viewPregnancyDatingContent language currentDate motherId form =
    let
        lmpRangeInput =
            option
                [ value ""
                , selected (form.lmpRange == Nothing)
                ]
                [ text "" ]
                :: ([ OneMonth, ThreeMonth, SixMonth ]
                        |> List.map
                            (\range ->
                                option
                                    [ value (encodeLmpRange range)
                                    , selected (form.lmpRange == Just range)
                                    ]
                                    [ text <| translate language <| Translate.LmpRange range ]
                            )
                   )
                |> select [ onInput SetLmpRange ]

        today =
            toLocalDateTime currentDate 0 0 0 0

        lmpDateInput =
            DateSelector.SelectorDropdown.view
                ToggleDateSelector
                SetLmpDate
                form.isDateSelectorOpen
                (Date.add Year -1 today)
                today
                form.lmpDate
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted 0 0 ]
    , div [ class "ui full segment" ]
        [ div [ class "content" ]
            [ div [ class "form pregnancy-dating" ]
                [ div [ class "header" ] [ text <| translate language Translate.LmpRangeHeader ]
                , lmpRangeInput
                , div [ class "header" ] [ text <| translate language Translate.LmpDateHeader ]
                , lmpDateInput
                , div [ class "header" ] [ text <| translate language Translate.LmpDateConfidentHeader ]
                ]
            ]
        ]
    ]
