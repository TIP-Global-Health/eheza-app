module Pages.PrenatalEncounter.View exposing (view)

import App.Model
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Encoder exposing (encodeEducationLevel, encodeMaritalStatus, encodeUbudehe)
import Backend.Person.Form exposing (ExpectedAge(..), PersonForm, expectedAgeFromForm, validatePerson)
import Backend.Person.Model exposing (Gender(..), Person, allEducationLevels, allMaritalStatuses, allUbudehes)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Date.Extra as Date exposing (Interval(Year))
import DateSelector.SelectorDropdown
import EveryDict
import EveryDictList exposing (EveryDictList)
import Form exposing (Form)
import Form.Field
import Form.Input
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityId)
import Set
import Time.Date exposing (date)
import Time.Iso8601
import Translate exposing (Language, TranslationId, translate)
import Utils.Form exposing (dateInput, getValueAsInt, isFormFieldSet, viewFormError)
import Utils.GeoLocation exposing (GeoInfo, geoInfo, getGeoLocation)
import Utils.Html exposing (script, tabItem, thumbnailImage, viewLoading)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    let
        content =
            EveryDict.get id db.people
                |> unwrap
                    []
                    (RemoteData.toMaybe
                        >> unwrap
                            []
                            (\mother ->
                                [ div [ class "ui unstackable items participant-page mother" ]
                                    [ viewMotherDetails language currentDate mother
                                    , viewMeasurements language currentDate
                                    ]
                                ]
                            )
                    )
    in
    div
        [ class "page-prenatal-encounter" ]
    <|
        viewHeader language
            :: content


viewHeader : Language -> Html App.Model.Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PrenatalEncounter ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewMotherDetails : Language -> NominalDate -> Person -> Html App.Model.Msg
viewMotherDetails language currentDate mother =
    div [ class "item" ]
        [ div [ class "ui image" ]
            [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
        , div [ class "content" ]
            [ h2 [ class "ui header" ]
                [ text mother.name ]
            , showMaybe <|
                Maybe.map
                    (\age ->
                        p [ class "age-wrapper" ]
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ":" ]
                            , span [] [ text <| translate language <| Translate.YearsOld age ]
                            ]
                    )
                    (ageInYears currentDate mother)
            ]
        ]


viewMeasurements : Language -> NominalDate -> Html App.Model.Msg
viewMeasurements language currentDate =
    let
        dummyDate =
            date 2019 12 10

        diffInDays =
            diffDays currentDate dummyDate

        diffInWeeks =
            diffInDays // 7

        egaWeeks =
            translate language <| Translate.WeekSinglePlural diffInWeeks

        egaDays =
            translate language <| Translate.DaySinglePlural (diffInDays - 7 * diffInWeeks)

        dummyGravida =
            2

        dummyPara =
            "0102"
    in
    div [ class "item measurements" ]
        [ div [ class "ui edd" ]
            [ div [ class "label" ] [ text <| translate language Translate.Edd ++ ":" ]
            , div [ class "value" ] [ text <| formatMMDDYYYY dummyDate ]
            ]
        , div [ class "ui ega" ]
            [ div [ class "label" ] [ text <| translate language Translate.Ega ++ ":" ]
            , div [ class "value" ] [ text <| egaWeeks ++ ", " ++ egaDays ]
            ]
        , div [ class "ui gravida" ]
            [ div [ class "label" ] [ text <| translate language Translate.Gravida ++ ":" ]
            , div [ class "value" ] [ text <| toString dummyGravida ]
            ]
        , div [ class "ui para" ]
            [ div [ class "label" ] [ text <| translate language Translate.Para ++ ":" ]
            , div [ class "value" ] [ text dummyPara ]
            ]
        ]
