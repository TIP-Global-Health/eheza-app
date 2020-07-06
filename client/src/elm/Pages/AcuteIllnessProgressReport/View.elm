module Pages.AcuteIllnessProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Pages.AcuteIllnessEncounter.Model exposing (AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 180
    , height = 180
    }


view : Language -> NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate id db =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id) identity data


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AssembledData -> Html Msg
viewContent language currentDate id data =
    let
        backIcon =
            a
                [ class "icon-back"
                , onClick <| SetActivePage (UserPage (AcuteIllnessEncounterPage id))
                ]
                []

        title =
            h1
                [ class "ui report header" ]
                [ text <| translate language Translate.ProgressReport ]

        subtitle =
            p
                [ class "date" ]
                [ text <| translate language Translate.CurrentIllnessBegan
                , text " - "
                , text <| renderDate language currentDate
                ]
    in
    div [ class "page-report acute-illness" ]
        [ div
            [ class "wrap-report" ]
            [ backIcon
            , title
            , subtitle
            , viewPersonInfo language currentDate data.person data.measurements
            ]
        ]


viewPersonInfo : Language -> NominalDate -> Person -> AcuteIllnessMeasurements -> Html Msg
viewPersonInfo language currentDate person measurements =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )

        viewAge =
            maybeAge
                |> Maybe.map
                    (\age ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ":" ]
                            , span [] [ text age ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        viewVillage =
            person.village
                |> Maybe.map
                    (\village ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.Village ++ ": " ]
                            , span [] [ text village ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div
        [ class "ui report unstackable items" ]
        [ div
            [ class "item person-details" ]
            [ div [ class "ui image" ]
                [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width
                ]
            , div [ class "content" ]
                [ h2 [ class "ui header" ]
                    [ text person.name ]
                , viewAge
                , viewVillage
                ]
            ]
        ]
