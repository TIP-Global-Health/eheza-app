module Pages.PrenatalParticipant.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EveryDict
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalParticipant.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityId)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    let
        person =
            EveryDict.get id db.people
                |> Maybe.withDefault NotAsked

        headerName =
            person
                |> RemoteData.map .name
                |> RemoteData.withDefault (translate language Translate.PrenatalParticipant ++ " " ++ fromEntityUuid id)
    in
    div
        [ class "page-prenatal-participant" ]
        [ viewHeader language headerName
        , div
            [ class "ui full segment blue" ]
            [-- viewWebData language (viewParticipantDetailsForm language currentDate db id) identity person
            ]
        ]


viewHeader : Language -> String -> Html App.Model.Msg
viewHeader language name =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text name ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage PrenatalParticipantsPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]
