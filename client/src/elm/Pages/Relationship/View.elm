module Pages.Relationship.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.Relationship.Model exposing (Relationship)
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Relationship.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PersonId -> PersonId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id1 id2 db model =
    div
        [ class "page-relationship" ]
        [ viewHeader language
        , div
            [ class "ui full segment blue" ]
            [ viewContent language currentDate id1 id2 db model ]
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.EditRelationship ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


type alias FetchedData =
    { person1 : Person
    , person2 : Person
    , relationships : EveryDictList RelationshipId Relationship
    }


viewContent : Language -> NominalDate -> PersonId -> PersonId -> ModelIndexedDb -> Model -> Html Msg
viewContent language currentDate id1 id2 db model =
    let
        person1 =
            EveryDict.get id1 db.people
                |> Maybe.withDefault NotAsked

        relationships =
            EveryDict.get id1 db.relationshipsByPerson
                |> Maybe.withDefault NotAsked

        person2 =
            EveryDict.get id2 db.people
                |> Maybe.withDefault NotAsked

        fetched =
            RemoteData.map3 FetchedData person1 person2 relationships
    in
    viewWebData language (viewFetchedContent language currentDate id1 id2 model) identity fetched


viewFetchedContent : Language -> NominalDate -> PersonId -> PersonId -> Model -> FetchedData -> Html Msg
viewFetchedContent language currentDate id1 id2 model data =
    div [ class "registration-page view" ]
        [ div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id1 data.person1 ]
        , div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id2 data.person2 ]
        ]


viewParticipant : Language -> NominalDate -> PersonId -> Person -> Html Msg
viewParticipant language currentDate id person =
    let
        typeForThumbnail =
            ageInYears currentDate person
                |> Maybe.map
                    (\age ->
                        if age > 12 then
                            "mother"

                        else
                            "child"
                    )
                |> Maybe.withDefault "mother"

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text person.name ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ person.birthDate
                                |> Maybe.map (renderDate language >> text)
                                |> showMaybe
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ person.village |> Maybe.withDefault "" |> text ]
                        ]
                    ]
                ]
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage typeForThumbnail person.avatarUrl person.name 120 120 ]
        , content
        ]
