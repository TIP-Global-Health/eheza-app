module Pages.Person.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.Relationship.Model exposing (Relationship)
import Backend.Relationship.Utils exposing (getRelatedTo, toMyRelationship)
import EveryDict
import EveryDictList
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html Msg
view language currentDate id db =
    div
        [ class "wrap wrap-alt-2" ]
        [ viewHeader language
        , div
            [ class "ui full segment blue" ]
            [ EveryDict.get id db.people
                |> Maybe.withDefault NotAsked
                |> viewWebData language (viewParticipantDetailsForm language currentDate db id) identity
            ]
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.People ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewRelationship : Language -> NominalDate -> ModelIndexedDb -> PersonId -> RelationshipId -> Relationship -> Html Msg
viewRelationship language currentDate db personId relationshipId relationship =
    let
        viewMyRelationship myRelationship =
            let
                relatedToId =
                    getRelatedTo myRelationship

                relatedTo =
                    EveryDict.get relatedToId db.people
                        |> Maybe.withDefault NotAsked
            in
            div []
                [ h4
                    [ class "ui header" ]
                    [ text <| translate language <| Translate.MyRelationship myRelationship ]
                , div
                    [ class "ui unstackable items" ]
                    [ viewWebData language (viewParticipant language currentDate relatedToId) identity relatedTo ]
                ]
    in
    toMyRelationship personId relationship
        |> Maybe.map viewMyRelationship
        |> showMaybe


viewParticipantDetailsForm : Language -> NominalDate -> ModelIndexedDb -> PersonId -> Person -> Html Msg
viewParticipantDetailsForm language currentDate db id person =
    let
        viewFamilyMembers relationships =
            relationships
                |> EveryDictList.map (viewRelationship language currentDate db id)
                |> EveryDictList.values
                |> div []

        familyMembers =
            EveryDict.get id db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> viewWebData language viewFamilyMembers identity
    in
    div [ class "wrap-list registration-page view" ]
        [ h3
            [ class "ui header" ]
            [ text <| translate language Translate.DemographicInformation ++ ": " ]
        , div
            [ class "ui unstackable items" ]
            [ viewParticipant language currentDate id person ]
        , div [ class "separator-line" ] []
        , h3
            [ class "ui header" ]
            [ text <| translate language Translate.FamilyMembers ++ ": " ]
        , familyMembers
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
                        [ text <| person.name ]
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
