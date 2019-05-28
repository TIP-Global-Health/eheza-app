module Pages.Relationship.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, Relationship)
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Relationship.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


{-| Offer to edit the relationship between these persons, from the point of
view of the first person.
-}
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

        request =
            EveryDict.get id1 db.postRelationship
                |> Maybe.withDefault NotAsked

        fetched =
            RemoteData.map3 FetchedData person1 person2 relationships
    in
    viewWebData language (viewFetchedContent language currentDate id1 id2 model request) identity fetched


type alias FetchedData =
    { person1 : Person
    , person2 : Person
    , relationships : EveryDictList RelationshipId MyRelationship
    }


viewFetchedContent : Language -> NominalDate -> PersonId -> PersonId -> Model -> WebData MyRelationship -> FetchedData -> Html Msg
viewFetchedContent language currentDate id1 id2 model request data =
    let
        savedRelationship =
            data.relationships
                |> EveryDictList.filter (\_ relationship -> relationship.relatedTo == id2)
                |> EveryDictList.head
                |> Maybe.map (Tuple.second >> .relatedBy)

        viewedRelationship =
            model
                |> Maybe.Extra.orElse savedRelationship

        possibleRelationships =
            case isPersonAnAdult currentDate data.person1 of
                Just True ->
                    [ MyChild, MyCaregiven ]

                Just False ->
                    [ MyParent, MyCaregiver ]

                Nothing ->
                    [ MyChild, MyCaregiven, MyParent, MyCaregiver ]

        relationshipSelector =
            div
                [ class "ui form relationship-selector" ]
                [ div
                    [ class "grouped fields" ]
                    (List.indexedMap showRelationship possibleRelationships)
                ]

        showRelationship index possible =
            let
                isChecked =
                    viewedRelationship == Just possible

                inputId =
                    "input-relationship-" ++ toString index
            in
            div
                [ class "field" ]
                [ div
                    [ classList
                        [ ( "ui radio checkbox", True )
                        , ( "checked", isChecked )
                        ]
                    ]
                    [ input
                        [ type_ "radio"
                        , id inputId
                        , checked isChecked
                        , classList [ ( "checked", isChecked ) ]
                        , onCheck (always (RelationshipSelected possible))
                        ]
                        []
                    , label
                        [ class "relationship-selection"
                        , for inputId
                        ]
                        [ text <| translate language <| Translate.MyRelatedByQuestion possible ]
                    ]
                ]

        buttons =
            div
                [ class "ui grid save-buttons" ]
                [ div
                    [ class "four wide column" ]
                    [ button
                        [ class "ui button secondary fluid"
                        , onClick Reset
                        ]
                        [ text <| translate language Translate.Cancel ]
                    ]
                , div
                    [ class "eight wide column" ]
                    []
                , div
                    [ class "four wide column" ]
                    [ button
                        [ classList
                            [ ( "ui button primary fluid", True )
                            , ( "loading", RemoteData.isLoading request )
                            , ( "disabled", RemoteData.isLoading request )
                            ]
                        , onClick Save
                        ]
                        [ text <| translate language Translate.Save ]
                    ]
                ]

        requestStatus =
            case request of
                Success _ ->
                    emptyNode

                Failure err ->
                    div
                        [ class "ui warning message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.BackendError ]
                        , viewError language err
                        ]

                Loading ->
                    emptyNode

                NotAsked ->
                    emptyNode
    in
    div [ class "registration-page view" ]
        [ div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id1 data.person1 ]
        , relationshipSelector
        , div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id2 data.person2 ]
        , requestStatus
        , buttons
        ]


viewParticipant : Language -> NominalDate -> PersonId -> Person -> Html Msg
viewParticipant language currentDate id person =
    let
        typeForThumbnail =
            case isPersonAnAdult currentDate person of
                Just True ->
                    "mother"

                Just False ->
                    "child"

                Nothing ->
                    "mother"

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
