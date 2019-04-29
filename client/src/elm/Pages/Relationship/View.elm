module Pages.Relationship.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.Relationship.Model exposing (MyRelationship(..), Relationship)
import Backend.Relationship.Utils exposing (toMyRelationship)
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
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
    let
        savedRelationship : Maybe ( RelationshipId, MyRelationship )
        savedRelationship =
            data.relationships
                |> EveryDictList.filter (\_ relationship -> relationship.person == id2 || relationship.relatedTo == id2)
                |> EveryDictList.head
                |> Maybe.andThen
                    (\( relationshipId, relationship ) ->
                        toMyRelationship id1 relationship
                            |> Maybe.map (\myRelationship -> ( relationshipId, myRelationship ))
                    )

        viewedRelationship : Maybe MyRelationship
        viewedRelationship =
            model
                |> Maybe.Extra.orElse (Maybe.map Tuple.second savedRelationship)

        -- We could look at the birthdates of person1 and person2 to cut down
        -- on these possibilities. (E.g. the older one can't be the child of
        -- the younger one).
        possibleRelationships =
            [ MyChild id2
            , MyCaregiver id2
            , MyParent id2
            , MyCaregiverFor id2
            ]

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
                        [ text <| translate language <| Translate.MyRelationshipQuestion possible ]
                    ]
                ]

        buttons =
            div
                [ class "ui grid save-buttons" ]
                [ div
                    [ class "four wide column" ]
                    [ button
                        [ class "ui button secondary fluid"
                        , onClick Cancel
                        ]
                        [ text <| translate language Translate.Cancel ]
                    ]
                , div
                    [ class "eight wide column" ]
                    []
                , div
                    [ class "four wide column" ]
                    [ button
                        [ class "ui button primary fluid"
                        , onClick Save
                        ]
                        [ text <| translate language Translate.Save ]
                    ]
                ]
    in
    div [ class "registration-page view" ]
        [ div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id1 data.person1 ]
        , relationshipSelector
        , div
            [ class "ui unstackable items participants-list" ]
            [ viewParticipant language currentDate id2 data.person2 ]
        , buttons
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
