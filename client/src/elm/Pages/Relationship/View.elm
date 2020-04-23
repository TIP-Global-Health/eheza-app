module Pages.Relationship.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, defaultIconForPerson, isPersonAnAdult)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, Relationship)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isNothing)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Relationship.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid)
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
            [ text <| translate language Translate.CreateRelationship ]
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
            Dict.get id1 db.people
                |> Maybe.withDefault NotAsked

        relationships =
            Dict.get id1 db.relationshipsByPerson
                |> Maybe.withDefault NotAsked

        person2 =
            Dict.get id2 db.people
                |> Maybe.withDefault NotAsked

        request =
            Dict.get id1 db.postRelationship
                |> Maybe.withDefault NotAsked

        participants =
            Dict.get id1 db.participantsByPerson
                |> Maybe.withDefault NotAsked

        clinics =
            db.clinics

        fetched =
            RemoteData.map FetchedData person1
                |> RemoteData.andMap person2
                |> RemoteData.andMap relationships
                |> RemoteData.andMap participants
                |> RemoteData.andMap clinics
    in
    viewWebData language (viewFetchedContent language currentDate id1 id2 model request) identity fetched


type alias FetchedData =
    { person1 : Person
    , person2 : Person
    , relationships : Dict RelationshipId MyRelationship
    , participants : Dict PmtctParticipantId PmtctParticipant
    , clinics : Dict ClinicId Clinic
    }


viewFetchedContent : Language -> NominalDate -> PersonId -> PersonId -> Model -> WebData MyRelationship -> FetchedData -> Html Msg
viewFetchedContent language currentDate id1 id2 model request data =
    let
        participants =
            data.participants
                |> Dict.filter
                    (\_ participant ->
                        (participant.child == id1 && participant.adult == id2)
                            || (participant.adult == id1 && participant.child == id2)
                    )

        currentGroupsIds =
            participants
                |> Dict.values
                |> List.map .clinic

        viewCurrentGroups =
            currentGroupsIds
                |> List.filterMap
                    (\clinicId ->
                        Dict.get clinicId data.clinics
                            |> Maybe.map .name
                    )
                |> String.join ", "
                |> text
                |> List.singleton
                |> div [ class "current-groups" ]

        viewGroupSelector =
            let
                emptyOption =
                    option
                        [ value ""
                        , selected (model.assignToGroup == Nothing)
                        ]
                        [ text "" ]

                selector =
                    data.clinics
                        |> Dict.filter
                            (\clinicId clinic ->
                                -- Clinic is not already selected.
                                (not <| List.member clinicId currentGroupsIds)
                                    && -- If both persons are assigned to a health
                                       -- center, show the clinic if it is
                                       -- assigned to one or the other.  If one of
                                       -- the persons has no health center, show
                                       -- all clinics.
                                       (Maybe.map2
                                            (\hc1 hc2 ->
                                                clinic.healthCenterId
                                                    == hc1
                                                    || clinic.healthCenterId
                                                    == hc2
                                            )
                                            data.person1.healthCenterId
                                            data.person2.healthCenterId
                                            |> Maybe.withDefault True
                                       )
                            )
                        |> Dict.map
                            (\clinicId clinic ->
                                option
                                    [ value (fromEntityUuid clinicId)
                                    , selected (model.assignToGroup == Just clinicId)
                                    ]
                                    [ text clinic.name ]
                            )
                        |> Dict.values
                        |> (::) emptyOption
                        |> select [ onInput AssignToClinicId ]
            in
            div [ class "ui form" ]
                [ div
                    [ class "inline field" ]
                    [ label [] [ text <| translate language Translate.AddToGroup ]
                    , selector
                    ]
                ]

        savedRelationship =
            data.relationships
                |> Dict.filter (\_ relationship -> relationship.relatedTo == id2)
                |> Dict.toList
                |> List.head
                |> Maybe.map (Tuple.second >> .relatedBy)

        viewedRelationship =
            model.relatedBy
                |> Maybe.Extra.orElse savedRelationship

        possibleRelationships =
            let
                expected =
                    case isPersonAnAdult currentDate data.person1 of
                        Just True ->
                            [ MyChild, MyCaregiven ]

                        Just False ->
                            [ MyParent, MyCaregiver ]

                        Nothing ->
                            [ MyChild, MyCaregiven, MyParent, MyCaregiver ]
            in
            case savedRelationship of
                -- In case we got relationship already,
                -- we do not allow to set another relationship type.
                Just saved ->
                    [ saved ]

                Nothing ->
                    -- Always add the currently set relationship, if there is one, even
                    -- if it's not expected.
                    case viewedRelationship of
                        Just viewed ->
                            if List.member viewed expected then
                                expected

                            else
                                viewed :: expected

                        Nothing ->
                            expected

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
                    "input-relationship-" ++ Debug.toString index
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
                            , ( "disabled", RemoteData.isLoading request || isNothing model.assignToGroup || isNothing viewedRelationship )
                            ]
                        , onClick <| Save viewedRelationship
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
        , div
            [ class "ui unstackable items participants-list" ]
            [ div
                [ class "ui header" ]
                [ text <| translate language Translate.Groups ++ ": " ]
            , viewCurrentGroups
            , viewGroupSelector
            ]
        , requestStatus
        , buttons
        ]


viewParticipant : Language -> NominalDate -> PersonId -> Person -> Html Msg
viewParticipant language currentDate id person =
    let
        typeForThumbnail =
            defaultIconForPerson currentDate person

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
