module Pages.Clinics.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import List.Extra
import List.Zipper as Zipper
import Pages.Clinics.Model exposing (Model)
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model exposing (SyncInfoStatus(..))
import SyncManager.Utils exposing (getSyncedHealthCenters)


{-| The `fetch` function is an innovation in how to manage the "lazy" loading
of needed data. It is an alternative to the `...Manager` modules that we have
sometimes used.

The weakness of the `...Manager` modules is that they don't clearly enough
distinguish between the job of marshalling data and providing the UI. By
putting the fundmental data from the backend in the `Pages` hierarchy, it can
become awkward to maintain a single source of truth. Ideally, we would keep all
contact with the backend out of the UI code, and manage the data more
"centrally." (In fact, we should probably have a `Data` module prefix something
like our `Pages` prefix). Then, each view can be provided with whatever data it
needs, from the centrally managed store.

Now, that would be straight-forward if we always had all our data. But we don't
-- we load it lazily from the backend as needed. And, it is the `Manager` modules
(and the view layer in general) that actually knows what data it needs to have
loaded. That is, it is the `view` functions that know what data they actually
need to have loaded. So, that was a strength of the `Manager` modules ... they
kept together the view and the knowledge of what data the view needed.

But, of course, when you're actually in the `view` function, you can't actually
trigger something that will fetch data. That is fundamentally something that
has to happen in the `update` function. However, logically, the data you need
to fetch is not something that is determined by the `msg` you received. So,
the normal "switching" and dispatching on the `msg` isn't what's called for.
Instead, what one would want is to switch on the `activePage` -- that is,
to follow the same hierarchy as the `view` function, but instead of asking for
some HTML, ask whether any data needs to be loaded.

So, the idea for the `fetch` function is that it would ordinarily take the same
parameters as `view`, and it would sort of follow the `view` function, by
assembling a List of messages which need to be sent in order to load data. Of
course, it's important not to load the data over and over again, so this needs
to be tied up with a kind of `WebData` wrapper for the data, so we know not to
trigger a fetch when one is in progress. ONe would also not want to triggr a
fetch in case of error (at least, not without a delay), since you wouldn't want
to just automatically retry errors constantly.

-}
fetch : HealthCenterId -> ModelIndexedDb -> SyncManager.Model.Model -> Model -> List Backend.Model.MsgIndexedDb
fetch selectedHealthCenterId db syncManager model =
    let
        fetchClinicsMsgs =
            RemoteData.toMaybe db.clinics
                |> Maybe.andThen
                    (\clinics ->
                        let
                            selectedHealthCenterSyncInfo =
                                syncManager.syncInfoAuthorities
                                    |> Maybe.andThen
                                        (Zipper.toList >> List.Extra.find (\authorityInfo -> authorityInfo.uuid == fromEntityUuid selectedHealthCenterId))
                        in
                        Maybe.map2
                            (\syncInfo clinicType ->
                                case syncInfo.status of
                                    NotAvailable ->
                                        []

                                    Uploading ->
                                        []

                                    Downloading ->
                                        []

                                    _ ->
                                        let
                                            syncedHealthCenters =
                                                getSyncedHealthCenters syncManager
                                                    |> List.map toEntityUuid
                                        in
                                        Dict.filter
                                            (\_ clinic ->
                                                -- Group belongs to seleced health center.
                                                (clinic.healthCenterId == selectedHealthCenterId)
                                                    -- Health center is synced.
                                                    && List.member clinic.healthCenterId syncedHealthCenters
                                                    -- Group is of selected type.
                                                    && (clinic.clinicType == clinicType)
                                            )
                                            clinics
                                            |> Dict.keys
                                            |> List.map Backend.Model.FetchSessionsByClinic
                            )
                            selectedHealthCenterSyncInfo
                            model.clinicType
                    )
                |> Maybe.withDefault []
    in
    -- So, to recap, this is called by the parent's `fetch` function, to see
    -- whether any data needs to be fetched. We can return messages that will
    -- fetch needed data. The function is located here because it is kind of a
    -- companion to the `view` function ... it needs to stay in sync with the
    -- view function, so that the things the `view` function needs are in fact
    -- loaded. So, the signature for the two functions would generally be
    -- related.
    --
    -- What we're returning here is a list of the desired messages. We don't
    -- need to check whether we actually have the data ... that will be done
    -- centrally, by looking at the messages we return. That allows us to
    -- **remember** what data is desired ... and, thus, no longer desired ...
    -- so we can know when to **forget** things as well.
    Backend.Model.FetchClinics :: fetchClinicsMsgs
