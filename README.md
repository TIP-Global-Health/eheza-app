# ihangane

[![Build Status](https://travis-ci.com/Gizra/ihangane.svg?token=p2M1EeCrd3dY32WxWj3X&branch=master)](https://travis-ci.com/Gizra/ihangane)

This app is now using Progressive Web App techniques in order to load static
assets (Javascript, images, CSS, etc.) while offline. This has several
implications.

- In order to force the app to use the latest version, you may need to do a
  shift-reload in the web browser. (Otherwise, you may get the cached version
  of the app, as if you were offline, even if there is a newer version
  available online).

- We have some fairly strict requirements for browser compatibility at the
  moment, for the sake of simplifying the implementation of offline support and
  photo uploading. Basically, you need a recent version of Chrome (Chrome 60 or
  later) -- otherwise, various things will fail, without helpful error messages.
  We can't support Safari at the moment, because it lacks support for service
  workers. However, service worker support is coming to Safari relatively soon,
  so it may be possible in the future.

- The app will "take over" the URL you use (e.g. ***REMOVED***)
  and serve itself from the cache. This isn't a big deal for
  ***REMOVED***, but is more of an issue for
  http://localhost:3000/. To re-use http://localhost:3000/ for something else,
  you can un-register the app in Chrome by opening the Developer Tools, and
  looking at "Service Workers" in the "Application" tab.

- To get the "work while offline" caching, you need to access the app
  via https ... e.g. https://***REMOVED***/app/

Eventually, we may want to deal with some of this in the app itself (i.e. ask
the user to reload when a new version is available, and give the user a way to
relinquish the URL from within the app itself).
