/*
 * This contains some common code for our service worker script. So,
 * it is included by sw-precache first, in a global context.
 */
'use strict';

// Various constants that get used in multiple places.
var syncTag = 'sync';
var configCache = 'config';
var configUrlRegex = /\/sw\/config/;
var credentialsUrlRegex = /\/sw\/config\/device$/;
var credentialsUrl = '/sw/config/device';

var nodesUuid = "78cf21d1-b3f4-496a-b312-d8ae73041f09";
