// This is a manual roll-up of kelektiv-uuid. Eventually, we could actually use
// something like rollup or browserify, but it's pretty easy to do this
// manually for this one case.

/* The MIT License (MIT)

Copyright (c) 2010-2016 Robert Kieffer and other contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

'use strict';

var kelektivUuid = (function() {

  // --------------
  // bytesToUuid.js
  // --------------

  var byteToHex = [];
  for (var i = 0; i < 256; ++i) {
    byteToHex[i] = (i + 0x100).toString(16).substr(1);
  }

  function bytesToUuid(buf, offset) {
    var i = offset || 0;
    var bth = byteToHex;
    // join used to fix memory issue caused by concatenation: https://bugs.chromium.org/p/v8/issues/detail?id=3175#c4
    return ([bth[buf[i++]], bth[buf[i++]],
      bth[buf[i++]], bth[buf[i++]], '-',
      bth[buf[i++]], bth[buf[i++]], '-',
      bth[buf[i++]], bth[buf[i++]], '-',
      bth[buf[i++]], bth[buf[i++]], '-',
      bth[buf[i++]], bth[buf[i++]],
      bth[buf[i++]], bth[buf[i++]],
      bth[buf[i++]], bth[buf[i++]]
    ]).join('');
  }

  // --------------
  // rng-browser-js
  // --------------

  var getRandomValues = (typeof(crypto) != 'undefined' && crypto.getRandomValues &&
      crypto.getRandomValues.bind(crypto)) ||
    (typeof(msCrypto) != 'undefined' && typeof window.msCrypto.getRandomValues ==
      'function' && msCrypto.getRandomValues.bind(msCrypto));

  if (getRandomValues) {
    // WHATWG crypto RNG - http://wiki.whatwg.org/wiki/Crypto
    var rnds8 = new Uint8Array(16); // eslint-disable-line no-undef

    var rng = function whatwgRNG() {
      getRandomValues(rnds8);
      return rnds8;
    };
  } else {
    // Math.random()-based (RNG)
    //
    // If all else fails, use Math.random().  It's fast, but is of unspecified
    // quality.
    var rnds = new Array(16);

    var rng = function mathRNG() {
      for (var i = 0, r; i < 16; i++) {
        if ((i & 0x03) === 0) r = Math.random() * 0x100000000;
        rnds[i] = r >>> ((i & 0x03) << 3) & 0xff;
      }

      return rnds;
    };
  }

  // -----
  // v4.js
  // -----

  function v4(options, buf, offset) {
    var i = buf && offset || 0;

    if (typeof(options) == 'string') {
      buf = options === 'binary' ? new Array(16) : null;
      options = null;
    }
    options = options || {};

    var rnds = options.random || (options.rng || rng)();

    // Per 4.4, set bits for version and `clock_seq_hi_and_reserved`
    rnds[6] = (rnds[6] & 0x0f) | 0x40;
    rnds[8] = (rnds[8] & 0x3f) | 0x80;

    // Copy bytes to buffer, if provided
    if (buf) {
      for (var ii = 0; ii < 16; ++ii) {
        buf[i + ii] = rnds[ii];
      }
    }

    return buf || bytesToUuid(rnds);
  }

  // ---------------
  // sha1-browser.js
  // ---------------

  function f(s, x, y, z) {
    switch (s) {
      case 0:
        return (x & y) ^ (~x & z);
      case 1:
        return x ^ y ^ z;
      case 2:
        return (x & y) ^ (x & z) ^ (y & z);
      case 3:
        return x ^ y ^ z;
    }
  }

  function ROTL(x, n) {
    return (x << n) | (x >>> (32 - n));
  }

  function sha1(bytes) {
    var K = [0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xca62c1d6];
    var H = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0];

    if (typeof(bytes) == 'string') {
      var msg = unescape(encodeURIComponent(bytes)); // UTF8 escape
      bytes = new Array(msg.length);
      for (var i = 0; i < msg.length; i++) bytes[i] = msg.charCodeAt(i);
    }

    bytes.push(0x80);

    var l = bytes.length / 4 + 2;
    var N = Math.ceil(l / 16);
    var M = new Array(N);

    for (var i = 0; i < N; i++) {
      M[i] = new Array(16);
      for (var j = 0; j < 16; j++) {
        M[i][j] =
          bytes[i * 64 + j * 4] << 24 |
          bytes[i * 64 + j * 4 + 1] << 16 |
          bytes[i * 64 + j * 4 + 2] << 8 |
          bytes[i * 64 + j * 4 + 3];
      }
    }

    M[N - 1][14] = ((bytes.length - 1) * 8) /
      Math.pow(2, 32);
    M[N - 1][14] = Math.floor(M[N - 1][14]);
    M[N - 1][15] = ((bytes.length - 1) * 8) & 0xffffffff;

    for (var i = 0; i < N; i++) {
      var W = new Array(80);

      for (var t = 0; t < 16; t++) W[t] = M[i][t];
      for (var t = 16; t < 80; t++) {
        W[t] = ROTL(W[t - 3] ^ W[t - 8] ^ W[t - 14] ^ W[t - 16], 1);
      }

      var a = H[0];
      var b = H[1];
      var c = H[2];
      var d = H[3];
      var e = H[4];

      for (var t = 0; t < 80; t++) {
        var s = Math.floor(t / 20);
        var T = ROTL(a, 5) + f(s, b, c, d) + e + K[s] + W[t] >>> 0;
        e = d;
        d = c;
        c = ROTL(b, 30) >>> 0;
        b = a;
        a = T;
      }

      H[0] = (H[0] + a) >>> 0;
      H[1] = (H[1] + b) >>> 0;
      H[2] = (H[2] + c) >>> 0;
      H[3] = (H[3] + d) >>> 0;
      H[4] = (H[4] + e) >>> 0;
    }

    return [
      H[0] >> 24 & 0xff, H[0] >> 16 & 0xff, H[0] >> 8 & 0xff, H[0] & 0xff,
      H[1] >> 24 & 0xff, H[1] >> 16 & 0xff, H[1] >> 8 & 0xff, H[1] & 0xff,
      H[2] >> 24 & 0xff, H[2] >> 16 & 0xff, H[2] >> 8 & 0xff, H[2] & 0xff,
      H[3] >> 24 & 0xff, H[3] >> 16 & 0xff, H[3] >> 8 & 0xff, H[3] & 0xff,
      H[4] >> 24 & 0xff, H[4] >> 16 & 0xff, H[4] >> 8 & 0xff, H[4] & 0xff
    ];
  }

  // ------
  // v35.js
  // ------

  function uuidToBytes(uuid) {
    // Note: We assume we're being passed a valid uuid string
    var bytes = [];
    uuid.replace(/[a-fA-F0-9]{2}/g, function(hex) {
      bytes.push(parseInt(hex, 16));
    });

    return bytes;
  }

  function stringToBytes(str) {
    str = unescape(encodeURIComponent(str)); // UTF8 escape
    var bytes = new Array(str.length);
    for (var i = 0; i < str.length; i++) {
      bytes[i] = str.charCodeAt(i);
    }
    return bytes;
  }

  var v35 = function(name, version, hashfunc) {
    var generateUUID = function(value, namespace, buf, offset) {
      var off = buf && offset || 0;

      if (typeof(value) == 'string') value = stringToBytes(value);
      if (typeof(namespace) == 'string') namespace = uuidToBytes(
        namespace);

      if (!Array.isArray(value)) throw TypeError(
        'value must be an array of bytes');
      if (!Array.isArray(namespace) || namespace.length !== 16) throw TypeError(
        'namespace must be uuid string or an Array of 16 byte values'
      );

      // Per 4.3
      var bytes = hashfunc(namespace.concat(value));
      bytes[6] = (bytes[6] & 0x0f) | version;
      bytes[8] = (bytes[8] & 0x3f) | 0x80;

      if (buf) {
        for (var idx = 0; idx < 16; ++idx) {
          buf[off + idx] = bytes[idx];
        }
      }

      return buf || bytesToUuid(bytes);
    };

    // Function#name is not settable on some platforms (#270)
    try {
      generateUUID.name = name;
    } catch (err) {}

    // Pre-defined namespaces, per Appendix C
    generateUUID.DNS = '6ba7b810-9dad-11d1-80b4-00c04fd430c8';
    generateUUID.URL = '6ba7b811-9dad-11d1-80b4-00c04fd430c8';

    return generateUUID;
  };

  // Our exports
  return {
    v4: v4,
    v5: v35('v5', 0x50, sha1)
  };
})();
