This directory contains some files useful for generating a self-signed SSL cert
that you can use in order to run a local server via browserSync with SSL. This
isn't normally necessary, because Chrome will trust 'localhost' as if it were
HTTPS. However, it is needed when running the Android emulator, because it
accesses your local environment as if it were a separate machine, at 10.0.2.2.

To install the certificate in the Android emulator, use the following steps:

- Run gulp ssl-cert to generate the certificate
- Drag the ssl/ssl.crt file onto the running Android emulator.
  This should copy it to Android storage.
- Go to the "settings" app in Android emulator, then "Security"
- Click on "Install from SD card"
- Click "Virtual SD Card"
- Click on "Download"
- Your ssl.crt file should appear ... click on it.
- It's self-explanatory from there.

To run the HTTPS server, just go `gulp emulator`. It will use the certificates
which you just generated.

Then, there's one more step. The certificates are set up for `eheza-app.dev`, so
you need to edit your Android /system/etc/hosts file to point `eheza-app.dev` at
your local machine. (Or, if you control your DNS server, you could do it that
way instead). Now, the Android emulator sets things up so that your local
machine is 10.0.2.2. So, that's what you need to point `eheza-app.dev` at.

To do that, you need to do roughly the following steps. (This is in addition
to "normal" emulator setup).

- Start the emulator with the `-writeable system` option.

  For me, that was something like:

      ./emulator -netdelay none -netspeed full -writable-system -avd Galaxy_Tab_4_API_24

  Now, here's a tricky bit: you need to *keep doing this* every time after you
  make these modicateions. That is, from now on, you *always* have to use the
  `-writeable-system` option -- it's not a one-time thing. (The rest of this is
  just a one-time thing).

- Fetch the `/system/etc/hosts` file.

  For me, the sequence was something like:

      ./adb root
      ./adb remount
      ./adb pull /system/etc/hosts

- Edit the hosts file you fetched to add `eheza-app.dev`

  So, something like adding a new line like this:

      10.0.2.2        eheza-app.dev

- Push the file back to the emulator.

  For me, something like:

      ./adb push hosts /system/etc/hosts

And, that should do it. Just remember that from now on you have to start the
emulator with the `-writable-system` option -- it just stalls otherwise.

Oh, also remember to make an entry in `LocalConfig.elm` to cover
`eheza-app.dev`. The `backendUrl` entry there is going to also have to be HTTPS.
So, you'll need to use the same cert & key files to configure whatever HTTP
server you're using for the backend as well. (Alternatively, perhaps we could
set up a proxy via BrowserSync).
