ARG BASE_IMAGE
FROM $BASE_IMAGE

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests inotify-tools python2.7 build-essential
RUN update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1
# Elm - directly downloading the binary.
RUN wget https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip binary-for-linux-64-bit.gz
RUN mv binary-for-linux-64-bit /usr/bin/elm
RUN chmod +x /usr/bin/elm
# gulp for the client compilation.
RUN npm install -g gulp
# For Elm Unit tests
RUN npm -g config set user root
RUN npm install -g elm-test@0.19.1-revision6
# Install svg2elm.
RUN npm install -g svg2elm
RUN if [ "$(uname -m)" = "aarch64" ]; then \
      DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests autoconf autotools-dev automake; \
    fi
