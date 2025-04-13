ARG BASE_IMAGE
FROM $BASE_IMAGE

RUN apt-get update || true
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests inotify-tools build-essential mime-support

RUN if [ "$(uname -m)" = "aarch64" ] || [ "$(uname -m)" = "arm64" ]; then \
      DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests autoconf autotools-dev automake; \
      wget https://deb.debian.org/debian/pool/main/libf/libffi/libffi7_3.3-6_arm64.deb -O /tmp/libffi7_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/o/openssl/libssl1.1_1.1.1w-0+deb11u1_arm64.deb -O /tmp/libssl1.1_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-minimal_2.7.18-8+deb11u1_arm64.deb -O /tmp/libpython2.7-minimal_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7-minimal_2.7.18-8+deb11u1_arm64.deb -O /tmp/python2.7-minimal_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-stdlib_2.7.18-8+deb11u1_arm64.deb -O /tmp/libpython2.7-stdlib_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7_2.7.18-8+deb11u1_arm64.deb -O /tmp/python2.7_arm64.deb && \
      dpkg -i /tmp/*.deb && rm /tmp/*.deb; \
    else \
      wget https://deb.debian.org/debian/pool/main/libf/libffi/libffi7_3.3-6_amd64.deb -O /tmp/libffi7_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/o/openssl/libssl1.1_1.1.1w-0+deb11u1_amd64.deb -O /tmp/libssl1.1_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-minimal_2.7.18-8+deb11u1_amd64.deb -O /tmp/libpython2.7-minimal_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7-minimal_2.7.18-8+deb11u1_amd64.deb -O /tmp/python2.7-minimal_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-stdlib_2.7.18-8+deb11u1_amd64.deb -O /tmp/libpython2.7-stdlib_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7_2.7.18-8+deb11u1_amd64.deb -O /tmp/python2.7_amd64.deb && \
      dpkg -i /tmp/*.deb && rm /tmp/*.deb; \
    fi

RUN update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1
# Elm - directly downloading the binary.
RUN wget https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip binary-for-linux-64-bit.gz
RUN mv binary-for-linux-64-bit /usr/bin/elm
RUN chmod +x /usr/bin/elm
# gulp for the client compilation.
RUN npm install -g gulp
# For Elm Unit tests
ENV NPM_CONFIG_UNSAFE_PERM=true
RUN npm install -g elm-test@0.19.1-revision6
# Install svg2elm.
RUN npm install -g svg2elm
