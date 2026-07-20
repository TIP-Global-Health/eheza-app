ARG BASE_IMAGE
FROM $BASE_IMAGE

RUN set -eux; \
    apt-get update; \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
      -o Dpkg::Options::="--force-confold" \
      --no-install-recommends \
      --no-install-suggests \
      inotify-tools \
      build-essential \
      media-types; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/*

RUN if [ "$(uname -m)" = "aarch64" ] || [ "$(uname -m)" = "arm64" ]; then \
      DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests autoconf autotools-dev automake mailcap; \
      wget https://deb.debian.org/debian/pool/main/libf/libffi/libffi7_3.3-6_arm64.deb -O /tmp/libffi7_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/o/openssl/libssl1.1_1.1.1w-0+deb11u1_arm64.deb -O /tmp/libssl1.1_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-minimal_2.7.18-8+deb11u1_arm64.deb -O /tmp/libpython2.7-minimal_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7-minimal_2.7.18-8+deb11u1_arm64.deb -O /tmp/python2.7-minimal_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-stdlib_2.7.18-8+deb11u1_arm64.deb -O /tmp/libpython2.7-stdlib_arm64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7_2.7.18-8+deb11u1_arm64.deb -O /tmp/python2.7_arm64.deb && \
      dpkg --force-depends -i /tmp/*.deb && rm /tmp/*.deb; \
    else \
      apt-get update && apt-get install -y mailcap && \
      wget https://deb.debian.org/debian/pool/main/libf/libffi/libffi7_3.3-6_amd64.deb -O /tmp/libffi7_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/o/openssl/libssl1.1_1.1.1w-0+deb11u1_amd64.deb -O /tmp/libssl1.1_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-minimal_2.7.18-8+deb11u1_amd64.deb -O /tmp/libpython2.7-minimal_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7-minimal_2.7.18-8+deb11u1_amd64.deb -O /tmp/python2.7-minimal_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-stdlib_2.7.18-8+deb11u1_amd64.deb -O /tmp/libpython2.7-stdlib_amd64.deb && \
      wget https://deb.debian.org/debian/pool/main/p/python2.7/python2.7_2.7.18-8+deb11u1_amd64.deb -O /tmp/python2.7_amd64.deb && \
      dpkg --force-depends -i /tmp/*.deb && rm /tmp/*.deb; \
    fi

RUN update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1
# Elm - directly downloading the binary. Since 0.19.2 the release ships one
# asset per architecture, so pick the one matching the container.
RUN set -eux; \
    if [ "$(uname -m)" = "aarch64" ] || [ "$(uname -m)" = "arm64" ]; then \
      ELM_ARCH=arm; \
    else \
      ELM_ARCH=x64; \
    fi; \
    wget -q "https://github.com/elm/compiler/releases/download/0.19.2/elm-0.19.2-linux-${ELM_ARCH}.gz" -O /tmp/elm.gz; \
    gunzip -c /tmp/elm.gz > /usr/bin/elm; \
    rm /tmp/elm.gz; \
    chmod +x /usr/bin/elm; \
    elm --version
# gulp for the client compilation.
RUN npm install -g gulp-cli
# For Elm Unit tests
ENV NPM_CONFIG_UNSAFE_PERM=true
RUN npm install -g elm-test@0.19.2-0
# Install svg2elm.
RUN npm install -g svg2elm
# Downgrade terminus.
RUN curl -L --fail -o /usr/local/bin/terminus https://github.com/pantheon-systems/terminus/releases/download/3.6.2/terminus.phar && chmod +x /usr/local/bin/terminus
