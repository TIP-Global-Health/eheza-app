ARG BASE_IMAGE
FROM $BASE_IMAGE

RUN apt-get update || true
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests inotify-tools build-essential mime-support

# Download and install Python 2.7 and dependencies from Debian 11 (Bullseye)
ADD https://deb.debian.org/debian/pool/main/libf/libffi/libffi7_3.3-6_amd64.deb /tmp/
ADD https://deb.debian.org/debian/pool/main/o/openssl/libssl1.1_1.1.1w-0+deb11u1_amd64.deb /tmp/
ADD https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-minimal_2.7.18-8+deb11u1_amd64.deb /tmp/
ADD https://deb.debian.org/debian/pool/main/p/python2.7/python2.7-minimal_2.7.18-8+deb11u1_amd64.deb /tmp/
ADD https://deb.debian.org/debian/pool/main/p/python2.7/libpython2.7-stdlib_2.7.18-8+deb11u1_amd64.deb /tmp/
ADD https://deb.debian.org/debian/pool/main/p/python2.7/python2.7_2.7.18-8+deb11u1_amd64.deb /tmp/

# Install the downloaded packages
RUN dpkg -i /tmp/*.deb && rm /tmp/*.deb

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
