FROM drud/ddev-webserver:v1.17.7
# We need an ancient version of NodeJs.
RUN apt-get purge nodejs --yes
RUN curl -sL https://deb.nodesource.com/setup_8.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
# We need build tools to have compass.
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" --no-install-recommends --no-install-suggests ruby ruby-dev build-essential nodejs=8*
# Elm - directly downloading the binary.
RUN wget https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip binary-for-linux-64-bit.gz
RUN mv binary-for-linux-64-bit /usr/bin/elm
RUN chmod +x /usr/bin/elm
# Install compass.
RUN gem update --system
RUN gem install compass --minimal-deps
# Bower - still in use for some dependencies like dropzone.
RUN npm install -g bower
# gulp for the client compilation.
RUN npm install -g gulp
# For Elm Unit tests
RUN npm -g config set user root
RUN npm install -g elm-test@0.19.1-revision6
