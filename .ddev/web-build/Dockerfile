FROM drud/ddev-webserver:v1.19.1
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
