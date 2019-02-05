FROM gizra/drupal-lamp

COPY "$TRAVIS_BUILD_DIR/ci-scripts/docker_files/php-memory-limit.ini" "/usr/local/etc/php/conf.d/php-memory-limit.ini"

ADD $TRAVIS_BUILD_DIR /var/www/html/Server
WORKDIR /var/www/html/Server

# Add a bash script to finalize all
RUN chmod +x /var/www/html/Server/ci-scripts/docker_files/run.sh
ENTRYPOINT ["/var/www/html/Server/ci-scripts/docker_files/run.sh"]

EXPOSE 80 3306 22
