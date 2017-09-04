core = 7.x
api = 2

; Modules
projects[admin_menu][subdir] = "contrib"
projects[admin_menu][version] = "3.0-rc5"

projects[admin_views][subdir] = "contrib"
projects[admin_views][version] = "1.6"

projects[auto_entitylabel][subdir] = "contrib"
projects[auto_entitylabel][version] = "1.4"

projects[ctools][subdir] = "contrib"
projects[ctools][version] = "1.12"
projects[ctools][patch][] = "https://www.drupal.org/files/issues/2067997-reload-plugins-class-7.patch"

projects[composer_manager][subdir] = "contrib"
projects[composer_manager][version] = "1.8"

projects[date][subdir] = "contrib"
projects[date][version] = "2.10"

projects[diff][subdir] = "contrib"
projects[diff][version] = "3.2"

projects[email][subdir] = "contrib"
projects[email][version] = "1.3"

projects[entity][subdir] = "contrib"
projects[entity][version] = "1.8"

projects[entitycache][subdir] = "contrib"
projects[entitycache][version] = 1.2

projects[entityreference][subdir] = "contrib"
projects[entityreference][version] = "1.5"

projects[entity_validator][subdir] = "contrib"
projects[entity_validator][version] = "1.2"

projects[facetapi][subdir] = "contrib"
projects[facetapi][version] = "1.5"

projects[features][subdir] = "contrib"
projects[features][version] = "2.10"

projects[flag][subdir] = "contrib"
projects[flag][version] = "3.9"

projects[jquery_update][subdir] = "contrib"
projects[jquery_update][version] = "2.4"

projects[libraries][subdir] = "contrib"
projects[libraries][version] = "2.3"

projects[mailsystem][version] = 2.34
projects[mailsystem][subdir] = "contrib"

projects[message][subdir] = "contrib"
projects[message][version] = "1.12"

projects[message_notify][subdir] = "contrib"
projects[message_notify][version] = "2.5"

projects[mimemail][version] = 1.0-beta3
projects[mimemail][subdir] = "contrib"

projects[module_filter][subdir] = "contrib"
projects[module_filter][version] = 2.1

projects[og][subdir] = "contrib"
projects[og][version] = 2.x

projects[restful][subdir] = "contrib"
projects[restful][version] = "1.8"

projects[search_api][subdir] = "contrib"
projects[search_api][version] = "1.12"

projects[search_api_solr][subdir] = "contrib"
projects[search_api_solr][version] = "1.5"

projects[strongarm][subdir] = "contrib"
projects[strongarm][version] = "2.0"

projects[title][subdir] = "contrib"
projects[title][version] = "1.0-alpha7"

projects[token][subdir] = "contrib"
projects[token][version] = "1.7"

projects[views][subdir] = "contrib"
projects[views][version] = "3.18"

projects[views_bulk_operations][subdir] = "contrib"
projects[views_bulk_operations][version] = "3.4"

; Libraries
; For the 'hedley_faker' module.
libraries[faker][download][type] = get
libraries[faker][download][url] = https://github.com/fzaninotto/Faker/archive/v1.6.0.tar.gz

; Development
projects[devel][subdir] = "development"
projects[devel][download][type] = git
projects[devel][download][branch] = 7.x-1.x
projects[devel][download][revision] = 2f0db7d7dd41af26fdd71033a2c0258a133efef8
projects[devel][patch][] = "https://www.drupal.org/files/issues/2879092-devel-generate-php7-warning-4.patch"

projects[migrate][subdir] = "development"
projects[migrate][version] = "2.8"

projects[migrate_extras][subdir] = "development"
projects[migrate_extras][version] = 2.5
