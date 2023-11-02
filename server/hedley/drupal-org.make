core = 7.x
api = 2

; Modules
projects[admin_menu][subdir] = "contrib"
projects[admin_menu][version] = "3.0-rc6"

projects[admin_views][subdir] = "contrib"
projects[admin_views][version] = "1.7"

projects[advancedqueue][subdir] = "contrib"
projects[advancedqueue][version] = "1.x-dev"
projects[advancedqueue][patch][] = "https://www.drupal.org/files/issues/2019-11-03/2981791-10-stuck-items_0.patch"
projects[advancedqueue][patch][] = "https://www.drupal.org/files/issues/2019-03-12/3039333-2-view-improvements.patch"
; Have AdvancedQueue::createItem() return the item_id
projects[advancedqueue][patch][] = "https://www.drupal.org/files/issues/2020-01-29/return-item-id-from-create-item-0.patch"

projects[auto_entitylabel][subdir] = "contrib"
projects[auto_entitylabel][version] = "1.4"

projects[ctools][subdir] = "contrib"
projects[ctools][version] = "1.15"
projects[ctools][patch][] = "https://www.drupal.org/files/issues/2067997-reload-plugins-class-7.patch"

projects[composer_manager][subdir] = "contrib"
projects[composer_manager][version] = "1.8"

projects[date][subdir] = "contrib"
projects[date][version] = "2.13"

projects[diff][subdir] = "contrib"
projects[diff][version] = "3.2"

projects[email][subdir] = "contrib"
projects[email][version] = "1.3"

projects[entity][subdir] = "contrib"
projects[entity][version] = "1.9"

projects[entitycache][subdir] = "contrib"
projects[entitycache][version] = 1.2

projects[entityreference][subdir] = "contrib"
projects[entityreference][version] = "1.5"
projects[entityreference][patch][] = "https://gist.githubusercontent.com/AronNovak/28c7ea79823db3da7cf3ee77c4799577/raw/4cf996dbf56486f5d36eff5105646e31402625fa/entityreference-devel-generate-speedup.patch"

projects[entity_validator][subdir] = "contrib"
projects[entity_validator][version] = "1.2"

projects[facetapi][subdir] = "contrib"
projects[facetapi][version] = "1.5"

projects[features][subdir] = "contrib"
projects[features][version] = "2.11"

projects[flag][subdir] = "contrib"
projects[flag][version] = "3.9"

projects[jquery_update][subdir] = "contrib"
projects[jquery_update][version] = "2.4"

projects[libraries][subdir] = "contrib"
projects[libraries][version] = "2.5"

projects[logs_rollbar][type] = "module"
projects[logs_rollbar][subdir] = "contrib"
projects[logs_rollbar][download][type] = "git"
projects[logs_rollbar][download][branch] = "master"
projects[logs_rollbar][download][url] = "https://github.com/Gizra/logs_rollbar.git"
projects[logs_rollbar][download][revision] = 8248ae1780c0608bf7a44a7c35cc4faa69f433cb

projects[mailsystem][version] = 2.34
projects[mailsystem][subdir] = "contrib"

projects[message][subdir] = "contrib"
projects[message][version] = "1.12"

projects[message_notify][subdir] = "contrib"
projects[message_notify][version] = "2.5"

projects[mimemail][version] = 1.1
projects[mimemail][subdir] = "contrib"

projects[module_filter][subdir] = "contrib"
projects[module_filter][version] = 2.2

projects[og][subdir] = "contrib"
projects[og][version] = 2.11

projects[prepopulate][subdir] = "contrib"
projects[prepopulate][version] = 2.1
projects[prepopulate][patch][] = "https://www.drupal.org/files/issues/prepopulate-2679839-55.patch"

projects[restful][subdir] = "contrib"
projects[restful][version] = "1.10"

projects[search_api][subdir] = "contrib"
projects[search_api][version] = "1.12"

projects[search_api_solr][subdir] = "contrib"
projects[search_api_solr][version] = "1.5"

projects[smtp][subdir] = "contrib"
projects[smtp][version] = "1.7"

projects[strongarm][subdir] = "contrib"
projects[strongarm][version] = "2.0"

projects[title][subdir] = "contrib"
projects[title][version] = "1.0-beta4"

projects[token][subdir] = "contrib"
projects[token][version] = "1.7"

projects[views][subdir] = "contrib"
projects[views][version] = "3.27"

projects[views_bulk_operations][subdir] = "contrib"
projects[views_bulk_operations][version] = "3.5"

projects[vbo_export][subdir] = "contrib"
projects[vbo_export][version] = "1.3"

projects[views_data_export][subdir] = "contrib"
projects[views_data_export][version] = "3.2"

projects[views_litepager][subdir] = "contrib"
projects[views_litepager][version] = "3.0"

; Libraries
; For the 'hedley_faker' module.
libraries[faker][download][type] = get
libraries[faker][download][url] = https://github.com/fzaninotto/Faker/archive/v1.6.0.tar.gz

; Development
projects[devel][subdir] = "development"
projects[devel][version] = "1.7"

projects[migrate][subdir] = "development"
projects[migrate][version] = "2.11"

projects[migrate_extras][subdir] = "development"
projects[migrate_extras][version] = "2.5"
