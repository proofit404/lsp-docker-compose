# lsp-docker-compose

`docker-compose` support for `lsp-mode`.

## TODO:

* single line global minor mode activation
* works with every enabled local client
* register enabled lsp clients when open file in docker-compose project
* works with `:add-on?` keyword clients
* `docker-compose.yml` located in upper directory than project root
* `docker/compose.yml` project path
* uri to path and back conversion
* don't redefine lsp service when open second file in the same project
* initialize js-mode lsp clients successfully after python-mode lsp clients was initialized before in the same workspace

## TESTS:

* nested paths (not only `.` in mounts)
* safep custom settings
* docker-compose in absent on machine
