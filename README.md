# lsp-docker-compose

`docker-compose` support for `lsp-mode`.

## TODO:

* [ ] single line global minor mode activation
* [ ] works with every enabled local client
* [ ] register enabled lsp clients when open file in docker-compose project
* [ ] works with `:add-on?` keyword clients
* [ ] multiple `-f` options support
* [ ] `docker-compose.yml` located in upper directory than project root
* [ ] uri to path and back conversion
* [ ] don't redefine lsp service when open second file in the same project
* [ ] initialize js-mode lsp clients successfully after python-mode lsp clients was initialized before in the same workspace

## TESTS:

* [x] no docker-compose project

* [x] no services volume contains current file
* [x] single service mount volume with current file
* [x] multiple services mount volume with current file

* [x] no containers running
* [x] single container running
* [x] multiple containers running (`docker-compose up --scale app=2`)

* [ ] `.dir-locals.el` should be able to define auto select for multiple options

* [ ] nested paths (not only `.` in mounts)

* [ ] `docker/compose.yml` project path
