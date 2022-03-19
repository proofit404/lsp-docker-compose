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

* [ ] no docker-compose project

* [ ] no services volume contains current file
* [ ] single service mount volume with current file
* [ ] multiple services mount volume with current file

* [ ] no containers running
* [ ] single container running
* [ ] multiple containers running (`docker-compose up --scale app=2`)

* [ ] `.dir-locals.el` should be able to define auto select for multiple options
