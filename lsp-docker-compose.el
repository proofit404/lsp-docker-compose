;; lsp-docker-compose.el --- LSP docker-compose projects support.

;; Copyright (C) 2022 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/lsp-docker-compose
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (f "0.20.0") (ht "2.0") (lsp-mode "6.2.1") (s "1.9") (yaml "0.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'dash)
(require 'f)
(require 'ht)
(require 'lsp-mode)
(require 's)
(require 'yaml)

(defgroup lsp-docker-compose nil
  "LSP support for docker-compose projects."
  :group 'lsp)

(defcustom lsp-docker-compose-filename "docker-compose.yml"
  "File name of the docker-compose project file."
  :type 'string
  :safe 'stringp)

(defcustom lsp-docker-compose-service-name nil
  "Default service name defined in the docker-compose project file."
  :type 'string
  :safe 'stringp)

(defun lsp-docker-compose-project ()
  (let ((project (locate-dominating-file default-directory lsp-docker-compose-filename)))
    (when project
      (f-full project))))

(defun lsp-docker-compose-filename (project)
  (f-join project lsp-docker-compose-filename))

(defun lsp-docker-compose-read-file (filename)
  (yaml-parse-string
   (f-read filename)
   :object-key-type 'string))

(defun lsp-docker-compose-volumes (struct)
  (let ((result (ht-create)))
    (-when-let (services (ht-get struct "services"))
      (seq-doseq (service (ht-items services))
        (let ((service-name (car service))
              (service-def (cadr service)))
          (seq-doseq (volume (ht-get service-def "volumes"))
            (when (s-starts-with-p "." volume)
              (pcase-let ((`(,local ,remote) (s-split ":" volume)))
                (if (ht-contains? result local)
                    (ht-set! (ht-get result local) service-name remote)
                  (ht-set! result local (ht (service-name remote))))))))))
    result))

(defun lsp-docker-compose-select-volume (project volumes)
  (let (paths)
    (seq-doseq (path (ht-keys volumes))
      (let ((local (f-join project path)))
        (when (or (f-same? local (buffer-file-name)) ;; FIXME: Cover dired buffer in tests.
                  (f-ancestor-of? local (buffer-file-name)))
          (push path paths))))
    (--reduce (if (> (length acc) (length it)) acc it) paths)))

(defun lsp-docker-compose-select-service (services)
  (let* ((names (-sort 's-less? (ht-keys services)))
         (name (if (null lsp-docker-compose-service-name)
                   (if (< 1 (length names))
                       (completing-read "Service: " names nil t)
                     (car names))
                 (if (-contains? names lsp-docker-compose-service-name)
                     lsp-docker-compose-service-name
                   (error "Unknown docker-compose service: %s" lsp-docker-compose-service-name)))))
    (list name (ht-get services name))))

(defun lsp-docker-compose-containers (filename service)
  (--map
   (car (s-split-up-to " " it 1))
   (-slice
    (s-split
     "\n"
     (s-trim
      (with-output-to-string
        (with-current-buffer
            standard-output
          (call-process "docker-compose" nil t nil "--file" filename "ps" service)))))
    2)))

(defun lsp-docker-compose-select-container (containers)
  (if (< 1 (length containers))
      (completing-read "Container: " containers nil t)
    (car containers)))

(defun lsp-docker-compose-current-container ()
  (let ((project (lsp-docker-compose-project)))
    (when project
      (let* ((filename (lsp-docker-compose-filename project))
             (struct (lsp-docker-compose-read-file filename))
             (volumes (lsp-docker-compose-volumes struct)))
        (unless (ht-empty? volumes)
          (let ((volume (lsp-docker-compose-select-volume project volumes)))
            (when volume
              (pcase-let* ((services (ht-get volumes volume))
                           (`(,service ,remote) (lsp-docker-compose-select-service services)))
                (when service
                  (let* ((containers (lsp-docker-compose-containers filename service))
                         (container (lsp-docker-compose-select-container containers)))
                    (when container
                      (list container (f-join project volume) remote))))))))))))

(defun lsp-docker-compose-execute (container command)
  `("docker" "exec" "-i" ,container ,@command))

(defun lsp-docker-compose-register (local-client container local-path remote-path)
  (let ((client (copy-lsp--client local-client)))
    (setf (lsp--client-server-id client) (intern (concat (symbol-name (lsp--client-server-id client)) "-docker-compose"))
          (lsp--client-uri->path-fn client) (-partial #'lsp-docker--uri->path path-mappings docker-container-name-full)
          (lsp--client-path->uri-fn client) (-partial #'lsp-docker--path->uri path-mappings)
          (lsp--client-activation-fn client) (lsp-docker-create-activation-function-by-project-dir (lsp-workspace-root))
          (lsp--client-new-connection client) (plist-put
                                               (lsp-stdio-connection
                                                (lambda ()
                                                  (funcall #'lsp-docker-launch-existing-container
                                                           docker-container-name-full
                                                           path-mappings
                                                           docker-image-id
                                                           server-command)))
                                               :test? (lambda (&rest _) t))
          (lsp--client-priority client) (or priority (lsp--client-priority client)))
    (lsp-register-client client)
    (message "Registered a language server with id: %s and container name: %s" docker-server-id docker-container-name-full)))

(defun lsp-docker-compose ()
  (if lsp-mode
      (error "docker-compose processing should happen before lsp-mode")
    (unless (lsp-workspace-root)
      (pcase-let ((`(,container ,local ,remote) (lsp-docker-compose-current-container)))
        (destructuring
         (dolist (client (lsp--filter-clients #'lsp--supports-buffer?))
           (lsp-docker-compose-register client container local remote)))))))

(provide 'lsp-docker-compose)

;;; lsp-docker-compose.el ends here
