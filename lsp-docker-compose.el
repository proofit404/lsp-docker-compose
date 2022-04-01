;; lsp-docker-compose.el --- LSP docker-compose projects support.  -*- lexical-binding: t; -*-

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

(defcustom lsp-docker-compose-files '("docker-compose.yml")
  "File name of the docker-compose project file."
  :type '(repeat string)
  :safe (-andfn #'listp (-partial #'-all? #'stringp)))

(defcustom lsp-docker-compose-service-name nil
  "Default service name defined in the docker-compose project file."
  :type 'string
  :safe 'stringp)

(defun lsp-docker-compose-project ()
  (let ((project (locate-dominating-file default-directory (car lsp-docker-compose-files))))
    (when project
      (f-full project))))

(defun lsp-docker-compose-filenames (project)
  (-map (-partial #'f-join project) lsp-docker-compose-files))

(defun lsp-docker-compose-config (files)
  (yaml-parse-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (apply #'call-process "docker-compose" nil t nil (-flatten (-concat (-zip-lists (-cycle '("--file")) files) '("config"))))))
   :object-key-type 'string))

(defun lsp-docker-compose-volumes (config)
  (let ((result (ht-create)))
    (-when-let (services (ht-get config "services"))
      (seq-doseq (service (ht-items services))
        (let ((service-name (car service))
              (service-def (cadr service)))
          (seq-doseq (volume (ht-get service-def "volumes"))
            (pcase-let ((`(,local ,remote ,mode) (s-split ":" volume)))
              (if (ht-contains? result local)
                  (ht-set! (ht-get result local) service-name remote)
                (ht-set! result local (ht (service-name remote)))))))))
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

(defun lsp-docker-compose-containers (files service)
  (--map
   (car (s-split-up-to " " it 1))
   (-slice
    (s-split
     "\n"
     (s-trim
      (with-output-to-string
        (with-current-buffer
            standard-output
          (apply #'call-process "docker-compose" nil t nil (-flatten (-concat (-zip-lists (-cycle '("--file")) files) (list "ps" service))))))))
    2)))

(defun lsp-docker-compose-select-container (containers)
  (car containers))

(defun lsp-docker-compose-current-container ()
  (let ((project (lsp-docker-compose-project)))
    (when project
      (let* ((files (lsp-docker-compose-filenames project))
             (config (lsp-docker-compose-config files))
             (volumes (lsp-docker-compose-volumes config)))
        (unless (ht-empty? volumes)
          (let ((volume (lsp-docker-compose-select-volume project volumes)))
            (when volume
              (pcase-let* ((services (ht-get volumes volume))
                           (`(,service ,remote) (lsp-docker-compose-select-service services)))
                (when service
                  (let* ((containers (lsp-docker-compose-containers files service))
                         (container (lsp-docker-compose-select-container containers)))
                    (when container
                      (list container (f-join project volume) remote))))))))))))

(defun lsp-docker-compose-uri-to-path (container local remote uri)
  (let ((path (lsp--uri-to-path-1 uri)))
    (if (f-ancestor-of? remote path)
        (f-expand (f-relative path remote) local)
      (format "/docker:%s:%s" container path))))

(defun lsp-docker-compose-path-to-uri (local remote path)
  (lsp--path-to-uri-1
   (if (f-ancestor-of? local path)
       (f-expand (f-relative path local) remote)
     (user-error "The path %s is not under %s" path local))))

(defun lsp-docker-compose-activate-on (local)
  (lambda (filename _mode)
    (or (f-same? local filename) ;; FIXME: Cover dired buffer in tests.
        (f-ancestor-of? local filename))))

(defun lsp-docker-compose-execute (container command)
  `("docker" "exec" "-i" ,container ,@command))

(defun lsp-docker-compose-register (local-client container local remote)
  (let* ((client (copy-lsp--client local-client))
         (server-id (intern (concat (symbol-name (lsp--client-server-id client)) "-" (s-replace "_" "-" container))))
         (saved-command (plist-get (lsp--client-new-connection client) :saved-command))
         (command (cond
                   ((functionp saved-command) (funcall saved-command))
                   ((stringp saved-command) (list saved-command))
                   ((listp saved-command) saved-command))))
    (setf (lsp--client-server-id client) server-id
          (lsp--client-uri->path-fn client) (-partial #'lsp-docker-compose-uri-to-path container local remote)
          (lsp--client-path->uri-fn client) (-partial #'lsp-docker-compose-path-to-uri local remote)
          (lsp--client-activation-fn client) (lsp-docker-compose-activate-on local)
          (lsp--client-new-connection client) (plist-put
                                               (lsp-stdio-connection
                                                (lambda ()
                                                  (lsp-docker-compose-execute container command)))
                                               :test? (lambda (&rest _) t))
          (lsp--client-priority client) (1+ (lsp--client-priority client)))
    (lsp-register-client client)
    (add-to-list 'lsp-enabled-clients server-id)
    (message "Registered a language server with id: %s and container name: %s" server-id container)))

(defun lsp-docker-compose ()
  (lsp--require-packages)
  (if lsp-mode
      (error "docker-compose processing should happen before lsp-mode")
    (pcase-let ((`(,container ,local ,remote) (lsp-docker-compose-current-container)))
      (dolist (client (lsp--filter-clients #'lsp--supports-buffer?))
        (lsp-docker-compose-register client container local remote)))))

(defun lsp-docker-compose-hack-stdio-connection (f &rest args)
  (plist-put
   (apply f args)
   :saved-command (car args)))

(advice-add
 'lsp-stdio-connection
 :around #'lsp-docker-compose-hack-stdio-connection)

(provide 'lsp-docker-compose)

;;; lsp-docker-compose.el ends here
