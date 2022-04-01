(require 'lsp-docker-compose)

(require 'lsp-pylsp)

(setq python-indent-guess-indent-offset-verbose nil
      this-project (f-dirname (f-dirname (f-this-file))))

(defmacro with-temp-project (project &rest body)
  (declare (indent 1))
  `(let* ((template-project-directory (f-expand (f-join this-project "testing" ,project)))
          (temp-directory (make-temp-file "lsp-docker-compose" t))
          (project-directory (f-join temp-directory ,project)))
     (unwind-protect
         (let ((default-directory project-directory))
           (f-copy template-project-directory project-directory)
           ,@body)
       (f-delete temp-directory t))))

(defmacro with-temp-running-project (project &rest body)
  (declare (indent 1))
  `(with-temp-project ,project
     (let ((compose-arg (s-join " " (-flatten (-zip-lists (-cycle '("-f")) (f-glob "docker-compose*.yml"))))))
       (unwind-protect
           (progn
             (call-process-shell-command (format "docker-compose %s up -d" compose-arg))
             ,@body)
         (call-process-shell-command (format "docker-compose %s down" compose-arg))
         (call-process-shell-command "docker container prune -f")
         (call-process-shell-command "docker volume prune -f")
         (call-process-shell-command "docker network prune -f")
         (call-process-shell-command "docker image prune -f")))))

(describe "docker-compose project"
  (it "is missed"
    (with-temp-project "a"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-be nil))))

  (it "does not have services"
    (with-temp-project "b"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-be nil))))

  (it "service does not have volumes"
    (with-temp-project "c"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-be nil))))

  (it "service does not have local volumes"
    (with-temp-project "d"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-be nil))))

  (it "service local volumes does not include current file"
    (with-temp-project "e"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-be nil))))

  (it "service is not up"
    (with-temp-project "f"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-be nil))))

  (it "single service"
    (with-temp-running-project "f"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-equal `("f_app_1" ,project-directory "/app")))))

  (it "multiple services"
    (with-temp-running-project "g"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (let ((completing-read-function
                       (lambda (prompt collection &rest _)
                         (car (last collection)))))
                  (lsp-docker-compose-current-container))
                :to-equal `("g_jobs_1" ,project-directory "/app")))))

  (it "single scaled service"
    (with-temp-running-project "f"
      (with-current-buffer (find-file-noselect "src/app.py")
        (call-process-shell-command "docker-compose up --scale app=3")
        (expect (lsp-docker-compose-current-container)
                :to-equal `("f_app_1" ,project-directory "/app")))))

  (it "multiple services node anchors and references"
    (with-temp-running-project "h"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (let ((completing-read-function
                       (lambda (prompt collection &rest _)
                         (car (last collection)))))
                  (lsp-docker-compose-current-container))
                :to-equal `("h_jobs_1" ,project-directory "/app")))))

  (it "multiple services directory locals"
    (with-temp-running-project "i"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-equal `("i_jobs_1" ,project-directory "/app")))))

  (it "unknown service directory locals"
    (with-temp-running-project "j"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (lsp-docker-compose-current-container)
                :to-throw))))

  (it "multiple project files"
    (with-temp-running-project "k"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (let ((completing-read-function
                       (lambda (prompt collection &rest _)
                         (car (last collection)))))
                  (lsp-docker-compose-current-container))
                :to-equal `("k_jobs_1" ,project-directory "/app"))))))

(describe "server id"
  (it "made of container and volume names"
    (let ((client (ht-get lsp-clients 'pylsp)))
      (expect (symbol-name (lsp-docker-compose-server-id "/home/coder/f" client))
              :to-equal "pylsp-docker-compose-home-coder-f"))))

(describe "uri to path"
  (it "remote to local"
    (let ((f (lsp-docker-compose-uri-to-path "f_app_1" "/home/coder/f" "/app")))
      (expect (funcall f "file:///app/src/app.py")
              :to-equal "/home/coder/f/src/app.py")))

  (it "not matched"
    (let ((f (lsp-docker-compose-uri-to-path "f_app_1" "/home/coder/f" "/app")))
      (expect (funcall f "file:///venv/lib/code.py")
              :to-equal "/docker:f_app_1:/venv/lib/code.py"))))

(describe "path to uri"
  (it "local to remote"
    (let ((f (lsp-docker-compose-path-to-uri "/home/coder/f" "/app")))
      (expect (funcall f "/home/coder/f/src/app.py")
              :to-equal "file:///app/src/app.py")))

  (it "not matched"
    (let ((f (lsp-docker-compose-path-to-uri "/home/coder/f" "/app")))
      (expect (funcall f "/home/coder/g/src/app.py")
              :to-throw))))

(describe "activation function"
  (it "inside volume"
    (with-temp-project "f"
      (let ((f (lsp-docker-compose-activate-on project-directory)))
        (with-current-buffer (find-file-noselect "src/app.py")
          (expect (funcall f (buffer-file-name) major-mode)
                  :to-be t)))))

  (it "outside volume"
    (with-temp-project "f"
      (let ((f (lsp-docker-compose-activate-on project-directory)))
        (with-temp-project "g"
          (with-current-buffer (find-file-noselect "src/app.py")
            (expect (funcall f (buffer-file-name) major-mode)
                    :to-be nil)))))))

(describe "stdio connection"
  (it "copy client command"
    (let ((client (ht-get lsp-clients 'pylsp)))
      (expect (lsp-docker-compose-execute "f_app_1" client)
              :to-equal '("docker" "exec" "-i" "f_app_1" "pylsp")))))

;;; test-lsp-docker-compose.el ends here
