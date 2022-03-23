(require 'lsp-docker-compose)

(setq python-indent-guess-indent-offset-verbose nil)

(defmacro with-temp-project (project &rest body)
  (declare (indent 1))
  `(let* ((template-project-directory (f-expand (f-join "testing" ,project)))
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
     (unwind-protect
         (progn
           (shell-command "docker-compose up -d")
           ,@body)
       (shell-command "docker-compose down")
       (shell-command "docker-compose container prune -f")
       (shell-command "docker-compose volume prune -f")
       (shell-command "docker-compose network prune -f")
       (shell-command "docker-compose image prune -f"))))

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
        (shell-command "docker-compose up --scale app=3")
        (expect (lsp-docker-compose-current-container)
                :to-equal `("f_app_1" ,project-directory "/app")))))

  (it "multiple services node anchors and references"
    (with-temp-running-project "h"
      (with-current-buffer (find-file-noselect "src/app.py")
        (expect (let ((completing-read-function
                       (lambda (prompt collection &rest _)
                         (car (last collection)))))
                  (lsp-docker-compose-current-container))
                :to-equal `("h_jobs_1" ,project-directory "/app"))))))

;;; test-lsp-docker-compose.el ends here