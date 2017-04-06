(require 'ensime-config)
(require 'mvn)

(defgroup ensime-mvn nil
  "Support for Maven builds in ENSIME"
  :group 'ensime
  :prefix "ensime-mvn-")

(setq ensime-mvn-regen-task "ensime:generate -U")
(setq ensime-mvn-verify-task "clean verify -e -U")
(setq ensime-mvn-test-task "test -U")

(defun ensime-configured-project-root () 
  (interactive)
  (ensime--get-root-dir (-> (ensime-connection) ensime-config)))

(defun ensime-mvn (&optional task args)
  "Run mvn in the project root directory"
  (interactive)
  (let ((default-directory (ensime-configured-project-root)))
        (if default-directory
            (let ((task (or task (mvn-get-task default-directory))))
              (setq mvn-last-task task)
              (compile (concat mvn-command " " task " " args)))
        (message "Couldn't find a maven project"))))

        
(defun ensime-mvn-verify ()
  (interactive)
  "Run clean verify on the current ensime project"
  (ensime-mvn ensime-mvn-verify-task))

(defun ensime-mvn-test ()
  (interactive)
  :group 'ensime-mvn
  "Run tests on the current ensime project. This does not do a clean first"
  (ensime-mvn ensime-mvn-test-task))

(defun ensime-mvn-regenerate ()
  (interactive)
  :group 'ensime-mvn
  "Run tests on the current ensime project. This does not do a clean first"
  (ensime-mvn ensime-mvn-regen-task))

(provide 'ensime-mvn)
