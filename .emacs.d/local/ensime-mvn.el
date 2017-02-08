(require 'ensime-config)
(require 'mvn)

(defgroup ensime-mvn nil
  "Support for Maven builds in ENSIME"
  :group 'ensime
  :prefix "ensime-mvn-")

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
  (ensime-mvn "clean verify"))

(defun ensime-mvn-test ()
  (interactive)
  :group 'ensime-mvn
  "Run tests on the current ensime project. This does not do a clean first"
  (ensime-mvn "test"))

(provide 'ensime-mvn)
