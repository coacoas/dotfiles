(require 'ensime-config)
(require 'ensime-inspector)
(require 'mvn)

(defgroup ensime-mvn nil
  "Support for Maven builds in ENSIME"
  :group 'ensime
  :prefix "ensime-mvn-")

(setq ensime-mvn-refresh-task "org.ensime.maven.plugins:ensime-maven:0.0.5:generate -U")
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

(defun ensime-mvn-test-package ()
  (interactive)
  :group 'ensime-mvn
  (let ((pack (ensime-package-at-point)))
    (ensime-mvn (concat ensime-mvn-test-task " -DwildcardSuites=" pack))))

(defun refresh-f (process event) 
  (if (= 0 (process-exit-status process))
    (ensime-reload))
  (message "ENSIME refresh failed: compilation `%s`", event))

(defun ensime-mvn-refresh ()
  (interactive)
  :group 'ensime-mvn
  "Rebuilds the .ensime configuration file" 
  (let ((compile (ensime-mvn ensime-mvn-refresh-task)))
    (set-process-sentinel (get-process "compilation") 'refresh-f)))

(provide 'ensime-mvn)
