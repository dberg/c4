;; server settings
(defvar c4-server-host "localhost"
  "Default hostname of the c4 server")

(defvar c4-server-port 8000
  "Default port of the c4 server")

;; server communication
(defvar c4-process-object-name "c4-process-object"
  "Default name of a c4 server connection")

;; request types
(defconst request-action-project 0)
(defconst request-action-compile 1)

(defvar c4-process-object nil
  "Connection to the c4 server")

;; TODO: handle connection errors
(defun c4-connect-to-server ()
  "Connect to a c4 server using 'c4-server-object'."
  (interactive)
  (let ((name c4-process-object-name)
        (buffer "c4-buffer-output") ;; should be nil and use a filter
        (host c4-server-host)
        (service c4-server-port))
    (if (c4-connection-alivep c4-process-object)
        c4-process-object
      (setq c4-process-object
            (open-network-stream name buffer host service)))))

(defun c4-connection-alivep (process-object)
  (and (processp process-object)
       (not (process-command process-object))))

(defun c4-write-request (request)
  "Send a request object to a c4 server"
  ;; TODO
  (process-send-string c4-process-object (byte-to-string 65)))

(defun c4-create-request-project (project-id)
  "Create a request to get information about the project."
  ;; TODO
  )

(defun c4-create-request-compile (project-id)
  "Create a request to compile a list of compilation units."
  (let ((request
         '(:action request-action-compile
           :project-id project-id
           :compilation-unit (:filename TODO.java
                              :buffer "public class A {}"))))
    request))

(provide 'c4)
