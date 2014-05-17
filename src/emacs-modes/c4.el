;; server settings
(defvar c4-server-host "localhost"
  "Default hostname of the c4 server")

(defvar c4-server-port 8000
  "Default port of the c4 server")

;; server communication
(defvar c4-process-object-name "c4-process-object"
  "Default name of a c4 server connection")

;; request types
(defconst c4-request-action-index 1)
(defconst c4-request-action-project 0)
(defconst c4-request-action-compile 1)

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

(defun c4-compile-current-buffer ()
  "Send the current buffer to a c4 server."
  (interactive)
  (let* ((project-id (c4-get-project-id))
         (filename (buffer-file-name))
         (buffer (buffer-substring-no-properties 1 (point-max)))
         (request (c4-create-request-compile
                   project-id
                   filename
                   buffer)))
    ;; TODO:
    ;;(c4-write-request request)
    (c4-write-request buffer)))

(defun c4-get-project-id ()
  "Get the project id of the current buffer"
  ;; TODO
  "project-001")

(defun c4-connection-alivep (process-object)
  (and (processp process-object)
       (not (process-command process-object))))

(defun c4-write-request (request)
  "Send a request object to a c4 server"
  ;; TODO
  (process-send-string c4-process-object request))

(defun c4-create-request-project (project-id)
  "Create a request to get information about the project."
  ;; TODO
  )

(defun c4-create-request-compile (project-id filename buffer)
  "Create a request to compile a list of compilation units."
  (let ((request
         '(:action request-action-compile
           :project-id project-id
           :compilation-unit (:filename filename
                              :buffer buffer))))
    (c4-serialize-request request)))

(defconst c4-protobuf-varint 0)
(defconst c4-protobuf-64bit 1)
(defconst c4-protobuf-length-delimited 2)
(defconst c4-protobuf-32bit 5)

(defun c4-serialize-request (request)
  "Serialize a request using protocol buffers"
  ;; TODO:
  (let (action-key action-value project-id unit)
    (setq action (logior (lsh c4-request-action-index 3)
                         c4-protobuf-length-delimited))
    (setq action-value (c4-protobuf-varint c4-request-action-compile))
    "TODO"))

;; most-positive-fixnum
;; 64 bit machine: 2305843009213693951 (60 bits)
(defconst c4-varint-mask (lsh 1 7))

(defun c4-protobuf-varint (i)
  "Encode a var int"
  (let ((hob (c4-hob(i))))
    ;; TODO
  ))

(defun c4-hob (i)
  "Get the highest order bit"
  (let ((result 1))
    (if (= i 0)
        0
      (while (> i 0)
        (setq i (lsh i -1))
        (setq result (lsh result 1)))
      result)))

;; TODO: emacs uses 30 bits on 32bit machines and 60 bits on 64bit machines.
(defun c4-leading-zeros (i)
  "Get the number of leading zeros of an integer."
  (if (eq i 0)
      32
    (let ((n 0))
      (if (<= i #x0000FFFF)
          (progn
            (setq n (+ n 16))
            (setq i (lsh i 16))))
      (if (<= i #x00FFFFFF)
          (progn
            (setq n (+ n 8))
            (setq i (lsh i 8))))
      (if (<= i #x0FFFFFFF)
          (progn
            (setq n (+ n 4))
            (setq i (lsh i 4))))
      (if (<= i #x3FFFFFFF)
          (progn
            (setq n (+ n 2))
            (setq i (lsh i 2))))
      (if (<= i #x7FFFFFFF)
          (setq n (+ n 1)))
      i)))

(provide 'c4)
