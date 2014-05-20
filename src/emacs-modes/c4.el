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

(defun c4-serialize-request (request)
  "Serialize a request using protocol buffers"
  ;; TODO:
  (let (action-key action-value project-id unit)
    (setq action (logior (lsh c4-request-action-index 3)
                         c4-protobuf-length-delimited))
    (setq action-value (c4-protobuf-varint c4-request-action-compile))
    "TODO"))

;; protobuf
(defconst c4-protobuf-varint 0)
(defconst c4-protobuf-64bit 1)
(defconst c4-protobuf-length-delimited 2)
(defconst c4-protobuf-32bit 5)

;; emacs word size
;; most-positive-fixnum
;;   32bit 536870911
;;   64bit 2305843009213693951
(defun c4-calculate-word-size ()
  "Returns 32 or 64 word size. On a 32bit machine the max
positive number is (2^29 - 1)."
  (if (< (1+ 536870911) 0) 32 64))

(defconst c4-word-size (c4-calculate-word-size))

(defconst c4-msb-mask (lsh 1 7))

(defun c4-protobuf-varint (int)
  "Encode a var int"
  (let ((bit-count (c4-bit-count(int)))
        (i 0))
    ;; TODO
  ))

(defun c4-bit-count (int)
  "Count how many bits are needed to represent the integer int."
  (if (= int 0) 0
    (let* ((bit-count (- c4-word-size 3))
           (mask (lsh 1 (1- bit-count))))
      (while (= (logand int mask) 0)
        (setq mask (lsh mask -1))
        (setq bit-count (1- bit-count)))
      bit-count)))

(defun c4-byte-count (int)
  "Count how many bytes are needed to represent the integer int."
  (let ((byte-count (if (= c4-word-size 32) 4 8))
        (mask 0)
        (result 0)
        (i 1))
    (while (<= i byte-count)
      (setq mask (if (= i byte-count) #x1F #xFF))
      (if (> (logand int mask) 0)
          (setq result i))
      (setq int (lsh int -8))
      (setq i (1+ i)))
    result))

(provide 'c4)
