;; ----------------------------------------------------------------------------
;; Server
;; ----------------------------------------------------------------------------
(defvar c4-server-host "localhost"
  "Default hostname of the c4 server")

(defvar c4-server-port 8000
  "Default port of the c4 server")

;; server communication
(defvar c4-process-object-name "c4-process-object"
  "Default name of a c4 server connection")

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
       (process-live-p process-object)))

(defun c4-write-request (encoded-message)
  "Send a request object to a c4 server"
  (process-send-string c4-process-object
                       (concat (c4-payload encoded-message)
                               encoded-message)))

(defun c4-payload (message)
  "Get the payload of the message as a string of bytes. The
payload is 32bit number in network byte order."
  (let ((mask-last (if (= c4-word-size 32) #x1f #xff))
        (size (string-bytes message))
        (i 1))
    (with-temp-buffer
      (while (<= i 4)
        (goto-char (point-min))
        (insert (byte-to-string
                 (logand size (if (= i 4) mask-last #xff))))
        (setq size (lsh size -8))
        (setq i (1+ i)))
      (buffer-substring-no-properties 1 (point-max)))))

;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------
(defun c4-compile-current-buffer ()
  "Send the current buffer to a c4 server."
  (interactive)
  (let ((project-id (c4-get-project-id))
        (filename (buffer-file-name))
        (unit (buffer-substring-no-properties 1 (point-max)))
        (compilation-unit ""))
    (with-temp-buffer
      ;; action
      (insert (byte-to-string c4-field-action))
      (insert (byte-to-string c4-request-action-compile))
      ;; project-id
      (insert (byte-to-string c4-field-project-id))
      (insert (c4-protobuf-encode-varint (string-bytes project-id)))
      (insert project-id)
      ;; unit
      (setq compilation-unit (c4-encode-compilation-unit filename unit))
      (insert (byte-to-string c4-field-unit))
      (insert (c4-protobuf-encode-varint (string-bytes compilation-unit)))
      (insert compilation-unit)
      ;; encoded message
      (c4-write-request (buffer-substring-no-properties 1 (point-max))))))

(defun c4-get-project-id ()
  "Get the project id of the current buffer"
  (interactive)
  ;; TODO
  "project-001")

;; ----------------------------------------------------------------------------
;; Message encoding
;; ----------------------------------------------------------------------------
;; protobuf
(defconst c4-protobuf-varint 0)
(defconst c4-protobuf-64bit 1)
(defconst c4-protobuf-length-delimited 2)
(defconst c4-protobuf-32bit 5)

;; request types
(defconst c4-request-action-index 1)
(defconst c4-request-action-project 0)
(defconst c4-request-action-compile 1)
(defconst c4-request-project-index 2)
(defconst c4-request-unit-index 3)
(defconst c4-request-unit-filename-index 1)
(defconst c4-request-unit-buffer-index 1)

(defconst c4-field-action
  (logior (lsh c4-request-action-index 3)
          c4-protobuf-varint))

(defconst c4-field-project-id
  (logior (lsh c4-request-project-index 3)
          c4-protobuf-length-delimited))

(defconst c4-field-unit
  (logior (lsh c4-request-unit-index 3)
          c4-protobuf-length-delimited))

;; emacs word size
;; most-positive-fixnum
;;   32bit 536870911
;;   64bit 2305843009213693951
(defun c4-calculate-word-size ()
  "Returns 32 or 64 word size. On a 32bit machine the max
positive number is (2^29 - 1)."
  (if (< (1+ 536870911) 0) 32 64))

(defconst c4-word-size (c4-calculate-word-size))

(defun c4-protobuf-encode-varint (int)
  "Encode a var int and returns it as a string"
  (let* ((bit-count (c4-bit-count int))
         (varint-count (ceiling (/ bit-count 7.0)))
         (tmp 0)
         (i 1))
    (with-temp-buffer
      (while (<= i varint-count)
        (setq tmp (logand int #x7F))
        (if (< i varint-count)
            (setq tmp (logior tmp #x80)))
        (insert (byte-to-string tmp))
        (setq tmp 0)
        (setq i (1+ i))
        (setq int (lsh int -7)))
      (buffer-substring-no-properties 1 (point-max)))))

(defun c4-bit-count (int)
  "Count how many bits are needed to represent the integer int."
  (if (= int 0) 0
    (let* ((bit-count (- c4-word-size 3))
           (mask (lsh 1 (1- bit-count))))
      (while (= (logand int mask) 0)
        (setq mask (lsh mask -1))
        (setq bit-count (1- bit-count)))
      bit-count)))

;; ----------------------------------------------------------------------------
;; Helper functions
;; ----------------------------------------------------------------------------
(defun c4-encode-compilation-unit (filename unit)
  "Encode the filename and unit and returns it a string"
  (with-temp-buffer
    ;; filename
    (insert (byte-to-string c4-request-unit-filename-index))
    (insert (c4-protobuf-encode-varint (string-bytes filename)))
    (insert filename)
    ;; unit
    (insert (byte-to-string c4-request-unit-buffer-index))
    (insert (c4-protobuf-encode-varint (string-bytes unit)))
    (insert unit)
    (buffer-substring-no-properties 1 (point-max))))

(provide 'c4)
