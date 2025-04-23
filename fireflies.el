;; -*- lexical-binding: t -*-
(require 'request)
(require 'json)
(require 'auth-source)

;; Hooks for integration with other packages
(defvar fireflies-after-transcript-load-hook nil
  "Hook run after a transcript is loaded and displayed.
The hook functions are called with the transcript data as argument.")

(defgroup fireflies nil
  "Fireflies GraphQL API client."
  :group 'external)

(defcustom fireflies-api-endpoint "https://api.fireflies.ai/graphql"
  "GraphQL endpoint for Fireflies API."
  :type 'string
  :group 'fireflies)

(defcustom fireflies-cache-directory (expand-file-name ".fireflies" user-emacs-directory)
  "Directory for caching fireflies transcripts"
  :type 'directory
  :group 'fireflies)

(defun fireflies-ensure-cache-directory ()
  "Ensure the cache directory exists"
  (unless (file-exists-p fireflies-cache-directory)
    (make-directory fireflies-cache-directory t)))

(defun fireflies-cache-file ()
  (expand-file-name "transcripts.el" fireflies-cache-directory))

(defun fireflies-cache-transcripts (transcripts)
  (fireflies-ensure-cache-directory)
  (with-temp-file (fireflies-cache-file)
    (prin1 transcripts (current-buffer))))

(defun fireflies-cache-transcript (transcript id)
  (fireflies-ensure-cache-directory)
  (with-temp-file (expand-file-name id fireflies-cache-directory)
    (prin1 transcript (current-buffer))))


(defun fireflies-cache-load-transcripts ()
  (let ((cache-file (fireflies-cache-file)))
    (when (file-exists-p cache-file)
      (with-temp-buffer
	(insert-file-contents cache-file)
	(read (current-buffer))))))

(defun fireflies-cache-load-transcript (transcript-id)
  (let ((cache-file (expand-file-name transcript-id fireflies-cache-directory)))
    (when (file-exists-p cache-file)
      (with-temp-buffer
	(insert-file-contents cache-file)
	(read (current-buffer))))))

(defun fireflies ()
  (interactive)
  (let ((cached-transcripts (fireflies-cache-load-transcripts)))
    (if cached-transcripts (fireflies-list-transcripts cached-transcripts)
      (fireflies-get-transcripts))))



(defvar fireflies-transcript-list nil
  "List of transcript data from Fireflies.")

(defun fireflies-get-api-token ()
  "Get Fireflies API token from auth-source."
  (let ((auth-info (auth-source-search :host "fireflies.ai" :user "api" :max 1)))
    (when auth-info
      (let ((secret (plist-get (car auth-info) :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun fireflies-humanize-endpoint (endpoint)
  "Create a human-readable name from ENDPOINT."
  (let ((name (replace-regexp-in-string "^https?://\\(api\\.\\)?\\|/graphql$" "" endpoint)))
    (replace-regexp-in-string "\\." "-" name)))

(defun fireflies-get-buffer-name ()
  "Get the buffer name for Fireflies output."
  (format "*Fireflies - %s*" (fireflies-humanize-endpoint fireflies-api-endpoint)))

(defun fireflies-display-result (data)
  "Display DATA in a formatted buffer."
  (let ((buffer (get-buffer-create (fireflies-get-buffer-name))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (propertize "Fireflies API Response\n" 'face 'bold))
      (insert (propertize (format "Endpoint: %s\n\n" fireflies-api-endpoint) 
                         'face 'italic))
      (insert (pp-to-string data))
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun fireflies-graphql-query (query variables &optional callback-fn)
  "Send QUERY with VARIABLES to Fireflies GraphQL API.
If CALLBACK-FN is provided, call it with the result data."
  (let ((api-token (fireflies-get-api-token)))
    (unless api-token
      (user-error "No Fireflies API token found in auth-source"))
    
    (request
     fireflies-api-endpoint
     :type "POST"
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " api-token)))
     :data (json-encode `((query . ,query)
                          (variables . ,variables)))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (let ((result (assoc-default 'data data)))
                   (if callback-fn
                       (funcall callback-fn result)
                     (fireflies-display-result result)))))
     :error (cl-function
             (lambda (&key error-thrown response data &allow-other-keys)
               (let ((buffer (get-buffer-create (fireflies-get-buffer-name))))
                 (with-current-buffer buffer
                   (read-only-mode -1)
                   (erase-buffer)
                   (insert (propertize "Fireflies API Error\n" 'face '(:foreground "red" :weight bold)))
                   (insert (format "Error: %s\n" error-thrown))
                   (when response
                     (insert (format "Status: %s\n" (request-response-status-code response))))
                   (when data
                     (insert (format "Response data: %s\n" (prin1-to-string data))))
                   (read-only-mode 1))
                 (display-buffer buffer)))))))

(defun fireflies-format-date (date)
  "Format DATE for display, handling both strings and timestamps."
  (condition-case nil
      (cond
       ;; If it's a number (timestamp in milliseconds)
       ((numberp date)
        (format-time-string "%Y-%m-%d" (seconds-to-time (/ date 1000.0))))
       ;; If it's a string (ISO date)
       ((stringp date)
        (format-time-string "%Y-%m-%d" (date-to-time date)))
       ;; Otherwise, just return as string
       (t (format "%s" date)))
    (error (format "%s" date))))

;; Define variable for transcript mode first to avoid forward reference
(defvar fireflies-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "t") 'fireflies-org-generate-todos)
    map)
  "Keymap for Fireflies transcript buffers.")

(define-derived-mode fireflies-transcript-mode org-mode "Fireflies Transcript"
  "Major mode for viewing Fireflies transcripts."
  (read-only-mode 1))

;; Define buffer-local variable for transcript data
(defvar-local fireflies-current-transcript nil
  "The currently displayed transcript data in a buffer.")

(defun fireflies-display-transcript (transcript)
  "Display TRANSCRIPT in a formatted buffer."
  (let ((buffer (get-buffer-create (format "*Fireflies - Transcript: %s*" 
                                          (or (alist-get 'title transcript) "Untitled"))))
        (inhibit-read-only t)
        (sentences (alist-get 'sentences transcript)))
    (with-current-buffer buffer
      ;; Store transcript data in buffer-local variable
      (setq-local fireflies-current-transcript transcript)
      
      (erase-buffer)
      (when (fboundp 'org-mode)
        (insert (format "#+TITLE: %s\n" (or (alist-get 'title transcript) "Untitled")))
        (insert (format "#+DATE: %s\n\n" (fireflies-format-date (alist-get 'date transcript)))))
      (unless (fboundp 'org-mode)
        (insert (format "Title: %s\n" (or (alist-get 'title transcript) "Untitled")))
        (insert (format "Date: %s\n\n" (fireflies-format-date (alist-get 'date transcript)))))
      
      ;; Add instructions for TODO generation
      (insert "Press 't' to generate TODOs from this transcript\n\n")
      
      ;; Add conversation
      (insert "* Transcript\n\n")
      (if sentences
          (let ((current-speaker nil)
                (i 0)
                (len (length sentences)))
            (while (< i len)
              (let* ((sentence (aref sentences i))
                     (speaker-name (alist-get 'speaker_name sentence))
                     (text (alist-get 'raw_text sentence)))
                (when (and text (not (string-empty-p text)))
                  (when (not (equal speaker-name current-speaker))
                    (setq current-speaker speaker-name)
                    (insert (format "\n*%s*: " (or speaker-name "Unknown"))))
                  (insert text " ")))
              (setq i (1+ i))))
        (insert "No transcript content available"))
      
      (goto-char (point-min))
      ;; Apply the mode after inserting content
      (if (fboundp 'org-mode)
          (fireflies-transcript-mode)
        (special-mode)))
    (display-buffer buffer)
    
    ;; Run hooks with transcript data
    (run-hook-with-args 'fireflies-after-transcript-load-hook transcript)))


(defun fireflies-transcript (transcript-id)
  (let ((cached-transcript (fireflies-cache-load-transcript transcript-id)))
    (if cached-transcript (fireflies-display-transcript cached-transcript) (fireflies-get-transcript transcript-id))))

(defun fireflies-get-transcript (transcript-id)
  "Get a specific transcript by TRANSCRIPT-ID."
  (fireflies-graphql-query
   "query GetTranscript($id: String!) {
      transcript(id: $id) {
        id
        title
        date
        cal_id
        sentences {
          raw_text
          speaker_name
        }
      }
    }"
   `((id . ,transcript-id))
   (lambda (result)
     (when-let ((transcript (alist-get 'transcript result)))
       (fireflies-cache-transcript transcript transcript-id)
       (fireflies-display-transcript transcript)))))

(defvar fireflies-transcripts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'fireflies-view-transcript-at-point)
    (define-key map (kbd "g") 'fireflies-get-transcripts)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for Fireflies transcripts list mode.")

(define-derived-mode fireflies-transcripts-mode tabulated-list-mode "Fireflies Transcripts"
  "Major mode for listing Fireflies transcripts."
  (setq tabulated-list-format [("Date" 12 t)
                              ("Title" 50 t)
                              ("ID" 36 nil)])
  (setq tabulated-list-sort-key '("Date" . t)) ;; Newest first
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun fireflies-view-transcript-at-point ()
  "View the transcript at point."
  (interactive)
  (let* ((id (tabulated-list-get-id)))
    (when id
      (fireflies-transcript id))))

(defun fireflies-list-transcripts (transcripts)
  "Display TRANSCRIPTS in a tabulated list."
  (setq fireflies-transcript-list transcripts)
  (let ((buffer (get-buffer-create "*Fireflies Transcripts*")))
    (with-current-buffer buffer
      (fireflies-transcripts-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (transcript)
                      (let ((id (alist-get 'id transcript))
                            (title (alist-get 'title transcript))
                            (date (fireflies-format-date (alist-get 'date transcript))))
                        (list id (vector date title id))))
                    transcripts))
      (tabulated-list-print t)
      (switch-to-buffer buffer))))

(defun fireflies-get-transcripts (&optional limit)
  "Get recent transcripts with optional LIMIT."
  (interactive "P")
  (message "loading transcripts from fireflies")
  (let ((limit-val (or limit 50)))
    (fireflies-graphql-query
     "query GetTranscripts($limit: Int) {
        transcripts(limit: $limit) {
          id
          title
          date
        }
      }"
     `((limit . ,limit-val))
     (lambda (result)
       (when-let ((transcripts (alist-get 'transcripts result)))
	 (fireflies-cache-transcripts transcripts)
         (fireflies-list-transcripts transcripts))))))

(provide 'fireflies)
