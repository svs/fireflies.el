;; -*- lexical-binding: t -*-
(require 'request)
(require 'json)
(require 'auth-source)

;; Hooks for integration with other packages

(defvar fireflies-anon-mode nil
  "When non-nil, anonymize names in transcripts and TODO files for demo/recording purposes.")

(defun fireflies-toggle-demo-mode ()
  "Toggle anonymization of names for demo/recording purposes."
  (interactive)
  (setq fireflies-anon-mode (not fireflies-anon-mode))
  (message "Fireflies demo mode %s" (if fireflies-anon-mode "enabled" "disabled"))

  ;; Refresh transcript list if open
  (fireflies-refresh-transcript-list-if-exists)

  ;; Offer to refresh open transcript buffers
  (when (y-or-n-p "Refresh open transcript buffers? ")
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (boundp 'fireflies-current-transcript)
                   fireflies-current-transcript)
          (fireflies-display-transcript fireflies-current-transcript)))))

  ;; Update TODO file overlays
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-match-p "fireflies-todos" buffer-file-name))
        (if fireflies-anon-mode
            (when (fboundp 'fireflies-org--anonymize-todo-buffer)
              (fireflies-org--anonymize-todo-buffer))
          (when (fboundp 'fireflies-org--clear-anon-overlays)
            (fireflies-org--clear-anon-overlays)))))))
(defvar fireflies-after-transcript-load-hook nil
  "Hook run after a transcript is loaded and displayed.
The hook functions are called with the transcript data as argument.")

(defvar fireflies-before-transcripts-render-hook nil
  "Hook run before transcripts are rendered.
Extensions can use this to modify `tabulated-list-format' and `tabulated-list-entries'.
Functions in this hook are called with no arguments in the transcript list buffer.")

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

(defconst fireflies-transcripts-buffer-name "*Fireflies Transcripts*"
  "Buffer name for the transcripts list.")

(defconst fireflies-transcript-buffer-name-format "*Fireflies - Transcript: %s*"
  "Format string for transcript buffer names. %s will be replaced with the title.")

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
    (when

	(file-exists-p cache-file)
      (with-temp-buffer
	(insert-file-contents cache-file)
	(read (current-buffer))))))

(defun fireflies-cache-load-transcript (transcript-id)
  (let ((cache-file (expand-file-name transcript-id fireflies-cache-directory)))
    (when (file-exists-p cache-file)
      (with-temp-buffer
	(insert-file-contents cache-file)
	(read (current-buffer))))))

(defun fireflies-clear-cache ()
  "Clear all cached Fireflies transcripts."
  (interactive)
  (when (file-exists-p fireflies-cache-directory)
    (dolist (file (directory-files fireflies-cache-directory t "^[^.].*"))
      (delete-file file))
    (message "Fireflies cache cleared"))
  (unless (file-exists-p fireflies-cache-directory)
    (message "Fireflies cache directory does not exist")))

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
  (message "starting graphql query")
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
                      ))))
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
  (let* ((title (or (alist-get 'title transcript) "Untitled"))
         (display-title (if fireflies-anon-mode
                           (fireflies-anonymize-title title)
                         title))
         (buffer (get-buffer-create (format fireflies-transcript-buffer-name-format display-title)))
         (inhibit-read-only t)
         (sentences (alist-get 'sentences transcript)))
    (with-current-buffer buffer
      ;; Store transcript data in buffer-local variable
      (setq-local fireflies-current-transcript transcript)

      (erase-buffer)
      (when (fboundp 'org-mode)
        (insert (format "#+TITLE: %s\n" display-title))
        (insert (format "#+DATE: %s\n\n" (fireflies-format-date (alist-get 'date transcript)))))
      (unless (fboundp 'org-mode)
        (insert (format "Title: %s\n" display-title))
        (insert (format "Date: %s\n\n" (fireflies-format-date (alist-get 'date transcript)))))

      ;; Add instructions for TODO generation
      (insert "Press 't' to generate TODOs from this transcript\n\n")

      ;; Add conversation
      (insert "* Transcript\n\n")
      (if sentences
          (let ((current-speaker nil)
                (speaker-map (make-hash-table :test 'equal))
                (speaker-counter 0)
                (i 0)
                (len (length sentences)))
            (while (< i len)
              (let* ((sentence (aref sentences i))
                     (speaker-name (alist-get 'speaker_name sentence))
                     (text (alist-get 'raw_text sentence)))
                (when (and text (not (string-empty-p text)))
                  (when (not (equal speaker-name current-speaker))
                    (setq current-speaker speaker-name)
                    (let ((display-name (if fireflies-anon-mode
                                           (let ((result (fireflies--get-anonymous-speaker-name
                                                         speaker-name speaker-map speaker-counter)))
                                             (setq speaker-counter (cdr result))
                                             (car result))
                                         (or speaker-name "Unknown"))))
                      (insert (format "\n*%s*: " display-name))))
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


(defun fireflies-view-transcript (transcript-id)
  "View the transcript by ID, delegating display to async loader."
  (fireflies-transcript transcript-id))

(defun fireflies-transcript (transcript-id)
  (let ((cached-transcript (fireflies-cache-load-transcript transcript-id)))
    (if cached-transcript (fireflies-display-transcript cached-transcript) (fireflies-get-transcript transcript-id (lambda (t) (fireflies-display-transcript t))))))

(defun fireflies-get-transcript (transcript-id &optional callback)
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
      ;; Minimal refresh: rebuild entries without stealing focus
      (when (get-buffer fireflies-transcripts-buffer-name)
        (fireflies-list-transcripts fireflies-transcript-list t))
       (if callback
	   (funcall callback transcript))
       ))))

(defvar fireflies-transcripts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'fireflies-view-transcript-at-point)
    (define-key map (kbd "g") 'fireflies-get-transcripts)
    (define-key map (kbd "s") 'fireflies-search)
    (define-key map (kbd "/") 'fireflies-search)
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

(defun fireflies-with-transcript (transcript-id callback)
  "Ensure TRANSCRIPT-ID is available, then CALLBACK with the transcript alist.
Callback is called with one argument: the transcript alist. Caching is
handled internally; callers need not care whether it came from cache or API."
  (let ((cached (fireflies-cache-load-transcript transcript-id)))
    (if cached
        (funcall callback cached)
      (fireflies-get-transcript
       transcript-id
       (lambda (transcript)
         (funcall callback transcript))))))



(defun fireflies-refresh-transcript-list-if-exists ()
  "Refresh the transcript list buffer if it exists."
  (when (get-buffer fireflies-transcripts-buffer-name)
    (with-current-buffer fireflies-transcripts-buffer-name
      (tabulated-list-print t))))

(defun fireflies-view-transcript-at-point ()
  "View the transcript at point."
  (interactive)
  (let* ((id (tabulated-list-get-id)))
    (when id
      (fireflies-view-transcript id))))

(defun fireflies-anonymize-title (title)
  "Anonymize TITLE based on keywords for demo mode."
  (let ((lower-title (downcase title)))
    (cond
     ((string-match-p "candidate\\|interview\\|screening" lower-title)
      "Candidate Interview")
     ((string-match-p "client\\|customer" lower-title)
      "Client Meeting")
     ((string-match-p "important\\|urgent\\|critical\\|exec\\|leadership" lower-title)
      "Important Meeting")
     (t "Team Meeting"))))

(defun fireflies--get-anonymous-speaker-name (speaker-name speaker-map speaker-counter)
  "Get or create anonymous name for SPEAKER-NAME using SPEAKER-MAP.
SPEAKER-COUNTER is current counter value. Returns cons cell (display-name . new-counter)."
  (let ((cached-name (gethash speaker-name speaker-map)))
    (if cached-name
        (cons cached-name speaker-counter)
      ;; Create new anonymous name
      (let* ((new-counter (1+ speaker-counter))
             (anon-name (format "Speaker %c" (+ ?A (1- new-counter)))))
        (puthash speaker-name anon-name speaker-map)
        (cons anon-name new-counter)))))

(defun fireflies-list-transcripts (transcripts &optional no-switch)
  "Display TRANSCRIPTS in a tabulated list.
When NO-SWITCH is non-nil, do not switch to the list buffer."
  (setq fireflies-transcript-list transcripts)
  (let ((buffer (get-buffer-create fireflies-transcripts-buffer-name)))
    (with-current-buffer buffer
      (fireflies-transcripts-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (transcript)
                      (let* ((id (alist-get 'id transcript))
                             (raw-title (alist-get 'title transcript))
                             (title (if fireflies-anon-mode
                                       (fireflies-anonymize-title raw-title)
                                     raw-title))
                             (date (fireflies-format-date (alist-get 'date transcript)))
                             (cached (file-exists-p (expand-file-name id fireflies-cache-directory)))
                             (display-title (propertize title 'face (if cached 'font-lock-keyword-face '(:foreground "gray50"))))
                             (display-date (propertize date 'face '(:foreground "gray50"))))
                        (list id (vector display-date display-title id))))
                    transcripts))
      ;; Run hook to allow extensions to modify format and entries
      (run-hooks 'fireflies-before-transcripts-render-hook)
      (tabulated-list-print t)
      (unless no-switch (switch-to-buffer buffer)))))

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

(defun fireflies-search-by-title (keyword callback &optional limit)
  "Search transcripts by KEYWORD in titles and call CALLBACK with results."
  (let ((limit-val (or limit 50)))
    (fireflies-graphql-query
     "query SearchByTitle($keyword: String!, $scope: String!, $limit: Int) {
        transcripts(keyword: $keyword, scope: $scope, limit: $limit) {
          id
          title
          date
        }
      }"
     `((keyword . ,keyword) (scope . "TITLE") (limit . ,limit-val))
     callback)))

(defun fireflies-search-by-participant (email callback &optional limit)
  "Search transcripts by participant EMAIL and call CALLBACK with results."
  (let ((limit-val (or limit 50)))
    (fireflies-graphql-query
     "query SearchByParticipant($email: [String]!, $limit: Int) {
        transcripts(attendee_emails: $email, limit: $limit) {
          id
          title
          date
        }
      }"
     `((email . [,email]) (limit . ,limit-val))
     callback)))

(defvar fireflies-search-results nil
  "Cached search results for completion.")

(defun fireflies-format-transcript-for-completion (transcript)
  "Format TRANSCRIPT for use in completing-read."
  (let ((date (fireflies-format-date (alist-get 'date transcript)))
        (title (or (alist-get 'title transcript) "Untitled"))
        (id (alist-get 'id transcript)))
    (cons (format "%s - %s" date title) id)))

(defun fireflies-search ()
  "Search Fireflies transcripts with completion interface."
  (interactive)
  (let* ((search-type (completing-read "Search by: " '("Title" "Participant") nil t))
         (search-term (cond
                       ((string= search-type "Title")
                        (read-string "Search titles for keyword: "))
                       ((string= search-type "Participant")
                        (read-string "Search for participant email: "))))
         (search-fn (if (string= search-type "Title")
                        #'fireflies-search-by-title
                      #'fireflies-search-by-participant)))

    (message "Searching transcripts...")
    (funcall search-fn search-term
             (lambda (result)
               (let ((transcripts (alist-get 'transcripts result)))
                 (if transcripts
                     (let* ((candidates (mapcar #'fireflies-format-transcript-for-completion transcripts))
                            (choice (completing-read "Select transcript: " candidates nil t)))
                       (when choice
                         (let ((transcript-id (cdr (assoc choice candidates))))
                           (fireflies-transcript transcript-id))))
                   (message "No transcripts found matching your search")))))))

(provide 'fireflies)
