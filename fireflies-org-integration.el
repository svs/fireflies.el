;; -*- lexical-binding: t -*-
;;; fireflies-org-integration.el --- Integrate Fireflies transcripts with org-mode  -*- lexical-binding: t; -*-

;; Author: Your Name
;; Keywords: calendar, convenience
;; Package-Requires: ((emacs "27.1") (fireflies "0.1") (gptel "0.1"))

;;; Commentary:
;; This package integrates Fireflies meeting transcripts with org-mode files.
;; It can automatically extract action items from transcripts and add them
;; as TODOs to dedicated org files for each meeting.

;;; Code:

(require 'fireflies)
(require 'org)
(require 'gptel nil t) ;; Optional dependency

(defgroup fireflies-org nil
  "Integration between Fireflies and org-mode."
  :group 'fireflies)

(defcustom fireflies-org-todos-directory (expand-file-name "fireflies-todos" user-emacs-directory)
  "Directory to store TODOs extracted from Fireflies transcripts."
  :type 'directory
  :group 'fireflies-org)

(defcustom fireflies-org-add-to-agenda nil
  "Whether to automatically add the TODOs directory to `org-agenda-files'."
  :type 'boolean
  :group 'fireflies-org)

(defcustom fireflies-org-debug-directory (expand-file-name "fireflies-debug" user-emacs-directory)
  "Directory to store debug files from Fireflies."
  :type 'directory
  :group 'fireflies-org)

(defcustom fireflies-org-todo-prompt
  "Here is a transcript. I (svs, recruiting@svs) would like you to extract from this transcript a set of tasks for me. Here is the order of importance
- Most important
  - introducing candidates to clients.
  - meeting new clients
  - invoice/contract follow up
- Everything else (we don't need to show this).

* TODO [Assignee] Some task
"
  "Prompt for GPT to extract TODOs from transcript."
  :type 'string
  :group 'fireflies-org)

(defun fireflies-org--sanitize-filename (name)
  "Sanitize NAME to be used as a filename."
  (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "_" name))

(defun fireflies-org--ensure-todos-directory ()
  "Ensure the TODOs directory exists."
  (unless (file-exists-p fireflies-org-todos-directory)
    (make-directory fireflies-org-todos-directory t))
  (when fireflies-org-add-to-agenda
    (unless (member fireflies-org-todos-directory org-agenda-files)
      (add-to-list 'org-agenda-files fireflies-org-todos-directory))))

(defun fireflies-org--get-todo-file-path (transcript)
  "Get the file path for storing TODOs for TRANSCRIPT."
  (fireflies-org--ensure-todos-directory)
  (let* ((title (or (alist-get 'title transcript) "Untitled Meeting"))
         (date (fireflies-format-date (alist-get 'date transcript)))
         (cal-id (alist-get 'cal_id transcript))
         (sanitized-title (fireflies-org--sanitize-filename title))
         (filename (concat date "-" sanitized-title ".org")))
    (expand-file-name filename fireflies-org-todos-directory)))

(defun fireflies-org--extract-transcript-text (transcript)
  "Extract readable text from TRANSCRIPT for analysis."
  (message "Extracting text from transcript: %s" (or (alist-get 'title transcript) "Untitled"))
  (let* ((sentences (alist-get 'sentences transcript))
         (title (alist-get 'title transcript))
         (date (alist-get 'date transcript))
         (context-intro (format "Transcript from meeting: '%s' on %s\n\n" 
                               (or title "Untitled") 
                               (fireflies-format-date date)))
         (result context-intro))
    
    ;; Check for sentences in the transcript
    (if (and sentences (> (length sentences) 0))
        (let ((i 0)
              (len (length sentences))
              (current-speaker nil))
          (while (< i len)
            (let* ((sentence (aref sentences i))
                   (speaker-name (alist-get 'speaker_name sentence))
                   (text (alist-get 'raw_text sentence)))
              (when (and text (not (string-empty-p text)))
                (when (not (equal speaker-name current-speaker))
                  (setq current-speaker speaker-name)
                  (setq result (concat result "\n\n" (or speaker-name "Unknown") ": ")))
                (setq result (concat result text " "))))
            (setq i (1+ i))))
      ;; If no sentences found, try to extract text from the buffer
      (when-let* ((buffer-name (format "*Fireflies - Transcript: %s*" 
                                     (or title "Untitled")))
                  (buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            ;; Skip past header to transcript section
            (when (search-forward "* Transcript" nil t)
              (forward-line 2) ;; Skip past the Transcript header and blank line
              ;; Collect all text from this point
              (let ((transcript-text (buffer-substring-no-properties (point) (point-max))))
                (setq result (concat result transcript-text))))))))
    
    ;; Add fallback message if no text was found
    (when (string= result context-intro)
      (setq result (concat result "\n\nNo transcript content available")))
    
    (message "Extracted transcript text length: %d characters" (length result))
    result))

(defun fireflies-org-create-or-update-todo-file (transcript todo-text)
  "Create or update TODO file for TRANSCRIPT with TODO-TEXT."
  (let ((file-path (fireflies-org--get-todo-file-path transcript))
        (title (or (alist-get 'title transcript) "Untitled Meeting"))
        (date (fireflies-format-date (alist-get 'date transcript)))
        (cal-id (alist-get 'cal_id transcript)))
    
    ;; Create new file or update existing
    (with-current-buffer (find-file-noselect file-path)
      (let ((exists (> (buffer-size) 0)))
        (unless exists
          ;; New file - add headers
          (insert (format "#+TITLE: %s\n" title))
          (insert (format "#+DATE: %s\n" date))
          (when cal-id
            (insert (format "#+PROPERTY: GCAL_ID %s\n" cal-id)))
          (insert "\n* Meeting TODOs\n"))
        
        ;; Add todos
        (goto-char (point-max))
        (unless (eolp) (insert "\n"))
        (insert todo-text)
        (save-buffer))
      
      ;; Return the file path
      file-path)))

;; Buffer-local variable to store transcript data
(defvar-local fireflies-current-transcript nil
  "The currently displayed transcript data.")

(defun fireflies-org-generate-todos (transcript)
  "Generate TODOs from TRANSCRIPT and save to dedicated org file."
  (interactive)
  (let ((transcript-to-use transcript))
    ;; If called interactively without transcript, try to get from buffer-local variable
    (when (and (not transcript-to-use) (called-interactively-p 'any))
      (setq transcript-to-use (when (boundp 'fireflies-current-transcript) 
                               fireflies-current-transcript)))
    
    ;; Handle case when still no transcript
    (unless transcript-to-use
      (error "No transcript data available"))
    
    ;; Extract text and generate TODOs
    (let* ((transcript-text (fireflies-org--extract-transcript-text transcript-to-use)))
      
      ;; Save transcript content to debug file for inspection
      (let ((debug-file (expand-file-name "fireflies-transcript-debug.txt" 
                                         fireflies-org-debug-directory)))
        ;; Ensure debug directory exists
        (unless (file-exists-p fireflies-org-debug-directory)
          (make-directory fireflies-org-debug-directory t))
        
        (with-temp-file debug-file
          (insert "TRANSCRIPT CONTENT SENT TO GPT:\n\n")
          (insert transcript-text)
          (insert "\n\n=====================================\n")
          (insert "INSTRUCTIONS SENT TO GPT:\n\n")
          (insert fireflies-org-todo-prompt))
        (message "Saved transcript content and instructions for debugging to %s (length: %d chars)"
                debug-file (length transcript-text)))
      
      (if (not (fboundp 'gptel-request))
          (message "GPTel not available. Please install it to extract TODOs")
        (message "Generating TODOs...")
        
        (gptel-request
         (concat transcript-text "\n\n" fireflies-org-todo-prompt)
         :callback
         (lambda (response info)
           (when response
             (let ((file-path (fireflies-org-create-or-update-todo-file transcript-to-use response)))
               (message "TODOs saved to %s" file-path)
               (find-file file-path)))))))))

;;;###autoload
(defun fireflies-org-setup ()
  "Set up integration between Fireflies and org-mode."
  (interactive)
  (message "Setting up Fireflies org integration")
  (fireflies-org--ensure-todos-directory)
  (remove-hook 'fireflies-after-transcript-load-hook #'fireflies-org-add-todo-button)
  (add-hook 'fireflies-after-transcript-load-hook #'fireflies-org-add-todo-button)
  (message "Fireflies org integration enabled"))

;;;###autoload
(defun fireflies-org-add-todo-button (transcript)
  "Add a button to generate TODOs for the displayed TRANSCRIPT."
  (message "Adding TODO button for transcript")
  (when (and transcript (get-buffer (format "*Fireflies - Transcript: %s*" 
                                           (or (alist-get 'title transcript) "Untitled"))))
    (with-current-buffer (format "*Fireflies - Transcript: %s*" 
                                (or (alist-get 'title transcript) "Untitled"))
      (let ((inhibit-read-only t))
        (save-excursion
          ;; Position at the start of the buffer
          (goto-char (point-min))
          ;; Move down a few lines to place button after the headers
          (forward-line 3)
          (insert "\n")
          ;; Store transcript data in a button property to avoid closure issues
          (let ((btn (insert-text-button "[Generate TODOs]"
                             'transcript transcript
                             'follow-link t
                             'help-echo "Generate TODOs from this transcript")))
            ;; Define the action after the button is created
            (button-put btn 'action (lambda (button)
                                      (fireflies-org-generate-todos 
                                       (button-get button 'transcript)))))
          (insert "\n"))))
    (message "TODO button added successfully")))

(provide 'fireflies-org-integration)
;;; fireflies-org-integration.el ends here
