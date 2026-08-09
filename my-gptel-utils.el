;;; my-gptel-utils.el --- Personal gptel utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `my/gptel-ask-buffer' starts or continues a dedicated gptel chat for
;; asking questions about the current buffer.  The source is supplied as live,
;; buffer-local context in the chat, so this utility does not modify the
;; global `gptel-context'.  Pluggable providers can add budgeted xref and LSP
;; relationships, including Python imports and clangd/ccls include graphs.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'xref)
(require 'gptel)
(require 'gptel-context)

(declare-function lsp-feature? "lsp-mode" (feature &optional workspace))
(declare-function lsp-request-async
                  "lsp-mode" (method params callback &rest args))

(defgroup my/gptel-ask nil
  "Ask gptel questions with automatically collected code context."
  :group 'gptel)

(defcustom my/gptel-ask-default-profile 'smart
  "Default context collection profile for `my/gptel-ask-buffer'.

`source-only' includes only the source buffer.  `smart' also includes
direct dependencies and symbol relationships.  `deep' additionally
asks providers for broader relationships such as references and calls."
  :type '(choice (const source-only) (const smart) (const deep))
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-snippet-radius 24
  "Number of lines to include on each side of a related location."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-max-auto-sources 12
  "Maximum number of automatically collected context sources."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-max-auto-chars 100000
  "Approximate maximum characters of automatically collected context."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-max-source-chars 40000
  "Maximum characters accepted from one automatic context source."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-max-python-imports 12
  "Maximum number of Python import locations resolved per question."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-max-xrefs 40
  "Maximum number of xref locations converted to context candidates."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-collection-timeout 8
  "Seconds to wait for context providers before sending the question."
  :type 'number
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-cache-ttl 30
  "Seconds an automatic context collection remains reusable.

Set this to zero to disable collection caching."
  :type 'number
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-cache-max-entries 32
  "Maximum context collections cached in one source buffer."
  :type 'natnum
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-include-external-files nil
  "When non-nil, allow automatic context from outside the source project."
  :type 'boolean
  :group 'my/gptel-ask)

(defcustom my/gptel-ask-excluded-path-regexp
  (rx "/" (or ".git" ".hg" ".svn" ".venv" "venv" "node_modules"
               "site-packages" "__pycache__" "build" "dist") "/")
  "Regexp matching paths excluded from automatic context."
  :type '(choice (const nil) regexp)
  :group 'my/gptel-ask)

(cl-defstruct
    (my/-gptel-ask-context-candidate
     (:constructor my/-gptel-ask-make-context-candidate)
     (:copier nil)
     (:conc-name my/-gptel-ask-context-candidate-))
  "A context source proposed by a context provider."
  source relation lines score origin)

(cl-defstruct
    (my/-gptel-ask-context-provider
     (:constructor my/-gptel-ask-make-context-provider)
     (:copier nil)
     (:conc-name my/-gptel-ask-context-provider-))
  "A registered context provider."
  name priority predicate collect)

(cl-defstruct
    (my/-gptel-ask-context-request
     (:constructor my/-gptel-ask-make-context-request)
     (:copier nil)
     (:conc-name my/-gptel-ask-context-request-))
  "A single automatic context collection request."
  source-buffer source-point profile project-root generation)

(defvar my/-gptel-ask-context-providers nil
  "Registered context providers, ordered by decreasing priority.")

(defvar my/-gptel-ask-provider-generation 0
  "Generation incremented whenever the provider registry changes.")

(cl-defun my/gptel-ask-context-candidate
    (source &key relation lines (score 50) origin)
  "Return a context candidate for SOURCE.

SOURCE is a live buffer or a file name.  RELATION describes why it is
relevant.  LINES is nil for the whole source, a single (FROM . TO)
line range, or a list of such ranges.  SCORE controls selection order,
and ORIGIN identifies the producing provider."
  (unless (or (bufferp source) (stringp source))
    (signal 'wrong-type-argument (list '(or bufferp stringp) source)))
  (when (and lines (consp lines) (integerp (car lines)))
    (setq lines (list lines)))
  (my/-gptel-ask-make-context-candidate
   :source source :relation relation :lines lines :score score :origin origin))

(cl-defun my/gptel-ask-register-context-provider
    (name &key (priority 0) predicate collect)
  "Register or replace context provider NAME.

PRIORITY orders providers, with larger values first.  Its value does
not override the scores assigned to individual candidates.

PREDICATE is called with a private request object and should return
non-nil when the provider applies.  COLLECT is called as

  (COLLECT REQUEST CALLBACK)

It must eventually call CALLBACK with a list of values created by
`my/gptel-ask-context-candidate'.  COLLECT may return a cancellation
function.  Inspect REQUEST with the public
`my/gptel-ask-context-request-*' accessors.  Providers must not modify
`gptel-context' themselves."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (unless (functionp predicate)
    (signal 'wrong-type-argument (list 'functionp predicate)))
  (unless (functionp collect)
    (signal 'wrong-type-argument (list 'functionp collect)))
  (setq my/-gptel-ask-context-providers
        (cl-delete name my/-gptel-ask-context-providers
                   :key #'my/-gptel-ask-context-provider-name))
  (push (my/-gptel-ask-make-context-provider
         :name name :priority priority :predicate predicate :collect collect)
        my/-gptel-ask-context-providers)
  (setq my/-gptel-ask-context-providers
        (sort my/-gptel-ask-context-providers
              (lambda (left right)
                (> (my/-gptel-ask-context-provider-priority left)
                   (my/-gptel-ask-context-provider-priority right)))))
  (cl-incf my/-gptel-ask-provider-generation)
  name)

(defun my/gptel-ask-unregister-context-provider (name)
  "Unregister context provider NAME."
  (setq my/-gptel-ask-context-providers
        (cl-delete name my/-gptel-ask-context-providers
                   :key #'my/-gptel-ask-context-provider-name))
  (cl-incf my/-gptel-ask-provider-generation)
  name)

(defun my/gptel-ask-context-request-source-buffer (request)
  "Return the source buffer associated with provider REQUEST."
  (my/-gptel-ask-context-request-source-buffer request))

(defun my/gptel-ask-context-request-point (request)
  "Return REQUEST's source position as an integer, or nil."
  (let ((marker (my/-gptel-ask-context-request-source-point request)))
    (and (markerp marker) (marker-position marker))))

(defun my/gptel-ask-context-request-profile (request)
  "Return the collection profile associated with provider REQUEST."
  (my/-gptel-ask-context-request-profile request))

(defun my/gptel-ask-context-request-project-root (request)
  "Return the project root associated with provider REQUEST."
  (my/-gptel-ask-context-request-project-root request))

(defvar my/-gptel-ask-question-history nil
  "Minibuffer history for `my/gptel-ask-buffer'.")

(defvar-local my/-gptel-ask-source-buffer nil
  "Source buffer associated with the current gptel question buffer.")

(defvar-local my/-gptel-ask-backend nil
  "Backend associated with the current gptel question buffer.")

(defvar-local my/-gptel-ask-model nil
  "Model associated with the current gptel question buffer.")

(defvar-local my/-gptel-ask-request-active nil
  "Non-nil while this gptel question buffer has a pending request.")

(defvar-local my/-gptel-ask-base-context nil
  "Context owned by the question buffer itself.")

(defvar-local my/-gptel-ask-auto-context nil
  "Context most recently produced by automatic providers.")

(defvar-local my/-gptel-ask-manual-context nil
  "Context added manually in the question buffer.")

(defvar-local my/-gptel-ask-collection-cancel nil
  "Cancellation function for the active context collection, if any.")

(defvar-local my/-gptel-ask-collection-generation 0
  "Generation number of the latest context collection.")

(defvar-local my/-gptel-ask-context-cache nil
  "Automatic context cache owned by this source buffer.")

(defun my/gptel-ask-clear-context-cache ()
  "Clear automatic context cached for the current source buffer.

When called from a `my/gptel-ask-buffer' conversation, clear the cache
belonging to its source buffer."
  (interactive)
  (let ((source (or my/-gptel-ask-source-buffer (current-buffer))))
    (unless (buffer-live-p source)
      (user-error "No live source buffer is associated with this buffer"))
    (with-current-buffer source
      (when (hash-table-p my/-gptel-ask-context-cache)
        (clrhash my/-gptel-ask-context-cache)))
    (message "Cleared gptel ask context cache for %s" (buffer-name source))))

(defun my/-gptel-ask-cancel-collection ()
  "Cancel context collection in the current question buffer."
  (when (functionp my/-gptel-ask-collection-cancel)
    (ignore-errors (funcall my/-gptel-ask-collection-cancel)))
  (setq my/-gptel-ask-collection-cancel nil))

(defun my/-gptel-ask-project-root (buffer)
  "Return the project root for BUFFER, or its default directory."
  (with-current-buffer buffer
    (file-name-as-directory
     (file-truename
      (expand-file-name
       (if-let* ((project (project-current nil)))
           (project-root project)
         default-directory))))))

(defun my/-gptel-ask-context-source-key (source)
  "Return a stable deduplication key for context SOURCE."
  (let ((path (cond
               ((bufferp source)
                (and (buffer-live-p source)
                     (buffer-local-value 'buffer-file-name source)))
               ((stringp source) source))))
    (if path
        (condition-case nil
            (file-truename (expand-file-name path))
          (error (expand-file-name path)))
      source)))

(defun my/-gptel-ask-context-entry-owned-p (entry owned)
  "Return non-nil if context ENTRY is present in OWNED."
  (cl-member entry owned :test #'equal))

(defun my/-gptel-ask-install-context (auto-context)
  "Install AUTO-CONTEXT in the current question buffer.

Context not owned by this utility is retained as manual context."
  (let ((owned (append my/-gptel-ask-base-context
                       my/-gptel-ask-auto-context)))
    (setq my/-gptel-ask-manual-context
          (cl-remove-if
           (lambda (entry)
             (my/-gptel-ask-context-entry-owned-p entry owned))
           gptel-context))
    (setq my/-gptel-ask-auto-context auto-context)
    (setq gptel-context
          (cl-remove-duplicates
           (append my/-gptel-ask-base-context
                   my/-gptel-ask-auto-context
                   my/-gptel-ask-manual-context)
           :test #'equal))))

(defun my/-gptel-ask-normalize-lines (lines)
  "Normalize and merge one-based inclusive line ranges LINES."
  (when lines
    (when (and (consp lines) (integerp (car lines)))
      (setq lines (list lines)))
    (let ((sorted
           (sort
            (cl-loop for range in lines
                     when (and (consp range)
                               (integerp (car range))
                               (integerp (cdr range)))
                     collect (cons (max 1 (min (car range) (cdr range)))
                                   (max 1 (max (car range) (cdr range)))))
            (lambda (left right) (< (car left) (car right)))))
          merged)
      (dolist (range sorted)
        (if (and merged (<= (car range) (1+ (cdar merged))))
            (setcdr (car merged) (max (cdar merged) (cdr range)))
          (push range merged)))
      (nreverse merged))))

(defun my/-gptel-ask-source-path (source)
  "Return SOURCE's file name, or nil."
  (cond
   ((and (bufferp source) (buffer-live-p source))
    (buffer-local-value 'buffer-file-name source))
   ((stringp source) source)))

(defun my/-gptel-ask-source-eligible-p (source request)
  "Return non-nil when SOURCE is eligible for REQUEST."
  (let ((path (my/-gptel-ask-source-path source)))
    (and
     (if (bufferp source) (buffer-live-p source) (file-readable-p source))
     (or (null path)
         (and
          (not (and my/gptel-ask-excluded-path-regexp
                    (string-match-p my/gptel-ask-excluded-path-regexp
                                    (expand-file-name path))))
          (or my/gptel-ask-include-external-files
              (file-in-directory-p
               (condition-case nil
                   (file-truename (expand-file-name path))
                 (error (expand-file-name path)))
               (my/-gptel-ask-context-request-project-root request))))))))

(defun my/-gptel-ask-prefer-buffer (source)
  "Return an unmodified visiting buffer for SOURCE when available."
  (if (or (bufferp source) (not (stringp source)))
      source
    (or (find-buffer-visiting
         source (lambda (buffer) (not (buffer-modified-p buffer))))
        source)))

(defun my/-gptel-ask-source-size (source lines)
  "Return the number of characters selected from SOURCE by LINES."
  (condition-case nil
      (let* ((visiting
              (and (stringp source)
                   (find-buffer-visiting
                    source (lambda (buffer) (not (buffer-modified-p buffer))))))
             (buffer (or (and (bufferp source) source) visiting)))
        (cond
         ((and (null lines) buffer)
          (with-current-buffer buffer
            (save-restriction (widen) (buffer-size))))
         ((null lines)
          (file-attribute-size (file-attributes source)))
         (t
          (let ((temporary (null buffer)))
            (unless buffer
              (setq buffer (generate-new-buffer " *my-gptel-context-size*"))
              (with-current-buffer buffer (insert-file-contents source)))
            (unwind-protect
                (with-current-buffer buffer
                  (save-restriction
                    (widen)
                    (cl-loop for (from . to) in lines
                             sum (max
                                  0
                                  (- (save-excursion
                                       (goto-char (point-min))
                                       (forward-line to)
                                       (point))
                                     (save-excursion
                                       (goto-char (point-min))
                                       (forward-line (1- from))
                                       (point)))))))
              (when temporary (kill-buffer buffer)))))))
    (error most-positive-fixnum)))

(defun my/-gptel-ask-candidates-to-context (candidates request)
  "Normalize CANDIDATES for REQUEST and return gptel context entries."
  (let ((source-key
         (my/-gptel-ask-context-source-key
          (my/-gptel-ask-context-request-source-buffer request)))
        (table (make-hash-table :test #'equal)))
    (dolist (candidate candidates)
      (when (and (my/-gptel-ask-context-candidate-p candidate)
                 (my/-gptel-ask-source-eligible-p
                  (my/-gptel-ask-context-candidate-source candidate) request))
        (let* ((source (my/-gptel-ask-context-candidate-source candidate))
               (key (my/-gptel-ask-context-source-key source))
               (lines
                (my/-gptel-ask-normalize-lines
                 (my/-gptel-ask-context-candidate-lines candidate)))
               (existing (gethash key table)))
          (unless (equal key source-key)
            (if existing
                (progn
                  (setf (my/-gptel-ask-context-candidate-score existing)
                        (max (my/-gptel-ask-context-candidate-score existing)
                             (my/-gptel-ask-context-candidate-score candidate)))
                  (setf (my/-gptel-ask-context-candidate-lines existing)
                        (if (or (null lines)
                                (null (my/-gptel-ask-context-candidate-lines
                                       existing)))
                            nil
                          (my/-gptel-ask-normalize-lines
                           (append
                            (my/-gptel-ask-context-candidate-lines existing)
                            lines)))))
              (setf (my/-gptel-ask-context-candidate-source candidate)
                    (my/-gptel-ask-prefer-buffer source))
              (setf (my/-gptel-ask-context-candidate-lines candidate) lines)
              (puthash key candidate table))))))
    (let ((ordered nil) (total 0) (count 0) result)
      (maphash (lambda (_ candidate) (push candidate ordered)) table)
      (setq ordered
            (sort ordered
                  (lambda (left right)
                    (> (my/-gptel-ask-context-candidate-score left)
                       (my/-gptel-ask-context-candidate-score right)))))
      (dolist (candidate ordered)
        (let* ((source (my/-gptel-ask-context-candidate-source candidate))
               (lines (my/-gptel-ask-context-candidate-lines candidate))
               (size (my/-gptel-ask-source-size source lines)))
          (when (and (< count my/gptel-ask-max-auto-sources)
                     (<= size my/gptel-ask-max-source-chars)
                     (<= (+ total size) my/gptel-ask-max-auto-chars))
            (push (if lines (cons source (list :lines lines)) source) result)
            (cl-incf count)
            (cl-incf total size))))
      (nreverse result))))

(defun my/-gptel-ask-run-async-tasks (tasks callback)
  "Run asynchronous TASKS and call CALLBACK with their combined results.

Each task is called with a one-shot callback and may return a
cancellation function."
  (if (null tasks)
      (progn (funcall callback nil) nil)
    (let ((remaining (length tasks))
          (results nil)
          (cancellers nil)
          (finished nil))
      (dolist (task tasks)
        (let ((called nil))
          (condition-case _err
              (let ((cancel
                     (funcall
                      task
                      (lambda (values)
                        (unless called
                          (setq called t)
                          (when (listp values)
                            (setq results (nconc results values)))
                          (cl-decf remaining)
                          (when (and (zerop remaining) (not finished))
                            (setq finished t)
                            (funcall callback results)))))))
                (when (functionp cancel) (push cancel cancellers)))
            ((error quit)
             (unless called
               (setq called t)
               (cl-decf remaining)
               (when (and (zerop remaining) (not finished))
                 (setq finished t)
                 (funcall callback results)))))))
      (lambda (&optional flush)
        (unless finished
          (setq finished t)
          (dolist (cancel cancellers)
            (ignore-errors (funcall cancel)))
          (when flush (funcall callback results)))))))

(defun my/-gptel-ask-applicable-providers (request)
  "Return registered providers applicable to REQUEST."
  (unless (eq (my/-gptel-ask-context-request-profile request) 'source-only)
    (cl-remove-if-not
     (lambda (provider)
       (condition-case nil
           (funcall (my/-gptel-ask-context-provider-predicate provider)
                    request)
         (error nil)))
     my/-gptel-ask-context-providers)))

(defun my/-gptel-ask-cache-key (request)
  "Return a context cache key for REQUEST."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request))
        (marker (my/-gptel-ask-context-request-source-point request)))
    (with-current-buffer buffer
      (list (buffer-chars-modified-tick)
            (and (markerp marker) (marker-position marker))
            (my/-gptel-ask-context-request-profile request)
            my/-gptel-ask-provider-generation
            (when (fboundp 'my/-gptel-ask-lsp-server-ids)
              (condition-case nil
                  (sort (mapcar (lambda (server-id)
                                  (format "%s" server-id))
                                (my/-gptel-ask-lsp-server-ids buffer))
                        #'string-lessp)
                (error nil)))))))

(defun my/-gptel-ask-cached-entry-live-p (entry)
  "Return non-nil when context ENTRY still names a usable source."
  (let ((source (if (consp entry) (car entry) entry)))
    (if (bufferp source)
        (buffer-live-p source)
      (and (stringp source) (file-readable-p source)))))

(defun my/-gptel-ask-cache-get (request)
  "Return (t . CONTEXT) when REQUEST has a valid cached collection."
  (when (> my/gptel-ask-cache-ttl 0)
    (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
      (with-current-buffer buffer
        (when (hash-table-p my/-gptel-ask-context-cache)
          (let* ((key (my/-gptel-ask-cache-key request))
                 (entry (gethash key my/-gptel-ask-context-cache)))
            (when entry
              (if (> (- (float-time) (car entry)) my/gptel-ask-cache-ttl)
                  (remhash key my/-gptel-ask-context-cache)
                (cons t
                      (cl-remove-if-not
                       #'my/-gptel-ask-cached-entry-live-p
                       (copy-sequence (cdr entry))))))))))))

(defun my/-gptel-ask-cache-put (request context)
  "Cache collected CONTEXT for REQUEST."
  (when (and (> my/gptel-ask-cache-ttl 0)
             (> my/gptel-ask-cache-max-entries 0))
    (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (hash-table-p my/-gptel-ask-context-cache)
            (setq my/-gptel-ask-context-cache
                  (make-hash-table :test #'equal)))
          (when (>= (hash-table-count my/-gptel-ask-context-cache)
                    my/gptel-ask-cache-max-entries)
            (clrhash my/-gptel-ask-context-cache))
          (puthash (my/-gptel-ask-cache-key request)
                   (cons (float-time) (copy-sequence context))
                   my/-gptel-ask-context-cache))))))

(defun my/-gptel-ask-collect-context (request callback)
  "Collect context for REQUEST and call CALLBACK with gptel entries.

Return a function that cancels all unfinished providers."
  (if-let* ((cached (my/-gptel-ask-cache-get request)))
      (progn (funcall callback (cdr cached)) nil)
    (let* ((providers (my/-gptel-ask-applicable-providers request))
         (finished nil)
         (timed-out nil)
         (timer nil)
         (provider-cancel nil))
    (cl-labels
        ((finish
          (candidates)
          (unless finished
            (setq finished t)
            (when (timerp timer) (cancel-timer timer))
            (let ((context
                   (my/-gptel-ask-candidates-to-context
                    candidates request)))
              (unless timed-out
                (my/-gptel-ask-cache-put request context))
              (funcall callback context)))))
      (setq timer
            (run-at-time my/gptel-ask-collection-timeout nil
                         (lambda ()
                           (setq timed-out t)
                           (when (functionp provider-cancel)
                             (funcall provider-cancel t))
                           (finish nil))))
      (setq provider-cancel
            (my/-gptel-ask-run-async-tasks
             (mapcar
              (lambda (provider)
                (lambda (done)
                  (funcall
                   (my/-gptel-ask-context-provider-collect provider)
                   request done)))
              providers)
             #'finish))
      (lambda ()
        (unless finished
          (setq finished t)
          (when (timerp timer) (cancel-timer timer))
          (when (functionp provider-cancel)
            (funcall provider-cancel))))))))

(defun my/-gptel-ask-xref-item-candidate
    (item relation score origin &optional whole-source)
  "Convert xref ITEM to a context candidate.

RELATION, SCORE, and ORIGIN annotate the candidate.  WHOLE-SOURCE
requests the whole destination instead of a line window."
  (condition-case nil
      (let* ((location (xref-item-location item))
             (marker (xref-location-marker location))
             (buffer (and (markerp marker) (marker-buffer marker))))
        (when (buffer-live-p buffer)
          (let ((line (with-current-buffer buffer
                        (line-number-at-pos marker t))))
            (prog1
                (my/gptel-ask-context-candidate
                 buffer
                 :relation relation
                 :lines (unless whole-source
                          (cons (max 1 (- line my/gptel-ask-snippet-radius))
                                (+ line my/gptel-ask-snippet-radius)))
                 :score score
                 :origin origin)
              (set-marker marker nil)))))
    (error nil)))

(defun my/-gptel-ask-xref-items-candidates
    (items relation score origin &optional whole-source)
  "Convert xref ITEMS into bounded context candidates.

RELATION, SCORE, ORIGIN, and WHOLE-SOURCE annotate each candidate."
  (let (result)
    (dolist (item items)
      (when (< (length result) my/gptel-ask-max-xrefs)
        (when-let* ((candidate
                     (my/-gptel-ask-xref-item-candidate
                      item relation score origin whole-source)))
          (push candidate result))))
    (nreverse result)))

(defun my/-gptel-ask-xref-at-point (request operation relation score
                                            &optional whole-source)
  "Run xref OPERATION at REQUEST's point and return candidates."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request))
        (marker (my/-gptel-ask-context-request-source-point request)))
    (when (and (buffer-live-p buffer)
               (markerp marker)
               (marker-buffer marker))
      (with-current-buffer buffer
        (save-excursion
          (goto-char marker)
          (when-let* ((backend (xref-find-backend))
                      (identifier
                       (xref-backend-identifier-at-point backend)))
            (condition-case nil
                (my/-gptel-ask-xref-items-candidates
                 (funcall operation backend identifier)
                 relation score 'xref whole-source)
              (error nil))))))))

(defun my/-gptel-ask-xref-provider-p (request)
  "Return non-nil when xref can serve REQUEST."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request))
        (marker (my/-gptel-ask-context-request-source-point request)))
    (and (buffer-live-p buffer)
         (markerp marker)
         (marker-buffer marker)
         (with-current-buffer buffer
           (save-excursion
             (goto-char marker)
             (condition-case nil
                 (and (xref-find-backend) t)
               (error nil)))))))

(defun my/-gptel-ask-xref-collect (request callback)
  "Collect portable xref context for REQUEST, then call CALLBACK."
  (let ((result
         (my/-gptel-ask-xref-at-point
          request #'xref-backend-definitions 'definition 95)))
    (when (eq (my/-gptel-ask-context-request-profile request) 'deep)
      (setq result
            (nconc
             result
             (my/-gptel-ask-xref-at-point
              request #'xref-backend-references 'reference 45))))
    (funcall callback result)
    nil))

(defun my/-gptel-ask-python-mode-p (buffer)
  "Return non-nil when BUFFER is in a Python major mode."
  (with-current-buffer buffer
    (derived-mode-p 'python-mode 'python-ts-mode 'python-base-mode)))

(defun my/-gptel-ask-python-import-bounds ()
  "Return bounds of Python import statements in the current buffer."
  (or
   (when (and (fboundp 'treesit-parser-list)
              (fboundp 'treesit-buffer-root-node)
              (treesit-parser-list))
     (condition-case nil
         (let ((stack (list (treesit-buffer-root-node))) result)
           (while stack
             (let* ((node (pop stack))
                    (type (treesit-node-type node)))
               (if (member type '("import_statement" "import_from_statement"))
                   (push (cons (treesit-node-start node)
                               (treesit-node-end node))
                         result)
                 (dotimes (index (treesit-node-child-count node t))
                   (push (treesit-node-child node index t) stack)))))
           (sort result (lambda (left right) (< (car left) (car right)))))
       (error nil)))
   (save-excursion
     (goto-char (point-min))
     (let (result)
       (while (re-search-forward
               "^[[:space:]]*\\(?:from\\|import\\)\\_>" nil t)
         (push (cons (line-beginning-position) (line-end-position)) result))
       (nreverse result)))))

(defun my/-gptel-ask-python-import-positions ()
  "Return positions of directly imported Python module names."
  (let (positions)
    (dolist (bounds (my/-gptel-ask-python-import-bounds))
      (save-excursion
        (goto-char (car bounds))
        (cond
         ((re-search-forward
           "\\_<from\\_>[[:space:]]+\\([.]*[[:word:]_]+\\(?:\\.[[:word:]_]+\\)*\\)"
           (cdr bounds) t)
          (let ((end (match-end 1)))
            (push (save-excursion
                    (goto-char end)
                    (skip-syntax-backward "w_")
                    (point))
                  positions)))
         ((re-search-forward "\\_<import\\_>" (cdr bounds) t)
          (let ((skip-alias nil))
            (while (re-search-forward
                    "\\_<[[:word:]_]+\\(?:\\.[[:word:]_]+\\)*\\_>"
                    (cdr bounds) t)
              (let ((word (match-string-no-properties 0)))
                (cond
                 ((string= word "as") (setq skip-alias t))
                 (skip-alias (setq skip-alias nil))
                 (t
                  (push (save-excursion
                          (goto-char (match-end 0))
                          (skip-syntax-backward "w_")
                          (point))
                        positions))))))))))
    (setq positions (delete-dups (nreverse positions)))
    (if (> (length positions) my/gptel-ask-max-python-imports)
        (cl-subseq positions 0 my/gptel-ask-max-python-imports)
      positions)))

(defun my/-gptel-ask-python-provider-p (request)
  "Return non-nil when REQUEST is for Python and has an xref backend."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
    (and (buffer-live-p buffer)
         (my/-gptel-ask-python-mode-p buffer)
         (with-current-buffer buffer
           (condition-case nil (and (xref-find-backend) t) (error nil))))))

(defun my/-gptel-ask-python-collect (request callback)
  "Resolve Python imports in REQUEST through xref, then call CALLBACK."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request))
        result)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((backend (condition-case nil (xref-find-backend) (error nil))))
            (when backend
              (dolist (position (my/-gptel-ask-python-import-positions))
                (goto-char position)
                (when-let* ((identifier
                             (condition-case nil
                                 (xref-backend-identifier-at-point backend)
                               (error nil)))
                            (items
                             (condition-case nil
                                 (xref-backend-definitions backend identifier)
                               (error nil))))
                  (setq result
                        (nconc
                         result
                         (my/-gptel-ask-xref-items-candidates
                          items 'import 80 'python-imports t))))))))))
    (funcall callback result)
    nil))

(defvar my/-gptel-ask-lsp-token-counter 0
  "Counter used to make unique lsp-mode cancellation tokens.")

(defun my/-gptel-ask-lsp-active-p (buffer)
  "Return non-nil when lsp-mode can issue requests for BUFFER."
  (with-current-buffer buffer
    (and (bound-and-true-p lsp-mode)
         buffer-file-name
         (fboundp 'lsp-request-async))))

(defun my/-gptel-ask-lsp-capability-p (capability)
  "Return non-nil when lsp-mode advertises CAPABILITY.

When capability inspection is unavailable, optimistically return t and
let the request's error handler provide the fallback."
  (if (not (fboundp 'lsp-feature?))
      t
    (condition-case nil
        (and (funcall 'lsp-feature? capability) t)
      (error t))))

(defun my/-gptel-ask-lsp-field (object field)
  "Read FIELD from an LSP OBJECT represented as a hash, plist, or alist."
  (let ((keyword (intern (concat ":" field)))
        (symbol (intern field)))
    (cond
     ((hash-table-p object)
      (or (gethash field object)
          (gethash keyword object)
          (gethash symbol object)))
     ((and (listp object) (keywordp (car object)))
      (or (plist-get object keyword) (plist-get object symbol)))
     ((and (listp object) (consp (car object)))
      (or (alist-get field object nil nil #'equal)
          (alist-get keyword object)
          (alist-get symbol object))))))

(defun my/-gptel-ask-lsp-uri-to-path (uri)
  "Convert file URI to a local path, returning nil for other schemes."
  (when (and (stringp uri) (string-prefix-p "file:" uri))
    (condition-case nil
        (if (fboundp 'lsp--uri-to-path)
            (funcall 'lsp--uri-to-path uri)
          (url-unhex-string (url-filename (url-generic-parse-url uri))))
      (error nil))))

(defun my/-gptel-ask-lsp-path-to-uri (path)
  "Convert local PATH to an LSP file URI."
  (if (fboundp 'lsp--path-to-uri)
      (funcall 'lsp--path-to-uri path)
    (url-encode-url (concat "file://" (expand-file-name path)))))

(defun my/-gptel-ask-lsp-document-identifier (request)
  "Return an LSP TextDocumentIdentifier for REQUEST."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
    (with-current-buffer buffer
      (if (fboundp 'lsp--text-document-identifier)
          (funcall 'lsp--text-document-identifier)
        (list :uri (my/-gptel-ask-lsp-path-to-uri buffer-file-name))))))

(defun my/-gptel-ask-lsp-position-params (request)
  "Return LSP text-document-position parameters for REQUEST."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request))
        (marker (my/-gptel-ask-context-request-source-point request)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (if (fboundp 'lsp--text-document-position-params)
            (funcall 'lsp--text-document-position-params)
          (list
           :textDocument (my/-gptel-ask-lsp-document-identifier request)
           :position
           (list :line (1- (line-number-at-pos nil t))
                 :character (current-column))))))))

(defun my/-gptel-ask-lsp-request
    (request method params callback &optional capability)
  "Issue asynchronous LSP METHOD with PARAMS for REQUEST.

CALLBACK is called exactly once with the result or nil.  CAPABILITY, if
non-nil, is checked first.  Return a cancellation function."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request))
        (done nil)
        (token (list 'my-gptel-ask
                     (my/-gptel-ask-context-request-generation request)
                     method
                     (cl-incf my/-gptel-ask-lsp-token-counter))))
    (if (or (not (buffer-live-p buffer))
            (and capability
                 (with-current-buffer buffer
                   (not (my/-gptel-ask-lsp-capability-p capability)))))
        (progn (funcall callback nil) nil)
      (condition-case _err
          (progn
            (with-current-buffer buffer
              (funcall
               'lsp-request-async method params
               (lambda (result)
                 (unless done
                   (setq done t)
                   (funcall callback result)))
               :error-handler
               (lambda (&rest _error)
                 (unless done
                   (setq done t)
                   (funcall callback nil)))
               :cancel-token token
               :no-merge t))
            (lambda ()
              (unless done
                (setq done t)
                (when (fboundp 'lsp-cancel-request-by-token)
                  (ignore-errors
                    (funcall 'lsp-cancel-request-by-token token))))))
        ((error quit)
         (unless done
           (setq done t)
           (funcall callback nil))
         nil)))))

(defun my/-gptel-ask-lsp-sequence (value)
  "Return VALUE as a list of LSP result objects."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((or (hash-table-p value) (keywordp (car-safe value)))
    (list value))
   ((and (listp value)
         (consp (car value))
         (not (consp (caar value)))
         (not (listp (cdar value))))
    (list value))
   ((listp value) value)
   (t nil)))

(defun my/-gptel-ask-lsp-object-candidate
    (object relation score origin &optional whole-source)
  "Convert an LSP location-like OBJECT to a context candidate.

RELATION, SCORE, ORIGIN, and WHOLE-SOURCE annotate the candidate."
  (let* ((uri (or (my/-gptel-ask-lsp-field object "uri")
                  (my/-gptel-ask-lsp-field object "targetUri")))
         (range (or (my/-gptel-ask-lsp-field object "selectionRange")
                    (my/-gptel-ask-lsp-field object "targetSelectionRange")
                    (my/-gptel-ask-lsp-field object "targetRange")
                    (my/-gptel-ask-lsp-field object "range")))
         (start (my/-gptel-ask-lsp-field range "start"))
         (line (my/-gptel-ask-lsp-field start "line"))
         (path (my/-gptel-ask-lsp-uri-to-path uri)))
    (when path
      (my/gptel-ask-context-candidate
       path
       :relation relation
       :lines (unless whole-source
                (let ((line (1+ (or line 0))))
                  (cons (max 1 (- line my/gptel-ask-snippet-radius))
                        (+ line my/gptel-ask-snippet-radius))))
       :score score
       :origin origin))))

(defun my/-gptel-ask-lsp-location-candidates
    (response relation score origin &optional whole-source)
  "Convert location RESPONSE to context candidates.

RELATION, SCORE, ORIGIN, and WHOLE-SOURCE annotate each candidate."
  (delq nil
        (mapcar
         (lambda (object)
           (my/-gptel-ask-lsp-object-candidate
            object relation score origin whole-source))
         (my/-gptel-ask-lsp-sequence response))))

(defun my/-gptel-ask-lsp-uri-candidates-recursively
    (object relation score origin &optional depth)
  "Find URI-bearing objects recursively below OBJECT.

RELATION, SCORE, and ORIGIN annotate results.  DEPTH limits traversal
of server-specific response trees."
  (let ((depth (or depth 0)) result)
    (when (< depth 12)
      (when-let* ((candidate
                   (and (or (my/-gptel-ask-lsp-field object "uri")
                            (my/-gptel-ask-lsp-field object "targetUri"))
                        (my/-gptel-ask-lsp-object-candidate
                         object relation score origin))))
        (push candidate result))
      (cond
       ((hash-table-p object)
        (maphash
         (lambda (_ value)
           (setq result
                 (nconc result
                        (my/-gptel-ask-lsp-uri-candidates-recursively
                         value relation score origin (1+ depth)))))
         object))
       ((vectorp object)
        (dolist (value (append object nil))
          (setq result
                (nconc result
                       (my/-gptel-ask-lsp-uri-candidates-recursively
                        value relation score origin (1+ depth))))))
       ((and (listp object) (keywordp (car-safe object)))
        (let ((tail object))
          (while tail
            (let ((value (cadr tail)))
              (setq result
                    (nconc result
                           (my/-gptel-ask-lsp-uri-candidates-recursively
                            value relation score origin (1+ depth)))))
            (setq tail (cddr tail)))))
       ((listp object)
        (dolist (value object)
          (let ((value (if (and (consp value)
                                (or (stringp (car value))
                                    (symbolp (car value))))
                           (cdr value)
                         value)))
            (unless (or (keywordp value) (symbolp value) (stringp value))
              (setq result
                    (nconc result
                           (my/-gptel-ask-lsp-uri-candidates-recursively
                            value relation score origin (1+ depth))))))))))
    result))

(defun my/-gptel-ask-lsp-location-task
    (request method capability relation score origin)
  "Return a location task for REQUEST using METHOD.

CAPABILITY gates the request.  RELATION, SCORE, and ORIGIN annotate its
results."
  (lambda (done)
    (my/-gptel-ask-lsp-request
     request method (my/-gptel-ask-lsp-position-params request)
     (lambda (response)
       (funcall done
                (my/-gptel-ask-lsp-location-candidates
                 response relation score origin)))
     capability)))

(defun my/-gptel-ask-lsp-call-candidates (response direction)
  "Convert call hierarchy RESPONSE for DIRECTION to candidates."
  (let ((field (if (eq direction 'caller) "from" "to")) result)
    (dolist (call (my/-gptel-ask-lsp-sequence response))
      (when-let* ((item (my/-gptel-ask-lsp-field call field))
                  (candidate
                   (my/-gptel-ask-lsp-object-candidate
                    item direction 55 'lsp-call-hierarchy)))
        (push candidate result)))
    (nreverse result)))

(defun my/-gptel-ask-lsp-call-hierarchy-task (request)
  "Return a task to collect one-hop call relationships for REQUEST."
  (lambda (done)
    (let (cancellers child-cancel)
      (push
       (my/-gptel-ask-lsp-request
        request "textDocument/prepareCallHierarchy"
        (my/-gptel-ask-lsp-position-params request)
        (lambda (response)
          (if-let* ((item (car (my/-gptel-ask-lsp-sequence response))))
              (setq child-cancel
                    (my/-gptel-ask-run-async-tasks
                     (list
                      (lambda (child-done)
                        (my/-gptel-ask-lsp-request
                         request "callHierarchy/incomingCalls"
                         (list :item item)
                         (lambda (calls)
                           (funcall child-done
                                    (my/-gptel-ask-lsp-call-candidates
                                     calls 'caller)))))
                      (lambda (child-done)
                        (my/-gptel-ask-lsp-request
                         request "callHierarchy/outgoingCalls"
                         (list :item item)
                         (lambda (calls)
                           (funcall child-done
                                    (my/-gptel-ask-lsp-call-candidates
                                     calls 'callee))))))
                     done))
            (funcall done nil)))
        "callHierarchyProvider")
       cancellers)
      (lambda ()
        (dolist (cancel cancellers)
          (when (functionp cancel) (ignore-errors (funcall cancel))))
        (when (functionp child-cancel)
          (ignore-errors (funcall child-cancel)))))))

(defun my/-gptel-ask-lsp-provider-p (request)
  "Return non-nil when REQUEST can use generic lsp-mode facilities."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
    (and (buffer-live-p buffer) (my/-gptel-ask-lsp-active-p buffer))))

(defun my/-gptel-ask-lsp-collect (request callback)
  "Collect generic LSP relationships for REQUEST, then call CALLBACK."
  (let ((tasks
         (list
          (my/-gptel-ask-lsp-location-task
           request "textDocument/implementation" "implementationProvider"
           'implementation 88 'lsp)
          (my/-gptel-ask-lsp-location-task
           request "textDocument/typeDefinition" "typeDefinitionProvider"
           'type-definition 72 'lsp))))
    (when (eq (my/-gptel-ask-context-request-profile request) 'deep)
      (setq tasks
            (append tasks
                    (list (my/-gptel-ask-lsp-call-hierarchy-task request)))))
    (my/-gptel-ask-run-async-tasks tasks callback)))

(defun my/-gptel-ask-lsp-server-ids (buffer)
  "Return lsp-mode server identifiers active in BUFFER."
  (when (and (buffer-live-p buffer) (fboundp 'lsp-workspaces))
    (with-current-buffer buffer
      (delq
       nil
       (mapcar
        (lambda (workspace)
          (condition-case nil
              (cond
               ((fboundp 'lsp--workspace-server-id)
                (funcall 'lsp--workspace-server-id workspace))
               ((and (fboundp 'lsp--workspace-client)
                     (fboundp 'lsp--client-server-id))
                (funcall 'lsp--client-server-id
                         (funcall 'lsp--workspace-client workspace))))
            (error nil)))
        (funcall 'lsp-workspaces))))))

(defun my/-gptel-ask-lsp-server-p (buffer regexp)
  "Return non-nil if BUFFER has an LSP server identifier matching REGEXP."
  (cl-some
   (lambda (server-id)
     (string-match-p regexp (format "%s" server-id)))
   (my/-gptel-ask-lsp-server-ids buffer)))

(defun my/-gptel-ask-c-family-mode-p (buffer)
  "Return non-nil when BUFFER is in a C-family major mode."
  (with-current-buffer buffer
    (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode
                    'objc-mode 'cuda-mode)))

(defun my/-gptel-ask-lsp-document-link-candidates (response)
  "Convert LSP document-link RESPONSE to whole-file candidates."
  (let (result)
    (dolist (link (my/-gptel-ask-lsp-sequence response))
      (when-let* ((target (my/-gptel-ask-lsp-field link "target"))
                  (path (my/-gptel-ask-lsp-uri-to-path target)))
        (push (my/gptel-ask-context-candidate
               path :relation 'include :score 82 :origin 'c-family-lsp)
              result)))
    (nreverse result)))

(defun my/-gptel-ask-c-family-document-link-task (request)
  "Return a task resolving REQUEST's C-family includes with document links."
  (lambda (done)
    (my/-gptel-ask-lsp-request
     request "textDocument/documentLink"
     (list :textDocument (my/-gptel-ask-lsp-document-identifier request))
     (lambda (response)
       (funcall done
                (my/-gptel-ask-lsp-document-link-candidates response)))
     "documentLinkProvider")))

(defun my/-gptel-ask-c-family-switch-header-task (request)
  "Return a task resolving REQUEST's corresponding source or header."
  (lambda (done)
    (my/-gptel-ask-lsp-request
     request "textDocument/switchSourceHeader"
     (my/-gptel-ask-lsp-document-identifier request)
     (lambda (uri)
       (funcall
        done
        (when-let* ((path (my/-gptel-ask-lsp-uri-to-path uri)))
          (list (my/gptel-ask-context-candidate
                 path :relation 'source-header :score 90
                 :origin 'c-family-lsp))))))))

(defun my/-gptel-ask-c-family-provider-p (request)
  "Return non-nil when REQUEST is a C-family lsp-mode request."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
    (and (buffer-live-p buffer)
         (my/-gptel-ask-c-family-mode-p buffer)
         (my/-gptel-ask-lsp-active-p buffer))))

(defun my/-gptel-ask-c-family-collect (request callback)
  "Collect includes and source/header pairing for REQUEST, then call CALLBACK."
  (let* ((buffer (my/-gptel-ask-context-request-source-buffer request))
         (tasks (list (my/-gptel-ask-c-family-document-link-task request))))
    (when (my/-gptel-ask-lsp-server-p buffer "\\(?:clangd\\|ccls\\)")
      (setq tasks
            (append tasks
                    (list
                     (my/-gptel-ask-c-family-switch-header-task request)))))
    (my/-gptel-ask-run-async-tasks tasks callback)))

(defun my/-gptel-ask-ccls-provider-p (request)
  "Return non-nil when deep REQUEST is served by ccls."
  (let ((buffer (my/-gptel-ask-context-request-source-buffer request)))
    (and (eq (my/-gptel-ask-context-request-profile request) 'deep)
         (buffer-live-p buffer)
         (my/-gptel-ask-lsp-active-p buffer)
         (my/-gptel-ask-lsp-server-p buffer "ccls"))))

(defun my/-gptel-ask-ccls-call-task (request callee)
  "Return a ccls call-tree task for REQUEST.

When CALLEE is non-nil, collect callees; otherwise collect callers."
  (lambda (done)
    (let ((params (copy-sequence
                   (my/-gptel-ask-lsp-position-params request)))
          (relation (if callee 'callee 'caller)))
      (setq params
            (append params (list :callee callee :levels 1 :hierarchy t)))
      (my/-gptel-ask-lsp-request
       request "$ccls/call" params
       (lambda (response)
         (funcall
          done
          (my/-gptel-ask-lsp-uri-candidates-recursively
           response relation 58 'ccls)))))))

(defun my/-gptel-ask-ccls-collect (request callback)
  "Collect ccls one-hop call trees for REQUEST, then call CALLBACK."
  (my/-gptel-ask-run-async-tasks
   (list (my/-gptel-ask-ccls-call-task request nil)
         (my/-gptel-ask-ccls-call-task request t))
   callback))

(defun my/-gptel-ask-buffer-name (model source-buffer)
  "Return the question buffer name for MODEL and SOURCE-BUFFER."
  (format "*%s-%s*"
          (gptel--model-name model)
          (buffer-name source-buffer)))

(defun my/-gptel-ask-request-active-p (buffer)
  "Return non-nil when BUFFER has an active gptel request."
  (or (buffer-local-value 'my/-gptel-ask-request-active buffer)
      (cl-some
       (lambda (entry)
         (and-let* ((fsm (cadr entry))
                    (info (gptel-fsm-info fsm)))
           (eq (plist-get info :buffer) buffer)))
       gptel--request-alist)))

(defun my/-gptel-ask-request-finished (&rest _bounds)
  "Mark the current question-buffer request as finished."
  (setq my/-gptel-ask-request-active nil)
  (my/-gptel-ask-cancel-collection))

(defun my/-gptel-ask-configure-buffer
    (question-buffer source-buffer backend model)
  "Configure QUESTION-BUFFER for SOURCE-BUFFER using BACKEND and MODEL."
  (with-current-buffer question-buffer
    (when (and (buffer-live-p my/-gptel-ask-source-buffer)
               (not (eq my/-gptel-ask-source-buffer source-buffer)))
      (user-error "Question buffer %s belongs to source buffer %s"
                  (buffer-name question-buffer)
                  (buffer-name my/-gptel-ask-source-buffer)))
    (when (and my/-gptel-ask-backend
               (not (eq my/-gptel-ask-backend backend)))
      (user-error "Question buffer %s belongs to backend %s"
                  (buffer-name question-buffer)
                  (gptel-backend-name my/-gptel-ask-backend)))
    (when (and my/-gptel-ask-model
               (not (equal my/-gptel-ask-model model)))
      (user-error "Question buffer %s belongs to model %s"
                  (buffer-name question-buffer)
                  (gptel--model-name my/-gptel-ask-model)))

    ;; Create an isolated context list instead of inheriting the global one.
    (unless (local-variable-p 'gptel-context)
      (setq-local gptel-context nil))
    (unless (local-variable-p 'my/-gptel-ask-base-context)
      (setq-local my/-gptel-ask-base-context nil)
      (setq-local my/-gptel-ask-auto-context nil)
      (setq-local my/-gptel-ask-manual-context
                  (cl-remove source-buffer gptel-context :test #'eq)))
    (setq my/-gptel-ask-base-context (list source-buffer))
    (my/-gptel-ask-install-context my/-gptel-ask-auto-context)

    (setq-local my/-gptel-ask-source-buffer source-buffer)
    (setq-local my/-gptel-ask-backend backend)
    (setq-local my/-gptel-ask-model model)
    (setq-local gptel-backend backend)
    (setq-local gptel-model model)
    (setq-local gptel-use-context 'system)
    (add-hook 'gptel-post-response-functions
              #'my/-gptel-ask-request-finished nil t)
    (add-hook 'kill-buffer-hook #'my/-gptel-ask-cancel-collection nil t)))

(defun my/-gptel-ask-get-buffer
    (name source-buffer backend model)
  "Return question buffer NAME for SOURCE-BUFFER, BACKEND and MODEL."
  (let ((question-buffer (get-buffer name)))
    (when (and question-buffer
               (not (buffer-local-value 'gptel-mode question-buffer))
               (not (buffer-local-value
                     'my/-gptel-ask-source-buffer question-buffer)))
      (user-error "Buffer %s exists but is not a gptel question buffer" name))
    (unless question-buffer
      (setq question-buffer (gptel name nil nil nil)))
    (with-current-buffer question-buffer
      (unless gptel-mode
        (gptel-mode 1)))
    (my/-gptel-ask-configure-buffer
     question-buffer source-buffer backend model)
    question-buffer))

(defun my/-gptel-ask-prompt-ready-p ()
  "Return non-nil when point follows an empty gptel prompt prefix."
  (let ((prefix (gptel-prompt-prefix-string)))
    (if (string-empty-p prefix)
        (or (bobp)
            (and (>= (- (point) (point-min))
                     (length gptel-response-separator))
                 (string=
                  gptel-response-separator
                  (buffer-substring-no-properties
                   (- (point) (length gptel-response-separator))
                   (point)))))
      (and (>= (- (point) (point-min)) (length prefix))
           (string= prefix
                    (buffer-substring-no-properties
                     (- (point) (length prefix)) (point)))))))

(defun my/-gptel-ask-append-question (question)
  "Append QUESTION as a new prompt in the current gptel question buffer."
  (goto-char (point-max))
  (unless (my/-gptel-ask-prompt-ready-p)
    (insert gptel-response-separator (gptel-prompt-prefix-string)))
  (insert question))

(defun my/-gptel-ask-rollback-question (question-buffer prompt-start)
  "Remove an unsent question from QUESTION-BUFFER at PROMPT-START."
  (when (and (buffer-live-p question-buffer)
             (markerp prompt-start)
             (marker-buffer prompt-start))
    (with-current-buffer question-buffer
      (let ((inhibit-read-only t))
        (delete-region prompt-start (point-max)))
      (setq my/-gptel-ask-request-active nil)
      (setq my/-gptel-ask-collection-cancel nil)))
  (when (markerp prompt-start) (set-marker prompt-start nil)))

(defun my/-gptel-ask-send-after-context
    (question-buffer request prompt-start)
  "Collect REQUEST context and send the question in QUESTION-BUFFER."
  (let ((completed nil)
        cancel)
    (setq
     cancel
     (my/-gptel-ask-collect-context
      request
      (lambda (auto-context)
        (setq completed t)
        (when-let* ((point-marker
                     (my/-gptel-ask-context-request-source-point request)))
          (set-marker point-marker nil))
        (when (buffer-live-p question-buffer)
          (with-current-buffer question-buffer
            (when (= my/-gptel-ask-collection-generation
                     (my/-gptel-ask-context-request-generation request))
              (setq my/-gptel-ask-collection-cancel nil)
              (if (not (buffer-live-p
                        (my/-gptel-ask-context-request-source-buffer request)))
                  (progn
                    (my/-gptel-ask-rollback-question
                     question-buffer prompt-start)
                    (message "Source buffer disappeared; question not sent"))
                (my/-gptel-ask-install-context auto-context)
                (condition-case err
                    (progn
                      (goto-char (point-max))
                      (gptel-send)
                      (set-marker prompt-start nil))
                  ((error quit)
                   (my/-gptel-ask-rollback-question
                    question-buffer prompt-start)
                   (message "Could not send gptel question: %s"
                            (error-message-string err)))))))))))
    (unless completed
      (let ((provider-cancel cancel)
            (point-marker
             (my/-gptel-ask-context-request-source-point request)))
        (setq cancel
              (lambda ()
                (when (functionp provider-cancel)
                  (funcall provider-cancel))
                (when (markerp point-marker)
                  (set-marker point-marker nil))))
        (with-current-buffer question-buffer
          (setq my/-gptel-ask-collection-cancel cancel))))
    cancel))

;;;###autoload
(defun my/gptel-ask-buffer (&optional profile)
  "Ask a question about the current buffer in a dedicated gptel chat.

The chat is named `*MODEL-SOURCE-BUFFER*'.  Repeated calls reuse that
buffer and continue its conversation.  The source buffer is included
as live context through a buffer-local `gptel-context' in the chat, so
global gptel context is left unchanged.

PROFILE is one of `source-only', `smart', or `deep'.  Interactively,
use `my/gptel-ask-default-profile'; with a prefix argument, use `deep'."
  (interactive
   (list (if current-prefix-arg 'deep my/gptel-ask-default-profile)))
  (when my/-gptel-ask-source-buffer
    (user-error "Run `my/gptel-ask-buffer' from a source buffer"))
  (setq profile (or profile my/gptel-ask-default-profile))
  (unless (memq profile '(source-only smart deep))
    (user-error "Unknown context profile: %S" profile))
  (gptel--sanitize-model)
  (let* ((source-buffer (current-buffer))
         (source-point (point))
         (backend gptel-backend)
         (model gptel-model)
         (question-buffer-name
          (my/-gptel-ask-buffer-name model source-buffer))
         (existing-buffer (get-buffer question-buffer-name)))
    (when (and existing-buffer
               (my/-gptel-ask-request-active-p existing-buffer))
      (user-error "A request is already active in %s" question-buffer-name))
    (let ((question
           (string-trim
            (read-string
             (format "Ask %s about %s: "
                     (gptel--model-name model)
                     (buffer-name source-buffer))
             nil 'my/-gptel-ask-question-history))))
      (when (string-empty-p question)
        (user-error "Question cannot be empty"))
      (let ((question-buffer
             (my/-gptel-ask-get-buffer
              question-buffer-name source-buffer backend model)))
        (with-current-buffer question-buffer
          (when (my/-gptel-ask-request-active-p question-buffer)
            (user-error "A request is already active in %s"
                        question-buffer-name))
          (when (or buffer-read-only
                    (get-char-property (point-max) 'read-only))
            (user-error "Question buffer %s is read-only"
                        question-buffer-name))
          (let* ((prompt-start (copy-marker (point-max)))
                 (generation (cl-incf my/-gptel-ask-collection-generation))
                 (request
                  (my/-gptel-ask-make-context-request
                   :source-buffer source-buffer
                   :source-point (with-current-buffer source-buffer
                                   (copy-marker source-point))
                   :profile profile
                   :project-root (my/-gptel-ask-project-root source-buffer)
                   :generation generation)))
            (my/-gptel-ask-append-question question)
            (setq my/-gptel-ask-request-active t)
            (message "Collecting %s context for %s..."
                     profile (buffer-name source-buffer))
            (my/-gptel-ask-send-after-context
             question-buffer request prompt-start)))
        (display-buffer question-buffer gptel-display-buffer-action)
        question-buffer))))

(my/gptel-ask-register-context-provider
 'python-imports
 :priority 110
 :predicate #'my/-gptel-ask-python-provider-p
 :collect #'my/-gptel-ask-python-collect)

(my/gptel-ask-register-context-provider
 'xref
 :priority 100
 :predicate #'my/-gptel-ask-xref-provider-p
 :collect #'my/-gptel-ask-xref-collect)

(my/gptel-ask-register-context-provider
 'c-family-lsp
 :priority 90
 :predicate #'my/-gptel-ask-c-family-provider-p
 :collect #'my/-gptel-ask-c-family-collect)

(my/gptel-ask-register-context-provider
 'ccls
 :priority 80
 :predicate #'my/-gptel-ask-ccls-provider-p
 :collect #'my/-gptel-ask-ccls-collect)

(my/gptel-ask-register-context-provider
 'lsp
 :priority 70
 :predicate #'my/-gptel-ask-lsp-provider-p
 :collect #'my/-gptel-ask-lsp-collect)

(provide 'my-gptel-utils)
;;; my-gptel-utils.el ends here
