;;; my-gptel-complete-tests.el --- Completion tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with this directory, gptel and its dependencies on `load-path':
;; emacs --batch -Q ... -L . -l my-gptel-complete-tests.el \
;;   -f ert-run-tests-batch-and-exit
;; Requests are mocked; no model API calls or live LSP servers are needed.

;;; Code:

(require 'ert)
(require 'my-gptel-complete)
(require 'gptel-openai)

(defvar lsp-mode)
(defvar treesit-defun-tactic)

(defmacro my/-gptel-complete-test-buffer (&rest body)
  "Run BODY in a source buffer with a captured, unsent gptel request."
  (declare (indent 0) (debug t))
  `(let ((old-buffers (buffer-list))
         (my/gptel-complete-use-tools nil)
         (my/gptel-complete-use-project-instructions nil)
         (sent-count 0) sent-prompt sent-args sent-buffer sent-settings)
     (unwind-protect
         (with-temp-buffer
           (emacs-lisp-mode)
           (buffer-enable-undo)
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (prompt &rest args)
                        (cl-incf sent-count)
                        (setq sent-prompt prompt sent-args args
                              sent-buffer (current-buffer)
                              sent-settings
                              (list gptel-context gptel-use-context gptel-tools
                                    gptel-model gptel-backend gptel-temperature)))))
             (prog1 (progn ,@body)
               (ignore sent-count sent-prompt sent-args sent-buffer sent-settings))))
       (dolist (buffer (cl-set-difference (buffer-list) old-buffers))
         (when (string-match-p "gptel.*completion" (buffer-name buffer))
           (kill-buffer buffer))))))

(defun my/-gptel-complete-test-select (beg end)
  "Activate a region from BEG to END."
  (goto-char end)
  (set-mark beg)
  (setq transient-mark-mode t mark-active t))

(ert-deftest my/gptel-complete-mode-prompt-and-default ()
  (let ((my/gptel-complete-system-prompts
         '((default . "fallback") (emacs-lisp-mode . "lisp"))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal (my/-gptel-complete-system-prompt) "lisp"))
      (fundamental-mode)
      (should (equal (my/-gptel-complete-system-prompt) "fallback")))))

(ert-deftest my/gptel-complete-defun-selection ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun one ()\n  nil)\n\n(defun two ()\n  nil)\n")
    (goto-char (point-min))
    (search-forward "nil")
    (let ((bounds (my/gptel-complete-default-bounds)))
      (should (= (car bounds) (point-min)))
      (should (string-match-p "defun one" (buffer-substring (car bounds) (cdr bounds))))
      (should-not (string-match-p "defun two" (buffer-substring (car bounds) (cdr bounds)))))))

(ert-deftest my/gptel-complete-treesit-prefers-nested-defun ()
  (with-temp-buffer
    (insert "abcdefghijklmnop")
    (goto-char 8)
    (cl-letf (((symbol-function 'treesit-parser-list) (lambda (&rest _) '(parser)))
              ((symbol-function 'treesit-defun-at-point)
               (lambda () (should (eq treesit-defun-tactic 'nested)) 'node))
              ((symbol-function 'treesit-node-start) (lambda (_) 4))
              ((symbol-function 'treesit-node-end) (lambda (_) 12)))
      (should (equal (my/gptel-complete-default-bounds) '(4 . 12)))
      (narrow-to-region 6 15)
      (should (>= (car (my/gptel-complete-default-bounds)) 6)))))

(ert-deftest my/gptel-complete-incomplete-code-and-blank-line ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun unfinished ()\n  (message\n")
    (goto-char (point-max))
    (backward-char 2)
    (let ((bounds (my/gptel-complete-default-bounds)))
      (should (<= (car bounds) (point) (cdr bounds))))
    (erase-buffer)
    (insert "\n")
    (goto-char 1)
    (should (equal (my/gptel-complete-default-bounds) '(1 . 1)))))

(ert-deftest my/gptel-complete-selection-full-source-narrowing-and-isolation ()
  (my/-gptel-complete-test-buffer
    (insert "before\nTARGET\nafter\n")
    (setq-local gptel-context '("manual-context")
                gptel-model 'test-model gptel-backend 'test-backend
                gptel-temperature 0.37)
    (narrow-to-region 8 14)
    (my/-gptel-complete-test-select 8 14)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Unexpected prompt"))))
      (let ((job (my/gptel-complete)))
        (should (= sent-count 1))
        (should (equal (my/-gptel-completion-original job) "TARGET"))
        (should (string-match-p "before\nTARGET\nafter" sent-prompt))
        (should (equal gptel-context '("manual-context")))
        (should (= (point-min) 8))
        (should (= (point-max) 14))
        (should-not (eq sent-buffer (current-buffer)))
        (should (equal sent-settings '(nil nil nil test-model test-backend 0.37)))
        (should (equal (plist-get sent-args :buffer) sent-buffer))
        (should-not (plist-get sent-args :stream))
        (funcall (plist-get sent-args :callback) "DONE" nil)
        (save-restriction
          (widen)
          (should (equal (buffer-string) "before\nDONE\nafter\n")))
        (should-not my/-gptel-completion)))))

(ert-deftest my/gptel-complete-replacement-one-undo-step ()
  (my/-gptel-complete-test-buffer
    (insert "before\nold\nafter\n")
    (setq buffer-undo-list nil)
    (my/-gptel-complete-test-select 8 12)
    (my/gptel-complete)
    (funcall (plist-get sent-args :callback) "new" nil)
    (should (equal (buffer-string) "before\nnew\nafter\n"))
    (undo 1)
    (should (equal (buffer-string) "before\nold\nafter\n"))))

(ert-deftest my/gptel-complete-empty-buffer-insertion ()
  (my/-gptel-complete-test-buffer
    (my/gptel-complete)
    (funcall (plist-get sent-args :callback) "(message \"hello\")" nil)
    (should (equal (buffer-string) "(message \"hello\")"))))

(ert-deftest my/gptel-complete-markers-follow-edits-outside-target ()
  (my/-gptel-complete-test-buffer
    (insert "before\nold\nafter\n")
    (my/-gptel-complete-test-select 8 11)
    (my/gptel-complete)
    (goto-char 1)
    (insert "prefix\n")
    (funcall (plist-get sent-args :callback) "new" nil)
    (should (equal (buffer-string) "prefix\nbefore\nnew\nafter\n"))))

(ert-deftest my/gptel-complete-target-edits-are-not-overwritten ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (my/-gptel-complete-test-select 1 4)
    (my/gptel-complete)
    (goto-char 2)
    (insert "X")
    (let (saved)
      (cl-letf (((symbol-function 'my/-gptel-complete-show-result)
                 (lambda (text _reason) (setq saved text))))
        (funcall (plist-get sent-args :callback) "new" nil))
      (should (equal saved "new")))
    (should (equal (buffer-string) "oXld"))
    (should-not my/-gptel-completion)))

(ert-deftest my/gptel-complete-rejects-fences-and-empty-output ()
  (dolist (response '("" "```elisp\nnew\n```"))
    (my/-gptel-complete-test-buffer
      (insert "old")
      (my/-gptel-complete-test-select 1 4)
      (my/gptel-complete)
      (cl-letf (((symbol-function 'my/-gptel-complete-show-result) #'ignore))
        (funcall (plist-get sent-args :callback) response nil))
      (should (equal (buffer-string) "old"))
      (should-not my/-gptel-completion))))

(ert-deftest my/gptel-complete-preserves-response-on-read-only-text ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (my/-gptel-complete-test-select 1 4)
    (my/gptel-complete)
    (put-text-property 1 4 'read-only t)
    (let (saved)
      (cl-letf (((symbol-function 'my/-gptel-complete-show-result)
                 (lambda (text _reason) (setq saved text))))
        (funcall (plist-get sent-args :callback) "new" nil))
      (should (equal saved "new")))
    (should (equal (buffer-substring-no-properties 1 4) "old"))
    (should-not my/-gptel-completion)))

(ert-deftest my/gptel-complete-ignores-tool-preambles-and-reasoning ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (my/-gptel-complete-test-select 1 4)
    (my/gptel-complete)
    (let ((callback (plist-get sent-args :callback)))
      (funcall callback "Let me read a file" '(:tool-use ((:name "read"))))
      (funcall callback '(reasoning . "thinking") nil)
      (funcall callback '(tool-result . "context") nil)
      (should (equal (buffer-string) "old"))
      (should my/-gptel-completion)
      (funcall callback "new" nil)
      (should (equal (buffer-string) "new")))))

(ert-deftest my/gptel-complete-errors-and-cancel-release-state ()
  (dolist (response '(nil abort))
    (my/-gptel-complete-test-buffer
      (insert "old")
      (my/-gptel-complete-test-select 1 4)
      (let ((job (my/gptel-complete)))
        (funcall (plist-get sent-args :callback) response '(:status "failure"))
        (should-not my/-gptel-completion)
        (should-not (marker-buffer (my/-gptel-completion-begin job)))
        (should (equal (buffer-string) "old"))))))

(ert-deftest my/gptel-complete-cancel-isolates-request-and-ignores-late-results ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (my/-gptel-complete-test-select 1 4)
    (my/gptel-complete)
    (should-error (my/gptel-complete) :type 'user-error)
    (let (aborted)
      (cl-letf (((symbol-function 'gptel-abort) (lambda (buffer) (setq aborted buffer))))
        (my/gptel-complete-cancel))
      (should (eq aborted sent-buffer))
      (should-not (eq aborted (current-buffer))))
    (funcall (plist-get sent-args :callback) "late" nil)
    (should (equal (buffer-string) "old"))))

(ert-deftest my/gptel-complete-custom-system-prompt ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (my/-gptel-complete-test-select 1 4)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Custom instructions")))
      (call-interactively #'my/gptel-complete-with-system-prompt))
    (should (string-prefix-p "Custom instructions\n" (plist-get sent-args :system)))
    (should (equal (my/-gptel-completion-original my/-gptel-completion) "old"))))

(ert-deftest my/gptel-complete-identifier-scan-ignores-comments-and-strings ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(alpha beta 42 \"quoted\") ; comment\n(gamma)")
    (goto-char (point-min))
    (let ((my/gptel-complete-max-identifiers 2))
      (should (equal (mapcar (lambda (pos)
                              (save-excursion (goto-char pos) (thing-at-point 'symbol t)))
                            (my/-gptel-complete-identifiers (point-min) (point-max)))
                     '("alpha" "beta"))))))

(ert-deftest my/gptel-complete-location-and-location-link-results ()
  (should (equal (my/-gptel-complete-location-files
                  [(:uri "file:///tmp/a%20b.el")
                   (:targetUri "file:///tmp/types.el")
                   (:uri "https://example.com/source")
                   (:uri "file://remotehost/tmp/no")])
                 '("/tmp/a b.el" "/tmp/types.el")))
  (let ((location (make-hash-table :test #'equal)))
    (puthash "uri" "file:///tmp/one.el" location)
    (should (equal (my/-gptel-complete-location-files location) '("/tmp/one.el"))))
  (should (equal (my/-gptel-complete-location-files '((uri . "file:///tmp/one.el")))
                 '("/tmp/one.el"))))

(ert-deftest my/gptel-complete-lsp-capabilities-async-and-timeout ()
  (my/-gptel-complete-test-buffer
    (let ((lsp-mode t) calls cancelled)
      (setq buffer-file-name "/tmp/completion-source.el")
      (insert "(alpha beta)")
      (my/-gptel-complete-test-select 1 (point-max))
      (cl-letf (((symbol-function 'lsp-feature?)
                 (lambda (method) (equal method "textDocument/typeDefinition")))
                ((symbol-function 'lsp-text-document-identifier)
                 (lambda () '(:uri "file:///tmp/completion-source.el")))
                ((symbol-function 'lsp-point-to-position)
                 (lambda (pos) (list :line 0 :character (1- pos))))
                ((symbol-function 'lsp-request-async)
                 (lambda (method params callback &rest args)
                   (push (list method params callback args) calls)))
                ((symbol-function 'lsp-cancel-request-by-token)
                 (lambda (token) (push token cancelled))))
        (let ((job (my/gptel-complete)))
          (should (zerop sent-count))
          (should (= (length calls) 2))
          (should-not (plist-get (nth 3 (car calls)) :no-merge))
          (funcall (nth 2 (car calls)) '(:uri "file:///tmp/types.el"))
          ;; Simulate firing the collection timer: keep the completed result.
          (my/-gptel-complete-send job)
          (should (= sent-count 1))
          (should (= (length cancelled) 1))
          (should (equal (my/-gptel-completion-paths job) '("/tmp/types.el")))
          (funcall (nth 2 (cadr calls)) '(:uri "file:///tmp/late.el"))
          (should (= sent-count 1))
          (should-not (member "/tmp/late.el" (my/-gptel-completion-paths job))))))))

(ert-deftest my/gptel-complete-source-change-during-collection-cancels-send ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (let (job)
      (cl-letf (((symbol-function 'my/-gptel-complete-collect)
                 (lambda (request _positions) (setq job request))))
        (my/gptel-complete))
      (insert "X")
      (my/-gptel-complete-send job)
      (should (zerop sent-count))
      (should-not my/-gptel-completion))))

(ert-deftest my/gptel-complete-context-deduplicates-and-uses-unsaved-files ()
  (let* ((root (make-temp-file "gptel-completion-test-" t))
         (file (expand-file-name "types.el" root))
         (big (expand-file-name "large.el" root))
         (related (generate-new-buffer " *completion-test-related*")))
    (unwind-protect
        (progn
          (write-region "disk" nil file nil 'silent)
          (write-region (make-string 100 ?x) nil big nil 'silent)
          (with-current-buffer related
            (setq buffer-file-name file)
            (insert "unsaved definition")
            (narrow-to-region 2 4))
          (with-temp-buffer
            (let* ((my/gptel-complete-max-file-chars 30)
                   (job (my/-gptel-completion-create
                         :root root :paths (list file file big "/etc/hosts")))
                   (text (my/-gptel-complete-context job)))
              (should (string-match-p "unsaved definition" text))
              (should-not (string-match-p "disk\|large.el\|/etc/hosts" text))
              (should (= (length (split-string text "Related definition file:")) 2)))))
      (with-current-buffer related (set-buffer-modified-p nil))
      (kill-buffer related)
      (delete-directory root t))))

(ert-deftest my/gptel-complete-tools-enforce-root-size-and-call-budget ()
  (let* ((root (make-temp-file "gptel-completion-tools-" t))
         (file (expand-file-name "small.el" root))
         (outside (make-temp-file "gptel-completion-outside-" nil nil "outside"))
         (my/gptel-complete-max-tool-calls 4))
    (unwind-protect
        (progn
          (write-region "small" nil file nil 'silent)
          (make-symbolic-link outside (expand-file-name "escape.el" root))
          (let* ((tools (my/-gptel-complete-tools root))
                 (read (gptel-tool-function (cadr tools)))
                 (listing (gptel-tool-function (car tools))))
            (should (equal (funcall read "small.el") "small"))
            (should (string-match-p "outside" (funcall read outside)))
            (should (string-match-p "outside" (funcall read "escape.el")))
            (should (equal (funcall listing "small") "small.el"))
            (should (string-match-p "budget exhausted" (funcall read "small.el")))))
      (delete-directory root t)
      (delete-file outside))))

(ert-deftest my/gptel-complete-send-error-cleans-up ()
  (my/-gptel-complete-test-buffer
    (insert "old")
    (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (error "Send failure"))))
      (let ((job (my/gptel-complete)))
        (should (my/-gptel-completion-done job))
        (should-not my/-gptel-completion)
        (should (equal (buffer-string) "old"))))))

(ert-deftest my/gptel-complete-builds-real-gptel-request-with-tools ()
  ;; Exercise the actual request builder without issuing an HTTP request.
  (let ((request (symbol-function 'gptel-request)))
    (my/-gptel-complete-test-buffer
      (let ((my/gptel-complete-use-tools t) built)
        (insert "(message \"hello\")")
        (setq-local gptel-api-key "test-key-unused")
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (prompt &rest args)
                     (setq built (apply request prompt :dry-run t args)))))
          (let ((job (my/gptel-complete)))
            (should built)
            (should-not (my/-gptel-completion-done job))
            (with-current-buffer (my/-gptel-completion-request-buffer job)
              (should (= (length gptel-tools) 2))
              (should gptel-use-tools)
              (should-not gptel-context))))))))

(defmacro my/-gptel-complete-test-project-instructions (&rest body)
  "Run BODY with a temporary project ROOT and nested source DIRECTORY."
  (declare (indent 0) (debug t))
  `(let* ((fixture (make-temp-file "gptel-completion-instructions-" t))
          (root (expand-file-name "project/" fixture))
          (directory (expand-file-name "src/nested/" root))
          (my/gptel-complete-instruction-files '("AGENTS.md" "CLAUDE.md"))
          (my/gptel-complete-max-instruction-chars 20000))
     (unwind-protect
         (progn
           (make-directory directory t)
           (my/-gptel-complete-test-buffer
             (let ((my/gptel-complete-use-project-instructions t))
               (setq default-directory directory
                     buffer-file-name (expand-file-name "source.el" directory))
               ,@body)))
       (delete-directory fixture t))))

(defun my/-gptel-complete-test-write-file (file text)
  "Create FILE and its parent directories with contents TEXT."
  (make-directory (file-name-directory file) t)
  (write-region text nil file nil 'silent))

(ert-deftest my/gptel-complete-instructions-follow-ancestor-precedence ()
  (my/-gptel-complete-test-project-instructions
    (dolist (entry '(("../AGENTS.md" . "OUTSIDE_ROOT")
                     ("CLAUDE.md" . "ROOT_CLAUDE")
                     ("AGENTS.md" . "ROOT_AGENTS")
                     ("src/CLAUDE.md" . "MIDDLE_CLAUDE")
                     ("src/nested/CLAUDE.md" . "LEAF_CLAUDE")
                     ("src/nested/AGENTS.md" . "LEAF_AGENTS")
                     ("src/sibling/AGENTS.md" . "SIBLING")
                     ("src/nested/child/AGENTS.md" . "CHILD")))
      (my/-gptel-complete-test-write-file (expand-file-name (car entry) root) (cdr entry)))
    ;; A buffer-local working directory must not change the source's scope.
    (setq default-directory root)
    (let ((text (my/-gptel-complete-project-instructions root))
          (previous -1))
      (dolist (rule '("ROOT_CLAUDE" "ROOT_AGENTS" "MIDDLE_CLAUDE" "LEAF_CLAUDE" "LEAF_AGENTS"))
        (let ((index (string-match (regexp-quote rule) text)))
          (should index)
          (should (> index previous))
          (setq previous index)))
      (should-not (string-match-p "OUTSIDE_ROOT\|SIBLING\|CHILD" text)))))

(ert-deftest my/gptel-complete-instruction-budget-keeps-nearest-and-priority ()
  (my/-gptel-complete-test-project-instructions
    (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" root) "ROOT_A")
    (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" directory) "LEAF_A")
    (my/-gptel-complete-test-write-file (expand-file-name "CLAUDE.md" directory) "LEAF_C")
    (let ((my/gptel-complete-max-instruction-chars 12)
          ;; Instructions have their own budget, independent of definition files.
          (my/gptel-complete-max-file-chars 0)
          (my/gptel-complete-max-context-chars 0))
      (let ((text (my/-gptel-complete-project-instructions root)))
        (should (string-match-p "LEAF_A" text))
        (should (string-match-p "LEAF_C" text))
        (should-not (string-match-p "ROOT_A" text)))
      (let ((my/gptel-complete-max-instruction-chars 6))
        (let ((text (my/-gptel-complete-project-instructions root)))
          (should (string-match-p "LEAF_A" text))
          (should-not (string-match-p "LEAF_C\|ROOT_A" text)))
        (let* ((my/gptel-complete-instruction-files '("CLAUDE.md" "AGENTS.md"))
               (text (my/-gptel-complete-project-instructions root)))
          (should (string-match-p "LEAF_C" text))
          (should-not (string-match-p "LEAF_A\|ROOT_A" text))))
      ;; Oversized files are omitted whole; smaller files can still fit.
      (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" directory)
                                          (make-string 13 ?X))
      (let ((text (my/-gptel-complete-project-instructions root)))
        (should (string-match-p "LEAF_C" text))
        (should (string-match-p "ROOT_A" text))
        (should-not (string-match-p "XXX" text))))))

(ert-deftest my/gptel-complete-instructions-deduplicate-paths-and-content ()
  (my/-gptel-complete-test-project-instructions
    (let ((file (expand-file-name "AGENTS.md" root)))
      (my/-gptel-complete-test-write-file file "SHARED_GUIDANCE")
      (make-symbolic-link file (expand-file-name "CLAUDE.md" root))
      (make-symbolic-link file (expand-file-name "CLAUDE.md" directory))
      (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" directory)
                                          "SHARED_GUIDANCE")
      (let ((text (my/-gptel-complete-project-instructions root)))
        (should (= (length (split-string text "SHARED_GUIDANCE")) 2))
        (should (string-match-p "src/nested/AGENTS.md" text))
        (should-not (string-match-p "CLAUDE.md" text))))))

(ert-deftest my/gptel-complete-instructions-prefer-widened-unsaved-buffers ()
  (my/-gptel-complete-test-project-instructions
    (let ((existing (generate-new-buffer " *instruction-existing*"))
          (new (generate-new-buffer " *instruction-new*"))
          (existing-path (expand-file-name "CLAUDE.md" root))
          (new-path (expand-file-name "AGENTS.md" directory)))
      (unwind-protect
          (progn
            (my/-gptel-complete-test-write-file existing-path "OLD_DISK_TEXT")
            (with-current-buffer existing
              (setq buffer-file-name existing-path)
              (insert "UPDATED_UNSAVED_TEXT")
              (narrow-to-region 2 5))
            (with-current-buffer new
              (setq buffer-file-name new-path)
              (insert "NEW_UNSAVED_TEXT"))
            (should-not (file-exists-p new-path))
            (let ((text (my/-gptel-complete-project-instructions root)))
              (should (string-match-p "UPDATED_UNSAVED_TEXT" text))
              (should (string-match-p "NEW_UNSAVED_TEXT" text))
              (should-not (string-match-p "OLD_DISK_TEXT" text)))
            (with-current-buffer existing
              (should (= (point-min) 2))
              (should (= (point-max) 5))))
        (dolist (buffer (list existing new))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest my/gptel-complete-instructions-exclude-escapes-binary-and-imports ()
  (my/-gptel-complete-test-project-instructions
    (let ((outside (expand-file-name "AGENTS.md" fixture))
          (my/gptel-complete-include-external-files t)
          (my/gptel-complete-instruction-files '("../AGENTS.md" "AGENTS.md" "CLAUDE.md")))
      (my/-gptel-complete-test-write-file outside "OUTSIDE_SECRET")
      (make-symbolic-link outside (expand-file-name "AGENTS.md" root))
      (my/-gptel-complete-test-write-file (expand-file-name "CLAUDE.md" root)
                                          "KEEP_GUIDANCE\n@extra.md")
      (my/-gptel-complete-test-write-file (expand-file-name "extra.md" root) "IMPORT_CONTENT")
      (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" directory) "\0BINARY")
      ;; A directory bearing an instruction filename is not a readable file.
      (make-directory (expand-file-name "CLAUDE.md" directory))
      (let ((text (my/-gptel-complete-project-instructions root)))
        (should (string-match-p "KEEP_GUIDANCE" text))
        (should-not (string-match-p "OUTSIDE_SECRET\|IMPORT_CONTENT\|BINARY" text))))))

(ert-deftest my/gptel-complete-instructions-disable-and-remote-skip-discovery ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'file-truename)
               (lambda (&rest _) (ert-fail "Discovery should not read the filesystem"))))
      (let ((my/gptel-complete-use-project-instructions nil))
        (should-not (my/-gptel-complete-project-instructions "/tmp/")))
      (let ((my/gptel-complete-use-project-instructions t)
            (my/gptel-complete-max-instruction-chars 0))
        (should-not (my/-gptel-complete-project-instructions "/tmp/")))
      (let ((my/gptel-complete-use-project-instructions t)
            (buffer-file-name "/ssh:example:/project/source.el"))
        (should-not (my/-gptel-complete-project-instructions "/ssh:example:/project/"))))))

(ert-deftest my/gptel-complete-instructions-without-project-stay-in-source-directory ()
  (my/-gptel-complete-test-project-instructions
    (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" root) "PARENT_GUIDANCE")
    (my/-gptel-complete-test-write-file (expand-file-name "AGENTS.md" directory) "LOCAL_GUIDANCE")
    (setq default-directory root)
    (insert "SOURCE_CODE")
    (cl-letf (((symbol-function 'project-current)
               (lambda (_prompt dir)
                 (should (equal dir directory))
                 nil)))
      (my/gptel-complete "Explicit custom system prompt"))
    (should (= sent-count 1))
    (should (string-match-p "LOCAL_GUIDANCE" sent-prompt))
    (should-not (string-match-p "PARENT_GUIDANCE" sent-prompt))
    (should (< (string-match "SOURCE_CODE" sent-prompt)
               (string-match "Project instructions" sent-prompt)))
    (should (string-prefix-p "Explicit custom system prompt\n" (plist-get sent-args :system)))
    (should (string-match-p "Repository workflow instructions do not expand the task"
                            (plist-get sent-args :system)))))

(ert-deftest my/gptel-complete-instructions-are-snapshotted-before-lsp ()
  (my/-gptel-complete-test-project-instructions
    (let ((file (expand-file-name "AGENTS.md" root)) job)
      (my/-gptel-complete-test-write-file file "ORIGINAL_GUIDANCE")
      (insert "SOURCE_CODE")
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (cons 'transient root)))
                ((symbol-function 'my/-gptel-complete-collect)
                 (lambda (request _positions) (setq job request))))
        (my/gptel-complete))
      (my/-gptel-complete-test-write-file file "LATER_GUIDANCE")
      (my/-gptel-complete-send job)
      (should (= sent-count 1))
      (should (string-match-p "ORIGINAL_GUIDANCE" sent-prompt))
      (should-not (string-match-p "LATER_GUIDANCE" sent-prompt)))))

(provide 'my-gptel-complete-tests)
;;; my-gptel-complete-tests.el ends here
