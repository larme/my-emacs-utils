;;; my-gptel-utils-tests.el --- Tests for my gptel utilities  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'my-gptel-utils)
(require 'gptel-openai)

(ert-deftest my/-gptel-ask-test-provider-registration-replaces-by-name ()
  (let ((my/-gptel-ask-context-providers nil))
    (my/gptel-ask-register-context-provider
     'low :priority 1 :predicate #'ignore :collect #'ignore)
    (my/gptel-ask-register-context-provider
     'high :priority 9 :predicate #'ignore :collect #'ignore)
    (my/gptel-ask-register-context-provider
     'low :priority 12 :predicate #'ignore :collect #'ignore)
    (should
     (equal (mapcar #'my/-gptel-ask-context-provider-name
                    my/-gptel-ask-context-providers)
            '(low high)))
    (my/gptel-ask-unregister-context-provider 'low)
    (should
     (equal (mapcar #'my/-gptel-ask-context-provider-name
                    my/-gptel-ask-context-providers)
            '(high)))))

(ert-deftest my/-gptel-ask-test-normalize-lines-merges-ranges ()
  (should
   (equal (my/-gptel-ask-normalize-lines
           '((20 . 25) (1 . 4) (5 . 8) (22 . 30) (40 . 39)))
          '((1 . 8) (20 . 30) (39 . 40)))))

(ert-deftest my/-gptel-ask-test-candidates-merge-and-exclude-source ()
  (let ((source (generate-new-buffer " *my-gptel-source*"))
        (related (generate-new-buffer " *my-gptel-related*")))
    (unwind-protect
        (progn
          (with-current-buffer source (insert "source\n"))
          (with-current-buffer related
            (dotimes (number 50) (insert (format "line %d\n" number))))
          (let* ((request
                  (my/-gptel-ask-make-context-request
                   :source-buffer source :source-point (with-current-buffer source
                                                         (copy-marker (point-min)))
                   :profile 'smart :project-root default-directory
                   :generation 1))
                 (context
                  (my/-gptel-ask-candidates-to-context
                   (list
                    (my/gptel-ask-context-candidate
                     source :relation 'definition :lines '(1 . 2) :score 100)
                    (my/gptel-ask-context-candidate
                     related :relation 'definition :lines '(1 . 10) :score 90)
                    (my/gptel-ask-context-candidate
                     related :relation 'reference :lines '(8 . 15) :score 40))
                   request)))
            (should (equal context `((,related :lines ((1 . 15))))))))
      (kill-buffer source)
      (kill-buffer related))))

(ert-deftest my/-gptel-ask-test-install-context-preserves-manual-entry ()
  (let ((source (generate-new-buffer " *my-gptel-source*"))
        (auto (generate-new-buffer " *my-gptel-auto*"))
        (manual (generate-new-buffer " *my-gptel-manual*"))
        (chat (generate-new-buffer " *my-gptel-chat*")))
    (unwind-protect
        (with-current-buffer chat
          (setq-local gptel-context (list source auto manual))
          (setq-local my/-gptel-ask-base-context (list source))
          (setq-local my/-gptel-ask-auto-context (list auto))
          (setq-local my/-gptel-ask-manual-context nil)
          (my/-gptel-ask-install-context nil)
          (should (equal gptel-context (list source manual)))
          (should (equal my/-gptel-ask-manual-context (list manual))))
      (kill-buffer source)
      (kill-buffer auto)
      (kill-buffer manual)
      (kill-buffer chat))))

(ert-deftest my/-gptel-ask-test-python-import-positions ()
  (with-temp-buffer
    (insert "import os, package.module as alias\n"
            "from .local import value\n"
            "  from nested.module import other\n")
    (let ((names
           (mapcar
            (lambda (position)
              (goto-char position)
              (and (looking-at "[[:word:]_.]+")
                   (match-string-no-properties 0)))
            (my/-gptel-ask-python-import-positions))))
      (should (equal names '("os" "module" "local" "module"))))))

(ert-deftest my/-gptel-ask-test-lsp-location-conversion ()
  (let* ((start (make-hash-table :test #'equal))
         (range (make-hash-table :test #'equal))
         (location (make-hash-table :test #'equal)))
    (puthash "line" 9 start)
    (puthash "start" start range)
    (puthash "range" range location)
    (puthash "uri" "file:///tmp/example%20file.py" location)
    (let ((candidate
           (my/-gptel-ask-lsp-object-candidate
            location 'definition 90 'test)))
      (should (equal (my/-gptel-ask-context-candidate-source candidate)
                     "/tmp/example file.py"))
      (should
       (equal (my/-gptel-ask-context-candidate-lines candidate)
              (list (cons 1 (+ 10 my/gptel-ask-snippet-radius))))))))

(ert-deftest my/-gptel-ask-test-generic-lsp-collector ()
  (let ((source (generate-new-buffer " *my-gptel-lsp-source*"))
        collected)
    (unwind-protect
        (with-current-buffer source
          (setq-local buffer-file-name "/tmp/source.py")
          (insert "symbol\n")
          (let ((request
                 (my/-gptel-ask-make-context-request
                  :source-buffer source :source-point (copy-marker (point-min))
                  :profile 'smart :project-root "/tmp/" :generation 1)))
            (cl-letf (((symbol-function 'lsp-feature?)
                       (lambda (&rest _) t))
                      ((symbol-function 'lsp--text-document-position-params)
                       (lambda () '(:textDocument (:uri "file:///tmp/source.py")
                                    :position (:line 0 :character 0))))
                      ((symbol-function 'lsp-request-async)
                       (lambda (method _params callback &rest _arguments)
                         (let ((location (make-hash-table :test #'equal)))
                           (puthash
                            "uri"
                            (if (string-suffix-p "implementation" method)
                                "file:///tmp/implementation.py"
                              "file:///tmp/type.py")
                            location)
                           (funcall callback (vector location))))))
              (my/-gptel-ask-lsp-collect
               request (lambda (candidates) (setq collected candidates))))
            (should (= (length collected) 2))
            (should
             (equal
              (mapcar #'my/-gptel-ask-context-candidate-relation collected)
              '(implementation type-definition)))
            (should (eq (my/gptel-ask-context-request-source-buffer request)
                        source))
            (should (= (my/gptel-ask-context-request-point request) 1))
            (should (eq (my/gptel-ask-context-request-profile request) 'smart))))
      (kill-buffer source))))

(ert-deftest my/-gptel-ask-test-document-links-become-whole-files ()
  (let ((first (make-hash-table :test #'equal))
        (second (make-hash-table :test #'equal)))
    (puthash "target" "file:///tmp/first.h" first)
    (puthash "target" "https://example.com/not-local" second)
    (let ((candidates
           (my/-gptel-ask-lsp-document-link-candidates
            (vector first second))))
      (should (= (length candidates) 1))
      (should (equal (my/-gptel-ask-context-candidate-source
                      (car candidates))
                     "/tmp/first.h"))
      (should-not (my/-gptel-ask-context-candidate-lines
                   (car candidates))))))

(ert-deftest my/-gptel-ask-test-source-only-collection-sends-immediately ()
  (let ((source (generate-new-buffer " *my-gptel-source*"))
        (chat (generate-new-buffer " *my-gptel-chat*"))
        (sent 0))
    (unwind-protect
        (progn
          (with-current-buffer source (insert "source\n"))
          (with-current-buffer chat
            (insert "question")
            (setq-local gptel-context nil)
            (setq-local my/-gptel-ask-base-context (list source))
            (setq-local my/-gptel-ask-auto-context nil)
            (setq-local my/-gptel-ask-manual-context nil)
            (setq-local my/-gptel-ask-collection-generation 1)
            (setq-local my/-gptel-ask-request-active t))
          (let ((request
                 (my/-gptel-ask-make-context-request
                  :source-buffer source
                  :source-point (with-current-buffer source
                                  (copy-marker (point-min)))
                  :profile 'source-only
                  :project-root default-directory
                  :generation 1)))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _) (cl-incf sent))))
              (my/-gptel-ask-send-after-context
               chat request (with-current-buffer chat
                              (copy-marker (point-min))))))
          (should (= sent 1))
          (should (equal (buffer-local-value 'gptel-context chat)
                         (list source))))
      (kill-buffer source)
      (kill-buffer chat))))

(ert-deftest my/-gptel-ask-test-selected-region-stays-with-question ()
  (let* ((source (generate-new-buffer " *my-gptel-region-source*"))
         (manual (generate-new-buffer " *my-gptel-region-manual*"))
         (gptel-model 'test-model)
         (gptel-backend (gptel--make-openai :name "test" :models '(test-model)))
         (gptel-context '(global-context))
         (gptel-context-string-function #'gptel-context--string)
         (chat (generate-new-buffer
                (my/-gptel-ask-buffer-name gptel-model source)))
         (transient-mark-mode t)
         (use-empty-active-region nil)
         (turn 0)
         serialized expected-messages)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "before\n" (propertize "chosen α\nand β" 'face 'bold)
                    "\nafter\n"))
          (with-current-buffer manual (insert "manual context"))
          (with-current-buffer chat
            (text-mode)
            (setq-local gptel-mode t)
            (setq-local gptel-system-prompt nil)
            (setq-local gptel-context (list manual))
            ;; A chat created before reloading may still have the old hook.
            (setq-local gptel-prompt-transform-functions
                        '(my/-gptel-ask-prepare-context t)))
          (cl-letf (((symbol-function 'gptel--sanitize-model) #'ignore)
                    ((symbol-function 'read-string)
                     (lambda (&rest _)
                       (with-current-buffer source (deactivate-mark))
                       (format "Question %d" turn)))
                    ((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq serialized
                             (map-nested-elt
                              (gptel-fsm-info
                               (gptel-request nil :dry-run t
                                 :transforms gptel-prompt-transform-functions))
                              '(:data :messages)))
                       (insert gptel-response-separator
                               (gptel-response-prefix-string)
                               (propertize (format "Answer %d" turn)
                                           'gptel 'response
                                           'front-sticky '(gptel)))
                       (my/-gptel-ask-request-finished))))
            (dolist (selection '("chosen α\nand β" "before" nil ""))
              (cl-incf turn)
              (with-current-buffer source
                (goto-char (point-min))
                (when selection
                  (search-forward selection)
                  ;; Exercise a reversed selection too.
                  (push-mark (point) t t)
                  (backward-char (length selection)))
                (should (eq (my/gptel-ask-buffer 'source-only) chat)))
              (setq expected-messages
                    (append
                     expected-messages
                     (list
                      (list :role "user" :content
                            (concat
                             (when (and selection
                                        (not (string-empty-p selection)))
                               (concat "selected region:\n\n```\n"
                                       selection "\n```\n\n"))
                             (format "Question %d" turn))))))
              (let ((context
                     (gptel-context--string
                      (gptel-context--collect (list source manual)))))
                (should
                 (equal serialized
                        (vconcat (list (list :role "system" :content context))
                                 expected-messages))))
              (setq expected-messages
                    (append expected-messages
                            (list (list :role "assistant" :content
                                        (format "Answer %d" turn)))))
              (should (equal (buffer-local-value 'gptel-context chat)
                             (list source manual)))
              (should (equal gptel-context '(global-context)))
              (should (eq gptel-context-string-function
                          #'gptel-context--string)))))
      (kill-buffer source)
      (kill-buffer manual)
      (kill-buffer chat))))

(ert-deftest my/-gptel-ask-test-selected-region-rolled-back-on-send-error ()
  (let ((source (generate-new-buffer " *my-gptel-region-source*")))
    (unwind-protect
        (with-temp-buffer
          (insert "Existing conversation")
          (setq-local gptel-context nil)
          (setq-local my/-gptel-ask-base-context (list source))
          (setq-local my/-gptel-ask-collection-generation 1)
          (setq-local my/-gptel-ask-request-active t)
          (let ((prompt-start (copy-marker (point-max)))
                (request
                 (my/-gptel-ask-make-context-request
                  :source-buffer source
                  :source-point (with-current-buffer source (point-marker))
                  :profile 'source-only :project-root default-directory
                  :generation 1))
                attempted)
            (my/-gptel-ask-append-question "New question" "selected text")
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq attempted
                               (buffer-substring-no-properties
                                prompt-start (point-max)))
                         (error "Mock send failure"))))
              (my/-gptel-ask-send-after-context
               (current-buffer) request prompt-start))
            (should (string-suffix-p
                     "selected region:\n\n```\nselected text\n```\n\nNew question"
                     attempted))
            (should (equal (buffer-string) "Existing conversation"))
            (should-not my/-gptel-ask-request-active)
            (should-not (marker-buffer prompt-start))))
      (kill-buffer source))))

(ert-deftest my/-gptel-ask-test-timeout-keeps-completed-provider-results ()
  (let ((source (generate-new-buffer " *my-gptel-source*"))
        (related (generate-new-buffer " *my-gptel-related*"))
        (my/gptel-ask-collection-timeout 0.01)
        (cancelled nil)
        (collected nil))
    (unwind-protect
        (progn
          (with-current-buffer source (insert "source\n"))
          (with-current-buffer related (insert "related\n"))
          (let* ((my/-gptel-ask-context-providers nil)
                 (request
                  (my/-gptel-ask-make-context-request
                   :source-buffer source
                   :source-point (with-current-buffer source
                                   (copy-marker (point-min)))
                   :profile 'smart
                   :project-root default-directory
                   :generation 1)))
            (my/gptel-ask-register-context-provider
             'completed
             :priority 10
             :predicate (lambda (_request) t)
             :collect
             (lambda (_request callback)
               (funcall callback
                        (list (my/gptel-ask-context-candidate related)))
               nil))
            (my/gptel-ask-register-context-provider
             'stalled
             :priority 1
             :predicate (lambda (_request) t)
             :collect
             (lambda (_request _callback)
               (lambda () (setq cancelled t))))
            (my/-gptel-ask-collect-context
             request (lambda (context) (setq collected context)))
            (let ((deadline (+ (float-time) 1)))
              (while (and (null collected) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should cancelled)
            (should (equal collected (list related)))))
      (kill-buffer source)
      (kill-buffer related))))

(ert-deftest my/-gptel-ask-test-cache-reuses-only-current-source-revision ()
  (let ((source (generate-new-buffer " *my-gptel-cache-source*"))
        (related (generate-new-buffer " *my-gptel-cache-related*"))
        (my/gptel-ask-cache-ttl 60)
        (calls 0))
    (unwind-protect
        (progn
          (with-current-buffer source (insert "source\n"))
          (with-current-buffer related (insert "related\n"))
          (let* ((my/-gptel-ask-context-providers nil)
                 (my/-gptel-ask-provider-generation 0)
                 (request
                  (my/-gptel-ask-make-context-request
                   :source-buffer source
                   :source-point (with-current-buffer source
                                   (copy-marker (point-min)))
                   :profile 'smart
                   :project-root default-directory
                   :generation 1)))
            (my/gptel-ask-register-context-provider
             'cached
             :priority 1
             :predicate (lambda (_request) t)
             :collect
             (lambda (_request callback)
               (cl-incf calls)
               (funcall callback
                        (list (my/gptel-ask-context-candidate related)))
               nil))
            (dotimes (_ 2)
              (let (context)
                (my/-gptel-ask-collect-context
                 request (lambda (value) (setq context value)))
                (should (equal context (list related)))))
            (should (= calls 1))
            (with-current-buffer source
              (goto-char (point-max))
              (insert "changed\n"))
            (my/-gptel-ask-collect-context request #'ignore)
            (should (= calls 2))))
      (kill-buffer source)
      (kill-buffer related))))

(provide 'my-gptel-utils-tests)
;;; my-gptel-utils-tests.el ends here
