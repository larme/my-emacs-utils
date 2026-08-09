;;; my-gptel-rewrite-with-buffer-context.el --- Enhanced gptel rewrite with buffer context  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Custom Function
;; Keywords: convenience, tools
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an enhanced version of `gptel-rewrite' that
;; automatically includes the current buffer as context for the rewrite
;; request. The function manages context carefully, only removing the
;; buffer from context if it wasn't already present when the function
;; was called.

;;; Code:

(require 'gptel)
(require 'gptel-rewrite)
(require 'gptel-context)

;;;###autoload
(defun my/gptel-rewrite-with-buffer-context (&optional rewrite-message)
  "Rewrite region with current buffer as context.

This function performs a gptel rewrite operation while temporarily
adding the current buffer as context. If the current buffer is not
already in the context when this function is called, it will be
removed from context after the rewrite operation completes.

If a region is active, rewrites that region. Otherwise, requires
an existing rewrite overlay at point to iterate on.

REWRITE-MESSAGE is the instruction for the rewrite operation. If
not provided, it defaults to \"Rewrite: \"."
  (interactive)
  (unless (or (use-region-p) (get-char-property (point) 'gptel-rewrite))
    (user-error "Region must be active or cursor on existing rewrite"))
  
  (let* ((current-buf (current-buffer))
         ;; Check if current buffer is already in context
         (buffer-already-in-context 
          (cl-some (lambda (entry)
                     (eq (car entry) current-buf))
                   gptel-context--alist))
         ;; Store original context state for cleanup
         (original-context-entry 
          (when buffer-already-in-context
            (alist-get current-buf gptel-context--alist)))
         ;; Store the original rewrite callback to wrap it
         (original-callback (symbol-function 'gptel--rewrite-callback)))
    
    ;; Add current buffer to context if not already present
    (unless buffer-already-in-context
      (gptel-context--add-region current-buf (point-min) (point-max) t))
    
    ;; Define a wrapper callback that handles context cleanup
    (cl-letf (((symbol-function 'gptel--rewrite-callback)
               (lambda (response info)
                 ;; Call the original callback first
                 (funcall original-callback response info)
                 
                 ;; Clean up context only if we added it and the operation is complete
                 (when (and (not buffer-already-in-context)
                           (or (eq response t)  ; successful completion
                               (eq response 'abort)  ; aborted
                               (null response)))  ; error
                   ;; Remove the buffer from context that we added
                   (when-let* ((context-overlays 
                               (alist-get current-buf gptel-context--alist)))
                     (dolist (ov context-overlays)
                       (when (and (overlay-start ov)
                                  (overlay-end ov)
                                  (= (overlay-start ov) (point-min))
                                  (= (overlay-end ov) (point-max)))
                         (gptel-context-remove ov)
                         (return))))))))
      
      ;; Perform the actual rewrite with enhanced context
      (if (get-char-property (point) 'gptel-rewrite)
          ;; Iterate on existing rewrite
          (gptel--rewrite-iterate)
        ;; Start new rewrite
        (let ((gptel--rewrite-message (or rewrite-message "Rewrite: ")))
          (call-interactively #'gptel-rewrite))))))

(provide 'my-gptel-rewrite-with-buffer-context)
;;; my-gptel-rewrite-with-buffer-context.el ends here