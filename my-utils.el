;;; my-utils.el --- Personal utility commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous personal utility commands.

;;; Code:

(require 'subr-x)

(defun my/copy-to-tmux ()
  "Copy the active region, or the entire buffer, to the tmux paste buffer."
  (interactive)
  (unless (executable-find "tmux")
    (user-error "Cannot find the tmux executable"))
  (let ((region-active (use-region-p))
        (output-buffer (generate-new-buffer " *tmux load-buffer*")))
    (unwind-protect
        (save-restriction
          (unless region-active
            (widen))
          (let* ((start (if region-active (region-beginning) (point-min)))
                 (end (if region-active (region-end) (point-max)))
                 (coding-system-for-write 'utf-8-unix)
                 (status (call-process-region
                          start end "tmux" nil (list output-buffer t) nil
                          "load-buffer" "-")))
            (unless (and (integerp status) (zerop status))
              (let ((details
                     (with-current-buffer output-buffer
                       (string-trim (buffer-string)))))
                (user-error "Tmux load-buffer failed%s"
                            (if (string-empty-p details)
                                ""
                              (format ": %s" details)))))
            (message "Copied %d character%s to the tmux paste buffer"
                     (- end start)
                     (if (= (- end start) 1) "" "s"))))
      (kill-buffer output-buffer))))

(provide 'my-utils)

;;; my-utils.el ends here
