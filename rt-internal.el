;;; rt.el --- refactoring tools.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(defvar rt-active-region nil
  "Available stack of regions.")

(defvar rt-commit-function nil
  "Function to execute when commiting.")

(defun point-in-buffer-limit (point)
  "limit point to the end of the buffer."
  (if (< point (point-max))
      point
    (point-max)))

(defun rt--create-region (start end)
  "Create a regions-like on the point START and END."
  (let ((overlay (make-overlay start end nil nil t)))
    (overlay-put overlay 'face 'rt-region-face)
    (overlay-put overlay 'type 'additional-region)
    overlay))

(defun rt--run-at-point (lang current-buffer current-point)
  "Try to find an available option to refactoring and return it."
  (let ((at-point-fn (intern (concat "rt--" lang "-at-point"))))
    (if at-point-fn
        (funcall at-point-fn current-buffer current-point)
      nil)))

(provide 'rt-internal)
;;; rt.el ends here
