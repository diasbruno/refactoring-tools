;;; rt.el --- refactoring tools.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'rt-state)
(require 'rt-popup)

(defface rt-region-face
  '((t :inherit region))
  "The face used for fake regions."
  :group 'rt)

(defun string-on-range (start end)
  "Return a string on a range from START to END without properties."
  (buffer-substring-no-properties start end))

(defun rt-apply-move ()
  "Apply the move."
  (let* ((start (overlay-start rt--active-region))
         (end (overlay-end rt--active-region))
         (region-string (string-on-range start end)))
    (delete-region start end)
    (insert region-string)))

(defun rt--toggle-mode ()
  "Enable refactoring mode when a command is issued."
  (rt-mode (not rt--mode-enabled))
  (setq rt-mode-enabled (not rt--mode-enabled)))

(defun rt-point-in-buffer-limit (point)
  "Limit POINT to the end of the buffer."
  (if (< point (point-max))
      point
    (point-max)))

(defun rt-create-region (start end)
  "Create a regions-like on the point START and END."
  (let ((overlay (make-overlay start end nil nil t)))
    (overlay-put overlay 'face 'rt-region-face)
    (overlay-put overlay 'type 'additional-region)
    overlay))

(defun rt-run-at-point (language current-buffer current-point)
  "From a selected LANGUAGE in a CURRENT-BUFFER in the CURRENT-POINT."
  (let ((at-point-fn (intern (concat "rt-" language "-at-point"))))
    (if at-point-fn
        (funcall at-point-fn current-buffer current-point)
      nil)))

;;;###autoload
(defun rt-commit-change ()
  "Apply change to code."
  (interactive)
  (when rt--commit-function
    (funcall rt--commit-function)))

;;;###autoload
(defun rt-run ()
  "Start refactoring."
  (interactive)
  (if rt--commit-function
      (rt-commit-change)
    ;; run the function for language 'rt--*-at-point'
    (let ((can-refactor (rt-run-at-point rt--list-langs
                                         (current-buffer)
                                         (point))))
      (if can-refactor
          (progn
            (rt--save-current-buffer)
            (rt--save-active-region (car can-refactor))
            (rt--toggle-mode)
            (rt-open-popup (funcall (cdr can-refactor)) 'rt-quit))
        (message "There are no actions at this point")))))

;;;###autoload
(defun rt-quit ()
  "End refactoring or quit when impossible or no action to do."
  (interactive)
  (when rt--active-region
    (delete-overlay rt--active-region)
    (deactivate-mark))
  (rt--toggle-mode)
  (rt-close-popup)
  (setq rt--commit-function nil)
  (setq rt--active-region nil)
  (setq rt--current-buffer nil))

(define-minor-mode rt-mode
  "Mode while refactoring is active."
  nil " rt" rt-keymap)

(unless rt-keymap
  (setq rt-keymap (make-sparse-keymap))
  (define-key rt-keymap (kbd "C-g") 'rt-quit)
  rt-keymap)

(provide 'rt)
;;; rt.el ends here
