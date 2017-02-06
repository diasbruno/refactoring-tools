;;; rt.el --- refactoring tools.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(cl-eval-when (load eval)
  (require 'rt-internal)
  (require 'rt-popup))

(defvar rt-mode-enabled nil
  "Check if refactoring mode is enabled.")

(defvar rt-keymap nil
  "Keymap while refactoring is active.")

(defvar rt--current-buffer nil)

(defvar rt--list-langs "js")

;;; Faces:

(defface rt-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'rt)

(defun string-on-range (start end)
  "Returns a string on a range without properties."
  (buffer-substring-no-properties start end))

(defun rt--apply-move ()
  "Apply the move."
  (let* ((start (overlay-start rt-active-region))
         (end (overlay-end rt-active-region))
         (region-string (string-on-range start end)))
    (delete-region start end)
    (insert region-string)))

(defun rt--toggle-mode ()
  "Enable refactoring mode when a command is issued.
This is the minor mode for warning the user what is going on."
  (if (not rt-mode-enabled)
      (progn
        (rt-mode)
        (setq rt-mode-enabled t))
    (progn
      (rt-mode 0)
      (setq rt-mode-enabled nil))))

;;;###autoload
(defun rt-commit-change ()
  "Apply changes to code."
  (interactive)
  (when rt-commit-function
    (funcall rt-commit-function)))

;;;###autoload
(defun rt-run ()
  (interactive)
  (if rt-commit-function
      (rt-commit-change)
    ;; run the function for language 'rt--*-at-point'
    (let ((can-refactor (rt--run-at-point rt--list-langs
                                          (current-buffer)
                                          (point))))
      (if can-refactor
          (progn
            (setq rt--current-buffer (current-buffer))
            (setq rt-active-region (car can-refactor))
            (rt--toggle-mode)
            (rt-open-popup (funcall (cdr can-refactor)) 'rt-quit))
        (message "There are no actions at this point")))))

;;;###autoload
(defun rt-quit ()
  "End refactoring or quit when impossible or no action to do."
  (interactive)
  (when rt-active-region
    (delete-overlay rt-active-region)
    (deactivate-mark))
  (rt--toggle-mode)
  (rt-close-popup)
  (setq rt-commit-function nil)
  (setq rt-active-region nil)
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
