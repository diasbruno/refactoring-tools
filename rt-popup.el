;;; rt-popup.el --- refactoring tools.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar rt-buffer-name "*refactoring-popup*"
  "Default buffer name.")

(defvar rt-popup-buffer nil
  "Popup buffer.")

(defvar rt-popup-keymap nil
  "Popup keymap.")

(defun rt--set-cmd (fn immediate)
  "Set FN to be executed IMMEDIATE or not."
  (lambda ()
    (interactive)
    (if immediate
        (progn
          (set-buffer rt--current-buffer)
          (funcall fn))
      (progn
        (setq rt-commit-function fn)
        (rt-close-popup)))))

(defun rt--create-bind (key)
  "Create a bind for a KEY."
  (let ((k (assoc 'key key))
        (f (assoc 'command key))
        (imm (assoc 'imm key)))
    (local-set-key (cdr k)
                   (rt--set-cmd (cdr f) (cdr imm)))))

(defun rt--create-binds (cmds quit)
  "Create all local key bindings for CMDS, including QUIT."
  (mapc 'rt--create-bind (cdr cmds))
  (local-set-key "q" quit))

(defun rt--menu-item (item)
  "Create a menu ITEM."
  (concat " "
          (propertize (cdr (assoc 'key item))
                      'face
                      'font-lock-function-name-face)
          " "
          (cdr (assoc 'title item))))

(defun rt--create-popup-menu (cmds)
  "Create the popup menu for all CMDS."
  (let ((h (assoc 'header cmds))
        (cms (assoc 'commands cmds)))
    (concat (propertize (cdr h) 'face 'font-lock-keyword-face)
            (mapconcat (lambda (x)
                         (concat (rt--menu-item x) "\n"))
                       (cdr cms) ""))))

(defun rt--display-popup (buffer)
  "Display the popup BUFFER."
  (set-buffer buffer)
  (setq rt-popup-buffer buffer)
  (pop-to-buffer buffer))

(defun rt-open-popup (options quit)
  "Display the refactoring OPTIONS and QUIT."
  (unless rt-popup-buffer
    (split-window-vertically (floor (* 0.68 (window-height))))
    (let ((bf (get-buffer-create rt-buffer-name)))
      (rt--display-popup bf)
      (insert (rt--create-popup-menu options))
      (rt-popup-mode)
      (rt--create-binds (assoc 'commands options) quit)
      (setq rt-popup-keymap (make-sparse-keymap))
      (define-key rt-popup-keymap (kbd "C-g") quit))))

(defun rt-close-popup ()
  "Close the current popup opened."
  (when rt-popup-buffer
    (delete-window (get-buffer-window rt-popup-buffer))
    (kill-buffer rt-popup-buffer)
    (setq rt-popup-buffer nil)
    (setq rt-popup-keymap nil)))

(define-derived-mode rt-popup-mode fundamental-mode "RefactoringPopup"
  "Major mode for infix argument popups."
  :mode 'rt-popup
  :keymap rt-popup-keymap
  (setq truncate-lines t)
  (setq buffer-read-only t))

(provide 'rt-popup)
;;; rt-popup.el ends here
