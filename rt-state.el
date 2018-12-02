;;; rt-state.el --- refactoring tools.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar rt--mode-enabled nil
  "Check if refactoring mode is enabled.")

(defvar rt-keymap nil
  "Keymap while refactoring is active.")

(defvar rt--current-buffer nil
  "Current buffer.")

(defvar rt--list-langs "js"
  "List of supported languages.")

(defvar rt--active-region nil
  "Available stack of regions.")

(defvar rt--commit-function nil
  "Function to execute when commiting.")

(defun rt--save-current-buffer ()
  "Save the current buffer."
  (setq rt--current-buffer (current-buffer)))

(defun rt--save-active-region (refactoring-item)
  "Save the current active region for REFACTORING-ITEM."
  (when refactoring-item
   (setq rt--active-region refactoring-item)))

(defun rt--dump-state ()
  "."
  (print "-- rt: dump state --")
  (print rt--mode-enabled)
  (print rt-keymap)
  (print rt--current-buffer)
  (print rt--list-langs))

(provide 'rt-state)
;;; rt-state.el ends here
