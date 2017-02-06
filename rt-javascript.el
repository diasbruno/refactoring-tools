;;; rt-javascript.el --- refactoring tools javascript.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(defvar rt--javascript nil)

(defun point-is-js-function-at (point)
  "at point, is it a function?."
  (let ((point-end (+ point 8)))
    (and (< point-end (point-max))
         (string= "function" (string-on-range point point-end)))))

(defun rt--js-read-function-scope ()
  "Try to read a function."
  (setq scopes 1)
  (setq end-point nil)
  (save-excursion
    ;; first we need to skip 'till the end of the aguments scope
    (while (not (string= ")" (string (char-after (point)))))
      (right-char))
    (right-char)

    ;; not we go to the first bracket where the counter starts with one
    (while (not (string= "{" (string (char-after (point)))))
      (right-char))
    (right-char)

    ;; read while the scope is canceled.
    (while (not (eq scopes 0))
      (let ((ch (string (char-after (point)))))
        (setq scopes (cond ((string= ch "{")
                            (setq scopes (+ 1 scopes)))
                           ((string= ch "}")
                            (setq scopes (- scopes 1)))
                           (t scopes)))
        (right-char)))
    (setq end-point (point)))
  end-point)

(defun rt--js-at-point (current-buffer current-point)
  "Find the current context for refactoring at point."
  (with-current-buffer current-buffer
    (let ((found (rt--js-detect)))

        (cond ((string= (car found) "fn")
               (progn
                 `(,(rt--create-region current-point
                                       (rt--js-read-function-scope))
                   .
                   rt--js-fn-options)))
              (t nil)))))

(defun rt--js-detect ()
  "Detect what expression/keyword is on cursor point."
  (let* ((current-point (point)))
    (save-excursion
      (cond ((or (string= "(" (string (char-after (point))))
                 (point-is-js-function-at (point)))
             '("fn" current-point))
            (t nil)))))

(defun rt--js-apply-move ()
  "Just move the function elsewhere."
  (progn
    (rt--apply-move)
    (rt-quit)))

(defun rt--js-apply-move-anonymous ()
  "Will give a name to function and move elsewhere."
  (let ((fn-name (read-input "Funtion name: "))
        (start (overlay-start rt-active-region))
        (end (overlay-end rt-active-region)))
    ;; this will let the point where the function was.
    (rt--apply-move)
    ;; insert the new name in the function and move back the point
    ;; where it were.
    (save-excursion
      ;; use the overlay range to get how many char to move.
      (forward-char (+ 8 (- start end)))
      (insert (concat " " fn-name)))
    (forward-char (- start (point)))
    (insert fn-name)
    (rt-quit)))

(defun rt--js-rewrite-as-arrow-fn ()
  "Transform a function into an arrow function."
  (while (not (string= "(" (string (char-after (point)))))
    (delete-char 1))
  (while (not (string= ")" (string (char-after (point)))))
    (right-char))
  (right-char)
  (insert " =>")
  (rt-quit))

(defun rt--js-rewrite-as-regular-fn ()
  "Transform a function into an regular function."
  (let ((fn-name (read-input "Funtion name: ")))
  (insert (concat "function " fn-name))
  (while (not (string= ")" (string (char-after (point)))))
    (right-char))
  (right-char)
  (while (not (string= "{" (string (char-after (point)))))
    (delete-char 1))
  (insert " ")
  (rt-quit)))

(defun rt--js-fn-options ()
  "Returns the javascript options for refactoring."
  (let ((available-commands-list
         `(((key . "m")
            (title . "Move")
            (imm . nil)
            (command . rt--js-apply-move))
           ((key . "n")
            (title . "Move anonymous")
            (imm . nil)
            (command . rt--js-apply-move-anonymous))
           ((key . "a")
            (title . "Turn into arrow function")
            (imm . t)
            (command . rt--js-rewrite-as-arrow-fn))
           ((key . "f")
            (title . "Turn into regular function")
            (imm . t)
            (command . rt--js-rewrite-as-regular-fn)))))
    `((header . "Refactoring function\n")
      (commands . ,available-commands-list))))

(provide 'rt-javascript)
;;; rt.el ends here
