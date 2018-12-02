;;; rt-javascript.el --- refactoring tools javascript.  -*- lexical-binding: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/rt

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'rt)

(defun rt-js--is-token-function (token)
  "Check if the TOKEN is a 'function'."
  (string= "function" token))

(defun rt-js--is-token-export (token)
  "Check if the TOKEN is 'export'."
  (string= "export" token))

nn(defun rt-js--is-token-var (token)
  "Check if the TOKEN is 'var'."
  (string= "var" token))

(defun rt-js--is-token-class (token)
  "Check if TOKEN is 'class'."
  (string= "class" token))

(defun rt-js--point-is-js-function-at ()
  "Is the current POINT in a function context?"
  (let ((w (thing-at-point 'word)))
    (progn
      (cond
       ((rt-js--is-token-function w) t)
       ((rt-js--is-token-export w)
        (progn
          (right-word)
          (rt-js--point-is-js-function-at)))
       (t nil)))))

(defun rt-js-read-function-scope ()
  "Try to read a function."
  (let ((scopes nil)
        (end-point nil))
    (setq scopes 1)
    (setq end-point nil)
    (save-excursion
      ;; first we need to skip 'till the end of the aguments scope
      (while (not (char-equal ?\) (char-after (point))))
        (right-char))
      (right-char)
      ;; not we go to the first bracket where the counter starts with one
      (while (not (char-equal ?\{ (char-after (point))))
        (right-char))
      (right-char)
      ;; read while the scope is canceled.
      (while (not (eq scopes 0))
        (let ((ch (char-after (point))))
          (setq scopes
                (cond ((char-equal ?\{ ch) (setq scopes (1+ scopes)))
                      ((char-equal  ?\} ch) (setq scopes (- scopes 1)))
                      (t scopes)))
          (right-char)))
      (setq end-point (point)))
    end-point))

(defun rt-js-read-scope (for)
  "Read scope FOR."
  (cond
   ((string= for "fn") (rt-js-read-function-scope))
   (t nil)))

(defun rt-js-detect ()
  "Detect what expression/keyword is on cursor point."
  (save-excursion
    (let ((w (thing-at-point 'word t)))
      (cond
       ((rt-js--is-token-class w) "class")
       ((rt-js--is-token-var w) "var")
       ((rt-js--point-is-js-function-at) "fn")
       (t nil)))))

(defun rt-js-fn-options ()
  "Return the javascript options for refactoring."
  (let ((available-commands-list
         `(((key . "m")
            (title . "Move")
            (imm . nil)
            (command . rt-js-apply-move))
           ((key . "n")
            (title . "Move anonymous")
            (imm . nil)
            (command . rt-js-apply-move-anonymous))
           ((key . "a")
            (title . "Turn into arrow function")
            (imm . t)
            (command . rt-js-rewrite-as-arrow-fn))
           ((key . "f")
            (title . "Turn into regular function")
            (imm . t)
            (command . rt-js-rewrite-as-regular-fn))
           ((key . "e")
            (title . "Toggle export")
            (imm . t)
            (command . rt-js-toggle-export))
           ((key . "r")
            (title . "Rename function")
            (imm . t)
            (command . rt-js-rename-function)))))
    `((header . "Refactoring function\n")
      (commands . ,available-commands-list))))

(defun rt-js-create-class ()
  "Create a new class."
  (let ((name (read-string "Class name: "))
        (current-point (point)))
    (insert (concatenate 'string "class " name " {" "\n"))
    (insert "}")
    (rt-quit)))

(defun rt-js-creational-options ()
  "Return the javascript options for refactoring."
  (let ((available-commands-list
         `(((key . "c")
            (title . "New class")
            (imm . t)
            (command . rt-js-create-class)))))
    `((header . "Create\n")
      (commands . ,available-commands-list))))

(defun rt-js-at-point (current-buffer point)
  "Find the context on the CURRENT-BUFFER for refactoring at POINT.
Returns a pair of with the current region or nil, and the options."
  (with-current-buffer current-buffer
    (let ((found (rt-js-detect)))
      (if (or (string= found "fn") (string= found "var"))
          `(,(rt-create-region
              point
              (rt-js-read-scope found)) . rt-js-fn-options)
        `(nil . rt-js-creational-options)))))

(defun rt-js-apply-move ()
  "Just move the function elsewhere."
  (progn
    (rt-apply-move)
    (rt-quit)))

(defun rt-js-apply-move-anonymous ()
  "Will give a name to function and move elsewhere."
  (let ((fn-name (read-string "Funtion name: "))
        (start (overlay-start rt--active-region))
        (end (overlay-end rt--active-region)))
    ;; this will let the point where the function was.
    (rt-apply-move)
    ;; insert the new name in the function and move back the point
    ;; where it were.
    (save-excursion
      ;; use the overlay range to get how many char to move.
      (forward-char (+ 8 (- start end)))
      (insert (concat " " fn-name)))
    (forward-char (- start (point)))
    (insert fn-name)
    (rt-quit)))

(defun rt-js-rewrite-as-arrow-fn ()
  "Transform a function into an arrow function."
  (while (not (string= "(" (string (char-after (point)))))
    (delete-char 1))
  (while (not (string= ")" (string (char-after (point)))))
    (right-char))
  (right-char)
  (insert " =>")
  (rt-quit))

(defun rt-js-toggle-export ()
  "Toggle export of a function declaration."
  (let ((w (thing-at-point 'word))
        (expd nil))
    (cond
     ((string= "export" w) (progn
                             (kill-word 1)
                             (delete-char 1)
                             (rt-quit)))
     ((string= "function" w) (progn
                               (save-excursion
                                 (left-word)
                                 (setq expd (string= "export" (thing-at-point 'word))))
                               (if expd
                                   (kill-word -1)
                                 (insert "export "))
                               (rt-quit))))))

(defun rt-js-rewrite-as-regular-fn ()
  "Transform a function into an regular function."
  (let ((fn-name (read-string "Funtion name: ")))
  (insert (concat "function " fn-name))
  (while (not (string= ")" (string (char-after (point)))))
    (right-char))
  (right-char)
  (while (not (string= "{" (string (char-after (point)))))
    (delete-char 1))
  (insert " ")
  (rt-quit)))

(defun rt-js-rename-function ()
  "Rename a function.")

(provide 'rt-javascript)
;;; rt-javascript.el ends here
