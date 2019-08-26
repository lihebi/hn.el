;;; hn.el --- A hacker news client. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Scholar
;; URL: https://github.com/lihebi/hn.el

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;; Code:

(require 'browse-url)
(require 'url)

(require 'hn-vars)
(require 'hn-utils)
(require 'hn-ui)
(require 'hn-comment)
(require 'hn-source)

(defvar hn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hn-reload-command)
    (define-key map "G" #'hn-hard-reload)
    ;; q by default is bound to quit-window
    ;; (define-key map "q" #'quit-window)
    (define-key map "m" #'hn-load-more-stories)
    (define-key map "M" #'hn-load-all-stories)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "l" #'hn-list-cycle)
    (define-key map "t" #'hn-add-tag)
    (define-key map "c" #'hn-browse-current-comment)
    (define-key map "u" #'hn-toggle-mark-as-read)
    (define-key map "d" #'hn-mark-as-read-and-continue)
    (define-key map "s" #'hn-toggle-star)
    (define-key map (kbd "RET") #'hn-browse-current-article)
    map)
  "Keymap used in hn buffer.")

(define-derived-mode hn-mode special-mode "HN"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

;; these mode exists just to indicate different modes
(define-derived-mode hn-all-mode hn-mode "HN-ALL"
  :group 'hn)
(define-derived-mode hn-starred-mode hn-mode "HN-STARRED"
  :group 'hn)
(define-derived-mode hn-new-mode hn-mode "HN-NEW"
  :group 'hn)

(defun hn-list-cycle ()
  (interactive)
  (setq *hn-list-type*
        (case *hn-list-type*
          (all (hn-all-mode)
               'new)
          (new (hn-new-mode)
               'starred)
          (starred (hn-starred-mode)
                   'all)
          (t (error "hn-list-type error"))))
  (hn-reload))

(defun hn-generate-json-for-web ()
  "Generate a json file containing starred articles for webpage
listing."
  (interactive)
  (hn--load)
  (with-temp-file hn-export-json-file
    (insert (json-encode (mapcar (lambda (id)
                                   (append (hn-retrieve-item id)
                                           (list (cons 'tags (or (gethash id *hn-tag-table*) '())))))
                                 *hn-starred*)))
    (json-pretty-print-buffer)))

(defun hn-ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hn-mode)
    (signal 'hn-error '("Not a HN buffer"))))

;;;###autoload
(defun hn ()
  "Load hn interface."
  (interactive)
  (let ((buffer (get-buffer-create "*hn*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hn-all-mode)
      ;; FIXME better loading and saving scheme?
      (hn--load)
      (setq *hn-source* 'current)
      (hn-reload))
    ;; view comment
    (pop-to-buffer buffer)))

(provide 'hn)

;;; hn.el ends here
