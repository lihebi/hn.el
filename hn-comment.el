;;; hn-comment.el --- A hacker news client. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Scholar
;; URL: https://github.com/lihebi/hn.el

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;; Code:

(defvar hn-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hn-comment-reload)
    (define-key map "m" #'hn-comment-load-more-stories)
    (define-key map "n" #'next-comment)
    (define-key map "p" #'previous-comment)
    (define-key map "l" #'hn-comment-list-new)
    (define-key map "L" #'hn-comment-list-all)
    map)
  "Keymap used in hn buffer.")

(define-derived-mode hn-comment-mode special-mode "HN-Comment"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun hn-pop-comment-buffer-async ()
  "Pop buffer async."
  (let ((buffer (get-buffer-create "*hn-comment*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (hn-comment-mode)
        (display-header)
        ;; lcoal-id is used to asynchronously pass the id
        (let* ((id local-id)
               (item (hn-retrieve-item id))
               (kids (cdr (assoc 'kids item))))
          ;; add the current article
          (display-item (hn-retrieve-item id))
          (insert "\n\n")
          ;; comments
          (mapc (lambda (id)
                  (hn-display-comment id 0))
                kids))))))

(defun hn-comment (id)
  "Show comment buffer for story ID."
  (let ((buffer (get-buffer-create "*hn-comment*")))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    ;; two local variables
    (setq local-id id)
    (setq local-done 0)
    (make-local-variable 'local-id)
    (make-local-variable 'local-done)
    ;; asynchronously pop the comments
    (make-thread #'hn-pop-comment-buffer-async)))

(defun hn-user-karma (user-id)
  (let ((user (hn-retrieve-user user-id)))
    (cdr (assoc 'karma user))))

(defun hn-display-comment (id depth)
  "Recursively display comments."
  (let* ((item (hn-retrieve-item id))
         (id (cdr (assoc 'id item)))
         (url (cdr (assoc 'url item)))
         (text (cdr (assoc 'text item)))
         (kids (cdr (assoc 'kids item)))
         (by (cdr (assoc 'by item)))
         (karma (hn-user-karma by))
         (user-url (format "https://news.ycombinator.com/user?id=%s" by))
         (indent (make-string (* depth 4) ? )))
    (let ((str (concat (make-text-button
                        (format "[%s (%s)]" by karma) nil
                        'type 'hn-user-button
                        'id by
                        ;; using item url, instead of user-url
                        ;; there is no score for a comment
                        'help-echo user-url
                        'url user-url)
                       " "
                       (title-fontifier (html-wrapper (or text ""))))))
      (save-excursion
        ;; insert at the end of the buffer
        (goto-char (point-max))
        (let ((pos (point)))
          (insert indent
                  (replace-regexp-in-string
                   "\n"
                   (concat "\n\n" indent)
                   ;; ""
                   str)
                  "\n\n")
          (fill-region pos (point)))
        ;; now go to line 4 and add the progress
        (setq local-done (1+ local-done))
        (goto-line 4)
        (when (not (looking-at "\n"))
          (kill-line))
        (insert (format "Loaded: %s" local-done)))
      ;; get kids
      (mapc (lambda (kid)
              (hn-display-comment kid (+ depth 1)))
            kids))))


(provide 'hn-comment)

;;; hn-comment.el ends here
