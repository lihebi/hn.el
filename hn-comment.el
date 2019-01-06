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

(defun hn-comment (id)
  (let* ((item (hn-retrieve-item id))
         (kids (cdr (assoc 'kids item))))
    ;; process comments
    (let ((buffer (get-buffer-create "*hn-comment*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (hn-comment-mode)
          (display-header)
          ;; add the current article
          (display-item (hn-retrieve-item id))
          (insert "\n\n")
          ;; comments
          (mapc (lambda (id)
                  (hn-display-comment id 0))
                kids)))
      ;; view comment
      (pop-to-buffer buffer)
      (goto-char (point-min)))))

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
         (user-url (format "https://news.ycombinator.com/user?id=%s" by))
         (pos (point))
         (indent (make-string (* depth 4) ? )))
    (let ((str (concat (make-text-button
                        (format "[%s (%s)]" by (hn-user-karma by)) nil
                        'type 'hn-user-button
                        'id by
                        ;; using item url, instead of user-url
                        ;; there is no score for a comment
                        'help-echo user-url
                        'url user-url)
                       " "
                       (html-wrapper text))))
      (insert indent
              (replace-regexp-in-string
               "\n"
               (concat "\n\n" indent)
               ;; ""
               str)
              "\n\n")
      (fill-region pos (point))
      ;; get kids
      (mapc (lambda (kid)
              (hn-display-comment kid (+ depth 1)))
            kids))))


(provide 'hn-comment)

;;; hn-comment.el ends here
