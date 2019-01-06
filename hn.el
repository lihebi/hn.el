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

(require 'hn-utils)
(require 'hn-comment)

;; list-colors-display
;; list-faces-display

(defface hn-link
  '((t :inherit link :underline nil)) "" :group 'hn)

(defface hn-link-visited
  '((t :inherit link-visited :underline nil)) "" :group 'hn)

(defface hn-title
  '((t :inherit hn-link)) "" :group 'hn)

(defface hn-title-visited
  '((t :inherit hn-link-visited)) "" :group 'hn)

(defface hn-comment-count
  '((t :foreground "dark red")) "" :group 'hn)

(defface hn-comment-count-visited
  '((t :foreground "dark magenta")) "" :group 'hn)

(defface hn-user
  '((t :foreground "blue")) "" :group 'hn)

(defcustom hn-hl-users '()
  "A list of users to follow. Articles or comments by these users
  are highlighted."
  :group 'hn
  :type '(list string))

(defcustom hn-hl-keywords '()
  "A list of keywords you care about. Articles and comments
  containing these keywords are highlighted."
  :group 'hn
  :type '(list string))

(defcustom hn-history-file
  (locate-user-emacs-file "hn-history.el")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'hn
  :type '(choice file (const :tag "None" nil)))



(defvar *hn-top-stories* ())
(defvar *hn-item-table* (make-hash-table)
  "A hash map from id to item.")
(defvar *hn-user-table* (make-hash-table)
  "A hash map from id to item.")
(defvar *hn-visited* ()
  "A set of visited ids. FIXME Use hash?")
(defvar *hn-num-stories* 20 "Number of stories")
(defvar *hn-list-all* t)

(defun hn-reset ()
  (interactive)
  (setq *hn-top-stories* ())
  (setq *hn-item-table* (make-hash-table))
  (setq *hn-user-table* (make-hash-table))
  (setq *hn-visited* ())
  (setq *hn-num-stories* 20)
  (setq *hn-list-all* t)
  (hn))



(defun hn-load-more-stories ()
  "Increase number; retrieve and display."
  (interactive)
  (setq *hn-num-stories* (+ *hn-num-stories* 20))
  (hn-reload))


(defun hn-list-all ()
  (interactive)
  (setq *hn-list-all* t)
  (hn-reload))

(defun hn-list-new ()
  (interactive)
  (setq *hn-list-all* nil)
  (hn-reload))

(defvar hn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hn-reload)
    (define-key map "m" #'hn-load-more-stories)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "l" #'hn-list-new)
    (define-key map "L" #'hn-list-all)
    (define-key map "r" #'hn-reset)
    (define-key map "c" #'hn-browse-current-comment)
    (define-key map (kbd "RET") #'hn-browse-current-comment)
    map)
  "Keymap used in hn buffer.")

(define-derived-mode hn-mode special-mode "HN"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

;; FIXME duplicate
(define-button-type 'hn-title-button
  'action                  #'browse-url-action
  'face                    'hn-link
  'follow-link             t)
(define-button-type 'hn-title-visited
  'action                  #'browse-url-action
  'face                    'hn-title-visited
  'follow-link             t)

(define-button-type 'hn-comment-button
  'action                  #'browse-url-action
  'face                    'hn-comment-count
  'follow-link             t)
(define-button-type 'hn-comment-visited
  'action                  #'browse-url-action
  'face                    'hn-comment-count-visited
  'follow-link             t)

(define-button-type 'hn-user-button
  'action                  #'browse-url-action
  'face                    'hn-user
  'follow-link             t)

(defun hn--load-history ()
  (when (not (file-exists-p hn-history-file))
    (hn--save-history))
  (setq *hn-visited* (with-temp-buffer
                       (insert-file-contents hn-history-file)
                       (read (current-buffer)))))

(defun hn--save-history ()
  (with-temp-file hn-history-file
      (prin1 *hn-visited* (current-buffer))))

(defun hn-mark-as-read (id)
  (add-to-list '*hn-visited* id)
  (hn--save-history)
  (hn-reload))

(defun browse-url-action (button)
  "Browse url when click BUTTON."
  (let* ((id    (button-get button 'id))
         (type  (button-type button))
         (url (button-get button 'url))
         (inhibit-read-only t))
    (hn-mark-as-read)
    (browse-url url)))

(defun browse-comment-action (button)
  (interactive)
  (let ((id (button-get button 'id)))
    (hn-mark-as-read)
    (hn-comment id)))

(defun hn-browse-current-comment ()
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (hn-mark-as-read)
    (hn-comment id)))

(defun get-current-article-button ()
  (next-button (line-beginning-position)))

(defun hn-ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hn-mode)
    (signal 'hn-error '("Not a HN buffer"))))

(defun display-header ()
  (insert (format "%-5s %-7s %-20s %s\n"
                  "Score"
                  (propertize "Comment" 'face 'hn-comment-count)
                  (propertize "User" 'face 'hn-user)
                  (propertize "Title" 'face 'hn-title))
          (make-string 80 ?-)
          "\n"))

(defun display-item (item)
  (let ((id (cdr (assoc 'id item)))
        (title (cdr (assoc 'title item)))
        (score (cdr (assoc 'score item)))
        (url (cdr (assoc 'url item)))
        (by (cdr (assoc 'by item)))
        (descendants  (cdr (assq 'descendants item))))
    (when (or *hn-list-all*
              (not (member id *hn-visited*)))
      (insert
       (format "%-5s %-7s %-20s %s\n"
               (propertize (format "%s" score))
               ;; these buttons can be clicked
               (make-text-button
                (format "%s" descendants) nil
                'type (if (member id *hn-visited*)
                          'hn-comment-visited
                        'hn-comment-button)
                'id id
                'help-echo (hn--comment-web-url id)
                'url (hn--comment-web-url id))
               (make-text-button
                (format "%s (%s)" by (hn-user-karma by)) nil
                'type 'hn-user-button
                'id by
                ;; using item url, instead of user-url
                ;; there is no score for a comment
                'help-echo url
                'url url)
               (make-text-button title nil
                                 'type (if (member id *hn-visited*)
                                           'hn-title-visited
                                         'hn-title-button)
                                 'id id
                                 'help-echo url
                                 'url url))))))

(defun hn-retrieve-top-stories ()
  "Get a list of top stories."
  (when (not *hn-top-stories*)
    (setq *hn-top-stories*
          (let ((top-story-url (concat hn-api-prefix "/topstories.json")))
            (json-read-url top-story-url))))
  *hn-top-stories*)

(defun hn-reload ()
  (interactive)
  (hn-ensure-major-mode)
  (let ((pos (point))
        (winpos (window-start)))
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (display-header)
        (let ((items (mapcar #'hn-retrieve-item
                             (seq-take (hn-retrieve-top-stories)
                                       *hn-num-stories*))))
          (mapc #'display-item items))))
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

;;;###autoload
(defun hn ()
  "Load hn interface."
  (interactive)
  (let ((buffer (get-buffer-create "*hn*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hn-mode)
      (hn--load-history)
      (hn-reload))
    ;; view comment
    (pop-to-buffer buffer)))

(provide 'hn)

;;; hn.el ends here
