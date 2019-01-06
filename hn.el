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

(defcustom hn-history-dir
  (locate-user-emacs-file "hn-history")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'hn
  :type '(choice file (const :tag "None" nil)))


(defvar *hn-top-stories* ())
(defvar *hn-item-table* (make-hash-table :test 'equal)
  "A hash map from id to item.")
(defvar *hn-user-table* (make-hash-table :test 'equal)
  "A hash map from id to item.")
(defvar *hn-visited* ()
  "A set of visited ids. FIXME Use hash?")
(defvar *hn-num-stories* 20 "Number of stories")
(defvar *hn-list-all* t)


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

(defun hn--save-visited ()
  (hn--save-to-file *hn-visited* "hn-visited.el"))
(defun hn--load-visited ()
  (setq *hn-visited* (hn--load-from-file "hn-visited.el")))

(defvar *hn-user-table-saved-size* 0)
(defun hn--save-user-table ()
  (hn--save-to-file (hash-to-list *hn-user-table*) "hn-user-table.el")
  (setq *hn-user-table-saved-size* (hash-table-size *hn-user-table*)))
(defun hn--load-user-table ()
  (setq *hn-user-table* (list-to-hash (hn--load-from-file "hn-user-table.el")))
  (setq *hn-user-table-saved-size* (hash-table-size *hn-user-table*)))

(defun ensure-history-dir ()
  (when (not (file-exists-p hn-history-dir))
    (make-directory hn-history-dir))
  (when (not (file-exists-p (concat hn-history-dir "/hn-visited.el")))
    (hn--save-visited))
  (when (not (file-exists-p (concat hn-history-dir "/hn-user-table.el")))
    (hn--save-user-table)))

(defun hn--save-to-file (obj filename)
  (with-temp-file (concat hn-history-dir "/" filename)
    (prin1 obj (current-buffer))))

(defun hn--load-from-file (filename)
  (with-temp-buffer
    (insert-file-contents (concat hn-history-dir "/" filename))
    (read (current-buffer))))

(defun hn--save ()
  "Save all information: history, user table, etc."
  (ensure-history-dir)
  (hn--save-visited)
  (hn--save-user-table))

(defun hn--load ()
  "Load all info."
  (ensure-history-dir)
  (hn--load-visited)
  (hn--load-user-table))


(defvar hn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hn-reload)
    (define-key map "G" #'hn-hard-reload)
    (define-key map "m" #'hn-load-more-stories)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "l" #'hn-list-new)
    (define-key map "L" #'hn-list-all)
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
  'action                  #'browse-url-and-mark-action
  'face                    'hn-link
  'follow-link             t)
(define-button-type 'hn-title-visited
  'action                  #'browse-url-action
  'face                    'hn-title-visited
  'follow-link             t)

(define-button-type 'hn-comment-button
  'action                  #'browse-url-and-mark-action
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


(defun hn-mark-as-read (id)
  (add-to-list '*hn-visited* id)
  (hn--save-visited)
  (hn-reload))

(defun browse-url-action (button)
  "Browse url when click BUTTON."
  (browse-url (button-get button 'url)))

(defun browse-url-and-mark-action (button)
  "Browse url when click BUTTON."
  (hn-mark-as-read (button-get button 'id))
  (browse-url (button-get button 'url)))

(defun browse-comment-action (button)
  (interactive)
  (let ((id (button-get button 'id)))
    (hn-mark-as-read id)
    (hn-comment id)))

(defun hn-browse-current-comment ()
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (hn-mark-as-read id)
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
                  (propertize "User (Karma)" 'face 'hn-user)
                  (concat (propertize "Title" 'face 'hn-title)
                          (format " (Stats: Read: %s, User Table: %s, Saved %s)"
                                  (length *hn-visited*)
                                  (hash-table-size *hn-user-table*)
                                  *hn-user-table-saved-size*)))
          (make-string 80 ?-)
          "\n"))

(defun display-item (item)
  (let* ((id (cdr (assoc 'id item)))
         (title (cdr (assoc 'title item)))
         (score (cdr (assoc 'score item)))
         (url (cdr (assoc 'url item)))
         (by (cdr (assoc 'by item)))
         (user-url (format "https://news.ycombinator.com/user?id=%s" by))
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
                'help-echo user-url
                'url user-url)
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
  ;; during reload, check if we need to save some user data
  (when (> (- (hash-table-size *hn-user-table*) *hn-user-table-saved-size*) 30)
    (hn--save-user-table))
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

(defun hn-hard-reload ()
  "Clear cache."
  (interactive)
  (setq *hn-top-stories* ())
  (clrhash *hn-item-table*)
  ;; (clrhash *hn-user-table*)
  ;; (hash-table-size *hn-user-table*)
  (setq *hn-num-stories* 20)
  (hn-reload))

;;;###autoload
(defun hn ()
  "Load hn interface."
  (interactive)
  (let ((buffer (get-buffer-create "*hn*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hn-mode)
      (hn--load)
      (hn-reload))
    ;; view comment
    (pop-to-buffer buffer)))

(provide 'hn)

;;; hn.el ends here
