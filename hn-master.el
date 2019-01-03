;;; hn-master.el --- A hacker news client. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Scholar
;; URL: https://github.com/lihebi/hn-master.el

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;; Code:

(require 'browse-url)
(require 'cus-edit)
(require 'format-spec)
(require 'json)
(require 'url)

(define-derived-mode hn-master-mode special-mode "HN"
  :group 'hn-master
  (setq truncate-lines t)
  (buffer-disable-undo))

(defconst hn-api-prefix
  "https://hacker-news.firebaseio.com/v0")

(defvar *hn-master-top-stories* ())
(defun hn-retrieve-top-stories ()
  "Get a list of top stories."
  (when (not *hn-master-top-stories*)
    (setq *hn-master-top-stories*
          (let ((top-story-url (concat hn-api-prefix "/topstories.json")))
            (json-read-url top-story-url))))
  *hn-master-top-stories*)


(defvar *hn-master-item-table* (make-hash-table)
  "A hash map from id to item.")
(defvar *hn-master-user-table* (make-hash-table)
  "A hash map from id to item.")
(defvar *hn-master-visited* ()
  "A set of visited ids. FIXME Use hash?")

(defcustom hn-master-history-file
  (locate-user-emacs-file "hn-master-history.el")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'hn-master
  :type '(choice file (const :tag "None" nil)))

(defun json-read-url (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    ;; (let ((json-object-type 'alist)
    ;;       (json-array-type  'vector))
    ;;   (json-read))
    (json-read)))

(defun hn-retrieve-item (id)
  "Retrieve item from hash. If not available, download it."
  (when (not (gethash id *hn-master-item-table*))
    (let ((item-url (concat hn-api-prefix "/item/"
                            (number-to-string id)
                            ".json")))
      (puthash id (json-read-url item-url) *hn-master-item-table*)))
  (gethash id *hn-master-item-table*))

(defface hn-master-title
  '((t :inherit hn-master-link))
  "Face used for links to stories."
  :group 'hn-master)

(defface hn-master-link
  '((t :inherit link :underline nil))
  "Face used for links to stories."
  :group 'hn-master)

(defface hn-master-link-visited
  '((t :inherit link-visited :underline nil))
  "Face used for visited links to stories."
  :group 'hn-master)

(defface hn-master-comment-count
  '((t :foreground "dark red"))
  ""
  :group 'hn-master)

(defun browse-url-action (button)
  "Browse url when click BUTTON."
  (let* ((id    (button-get button 'id))
         (type  (button-type button))
         (url (button-get button 'url))
         (inhibit-read-only t))
    ;; mark as read
    (when (not (member id *hn-master-visited*))
      (add-to-list '*hn-master-visited* id)
      (button-put (point) 'type 'hn-master-button-visited))
    (browse-url url)))

;; (add-to-list *hn-master-visited* 3)

(define-button-type 'hn-master-title-button
  'action                  #'browse-url-action
  'face                    'hn-master-link
  'follow-link             t)

(define-button-type 'hn-master-comment-button
  'action                  #'browse-url-action
  'face                    'hn-master-comment-count
  'follow-link             t)

(define-button-type 'hn-master-button-visited
  'action                  #'browse-url-action
  'face                    'hn-master-link-visited
  'follow-link             t)

(defconst hackernews-site-item-format 
  "Format of Hacker News website item URLs.")

(defun comment-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun display-items (items)
  (let ((inhibit-read-only t))
    (mapc (lambda (item)
            (let ((id (cdr (assoc 'id item)))
                  (title (cdr (assoc 'title item)))
                  (score (cdr (assoc 'score item)))
                  (url (cdr (assoc 'url item)))
                  (descendants  (cdr (assq 'descendants item))))
              (insert
               (format "%-7s %s %s\n"
                       (propertize (format "[%s]" score))
                       ;; these buttons can be clicked
                       (make-text-button title nil
                                         'type (if (member id *hn-master-visited*)
                                                   'hn-master-button-visited
                                                 'hn-master-title-button)
                                         'id id
                                         'help-echo url
                                         'url url)
                       (make-text-button
                        (format "(%s comments)" descendants) nil
                        'type (if (member id *hn-master-visited*)
                                  'hn-master-button-visited
                                'hn-master-comment-button)
                        'id id
                        'help-echo url
                        'url (comment-url id))))))
          items)))

;;;###autoload
(defun hn-master ()
  "Load hn-master interface."
  (interactive)
  (let ((buffer (get-buffer-create "*hn-master*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hn-master-mode)
      (let ((items (mapcar #'hn-retrieve-item
                           (seq-take (hn-retrieve-top-stories) 20))))
        (display-items items)))
    ;; view comment
    (pop-to-buffer buffer)))

(provide 'hn-master)

;;; hn-master.el ends here
