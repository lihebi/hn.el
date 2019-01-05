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
(require 'cus-edit)
(require 'format-spec)
(require 'json)
(require 'url)

(defvar hn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hn-reload)
    (define-key map "m" #'hn-load-more-stories)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "l" #'hn-list-new)
    (define-key map "L" #'hn-list-all)
    (define-key map "o" #'hn-browse-article-current)
    (define-key map (kbd "RET") #'hn-browse-comment-current)
    map)
  "Keymap used in hn buffer.")

(define-derived-mode hn-mode special-mode "HN"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

(define-derived-mode hn-comment-mode special-mode "HN-Comment"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

(defconst hn-api-prefix
  "https://hacker-news.firebaseio.com/v0")

(defvar *hn-top-stories* ())
(defun hn-retrieve-top-stories ()
  "Get a list of top stories."
  (when (not *hn-top-stories*)
    (setq *hn-top-stories*
          (let ((top-story-url (concat hn-api-prefix "/topstories.json")))
            (json-read-url top-story-url))))
  *hn-top-stories*)


(defvar *hn-item-table* (make-hash-table)
  "A hash map from id to item.")
(defvar *hn-user-table* (make-hash-table)
  "A hash map from id to item.")
(defvar *hn-visited* ()
  "A set of visited ids. FIXME Use hash?")

(defcustom hn-history-file
  (locate-user-emacs-file "hn-history.el")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'hn
  :type '(choice file (const :tag "None" nil)))

(defun json-read-url (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    ;; (let ((json-object-type 'alist)
    ;;       (json-array-type  'vector))
    ;;   (json-read))
    (json-read)))

(defun hn-retrieve-item (id)
  (when (not (gethash id *hn-item-table*))
    (let ((item-url
           (format (concat hn-api-prefix "/item/%s.json") id)))
      (puthash id (json-read-url item-url) *hn-item-table*)))
  (gethash id *hn-item-table*))

(defun hn-retrieve-user (id)
  (when (not (gethash id *hn-user-table*))
    (let ((item-url
           (format (concat hn-api-prefix "/user/%s.json") id)))
      (puthash id (json-read-url item-url) *hn-user-table*)))
  (gethash id *hn-user-table*))

(defface hn-title
  '((t :inherit hn-link))
  "Face used for links to stories."
  :group 'hn)

(defface hn-link
  '((t :inherit link :underline nil))
  "Face used for links to stories."
  :group 'hn)

(defface hn-link-visited
  '((t :inherit link-visited :underline nil))
  "Face used for visited links to stories."
  :group 'hn)

(defface hn-comment-count
  '((t :foreground "dark red"))
  ""
  :group 'hn)

(defface hn-user
  '((t :foreground "blue"))
  ""
  :group 'hn)

(defun browse-url-action (button)
  "Browse url when click BUTTON."
  (let* ((id    (button-get button 'id))
         (type  (button-type button))
         (url (button-get button 'url))
         (inhibit-read-only t))
    ;; mark as read
    (when (not (member id *hn-visited*))
      (add-to-list '*hn-visited* id)
      (button-put button 'type 'hn-button-visited))
    (browse-url url)))

(defun get-current-article-button ()
  (next-button (line-beginning-position)))

;; (add-to-list *hn-visited* 3)

(define-button-type 'hn-title-button
  'action                  #'browse-url-action
  'face                    'hn-link
  'follow-link             t)

(define-button-type 'hn-comment-button
  'action                  #'browse-url-action
  'face                    'hn-comment-count
  'follow-link             t)

(define-button-type 'hn-button-visited
  'action                  #'browse-url-action
  'face                    'hn-link-visited
  'follow-link             t)

(define-button-type 'hn-user-button
  'action                  #'browse-url-action
  'face                    'hn-user
  'follow-link             t)

(defconst hackernews-site-item-format 
  "Format of Hacker News website item URLs.")



(defun hn-ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hn-mode)
    (signal 'hn-error '("Not a hackernews buffer"))))

(defun hn-load-more-stories ()
  "Increase number; retrieve and display."
  (interactive)
  (setq *hn-num-stories* (+ *hn-num-stories* 20))
  (hn-reload))

(defvar *hn-list-all* t)

(defun hn-list-all ()
  (interactive)
  (setq *hn-list-all* t)
  (hn-reload))

(defun hn-list-new ()
  (interactive)
  (setq *hn-list-all* nil)
  (hn-reload))

(defun comment-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun display-item (item)
  (let ((id (cdr (assoc 'id item)))
        (title (cdr (assoc 'title item)))
        (score (cdr (assoc 'score item)))
        (url (cdr (assoc 'url item)))
        (descendants  (cdr (assq 'descendants item))))
    (when (or *hn-list-all*
              (not (member id *hn-visited*)))
      (insert
       (format "%-7s %s %s\n"
               (propertize (format "[%s]" score))
               ;; these buttons can be clicked
               (make-text-button title nil
                                 'type (if (member id *hn-visited*)
                                           'hn-button-visited
                                         'hn-title-button)
                                 'id id
                                 'help-echo url
                                 'url url)
               (make-text-button
                (format "(%s comments)" descendants) nil
                'type (if (member id *hn-visited*)
                          'hn-button-visited
                        'hn-comment-button)
                'id id
                'help-echo url
                'url (comment-url id)))))))

(defvar *hn-num-stories* 20 "Number of stories")

(defun decode-html-entities (html)
  (let ((data (match-data)))
    (unwind-protect
        (with-temp-buffer
          (save-excursion (insert html))
          (xml-parse-string))
      (set-match-data data))))

(defun get-a-href (s)
  (string-match "href=\"\\([^\"]*\\)\"" s)
  (match-string 1 s))

(defun get-a-text (s)
  (string-match "<a [^>]*>\\([^<]*\\)</a>" s)
  (match-string 1 s))

;; (get-href-content "<a href=\"http://xxx\"")

(defun html-wrapper (text)
  "Special treatment of html tags and entities."
  ;; replace all &xxx;
  ;; replace all <a>...</a>
  ;; replace <p> with paragraph, remove </p>
  (let ((wrapper-1 (lambda (s)
                     (replace-regexp-in-string
                      "&[0-9a-zA-Z#]*;"
                      #'decode-html-entities
                      s)))
        (wrapper-2 (lambda (s)
                     (replace-regexp-in-string
                      "<a [^>]*>[^<]*</a>"
                      (lambda (x)
                        (let ((href (get-a-href x))
                              (text (get-a-text x)))
                          (format "%s (%s)" text href)))
                      s)))
        (wrapper-3 (lambda (s)
                     (replace-regexp-in-string
                      "<p>" "\n" s)))
        (wrapper-4 (lambda (s)
                     (replace-regexp-in-string
                      "</p>\\|</a>" "" s))))
    (funcall
     wrapper-4
     (funcall
      wrapper-3
      (funcall
       wrapper-2
       (funcall
        wrapper-1 text))))))

(defun hn-reload ()
  (interactive)
  (hn-ensure-major-mode)
  (let ((pos (point))
        (winpos (window-start)))
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((items (mapcar #'hn-retrieve-item
                             (seq-take (hn-retrieve-top-stories)
                                       *hn-num-stories*))))
          (mapc #'display-item items))))
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

(defun hn-browse-article-current ()
  "Browse current line article in browser."
  (interactive)
  (let* ((button (get-current-article-button)))
    (browse-url-action button)))

(defun hn-browse-comment-current ()
  "Open this thread in emacs."
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id))
         (item (hn-retrieve-item id))
         (kids (cdr (assoc 'kids item))))
    ;; add to visited and reload
    (add-to-list '*hn-visited* id)
    (hn-reload)
    ;; process comments
    (let ((buffer (get-buffer-create "*hn-comment*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (hn-comment-mode)
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
         (user-url (format
                    ;; "https://hacker-news.firebaseio.com/v0/user/%s.json?"
                    "https://news.ycombinator.com/user?id=%s"
                    by))
         (pos (point))
         (indent (make-string (* depth 4) ? )))
    (let ((str (concat (make-text-button
                        (format "[%s (%s)]" by (hn-user-karma by)) nil
                        'type 'hn-user-button
                        'id by
                        ;; using item url, instead of user-url
                        ;; there is no score for a comment
                        'help-echo url
                        'url url)
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

;;;###autoload
(defun hn ()
  "Load hn interface."
  (interactive)
  (let ((buffer (get-buffer-create "*hn*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hn-mode)
      (hn-reload))
    ;; view comment
    (pop-to-buffer buffer)))

(provide 'hn)

;;; hn.el ends here
