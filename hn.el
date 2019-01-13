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

(defface gold-on-white
  '((t :foreground "dark orange")) "" :group 'hn)

(defface red-on-white
  '((t :foreground "red")) "" :group 'hn)

(defcustom hn-hl-users '()
  "A list of users to follow. Articles or comments by these users
  are highlighted."
  :group 'hn
  :type '(list string))

(defcustom hn-hl-keywords '()
  "A list of keywords regexp you care about. Articles and
  comments containing these keywords are highlighted."
  :group 'hn
  :type '(list string))

(defcustom hn-history-dir
  (locate-user-emacs-file "hn-history")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'hn
  :type '(choice file (const :tag "None" nil)))

(defcustom hn-fields
  '(star score comment user title)
  "Fields on article page. Title should be at the end because its
length is most variable."
  :group 'hn
  :type '(list symbol))


(defvar *hn-top-stories* ())
(defvar *hn-item-table* (make-hash-table :test 'equal)
  "A hash map from id to item.")
(defvar *hn-user-table* (make-hash-table :test 'equal)
  "A hash map from id to item.")
(defvar *hn-visited* ()
  "A set of visited ids. FIXME Use hash?")
(defvar *hn-num-stories* 20 "Number of stories")

;; possible values:
;; 'all, 'new, 'starred
(defvar *hn-list-type* 'all)
(defvar *hn-starred* '())


(defun hn-load-more-stories ()
  "Increase number; retrieve and display."
  (interactive)
  (setq *hn-num-stories* (+ *hn-num-stories* 20))
  (hn-reload))

(defun hn-list-all ()
  (interactive)
  (setq *hn-list-type* 'all)
  (hn-reload))

(defun hn-list-new ()
  (interactive)
  (setq *hn-list-type* 'new)
  (hn-reload))

(defun hn-list-starred ()
  "List only starred articles."
  (interactive)
  (setq *hn-list-type* 'starred)
  (hn-reload))

(defun hn-list-cycle ()
  (interactive)
  (setq *hn-list-type*
        (case *hn-list-type*
          (all (hn-new-mode)
               'new)
          (new (hn-starred-mode)
               'starred)
          (starred (hn-all-mode)
                   'all)
          (t (error "hn-list-type error"))))
  (hn-reload))

(defun hn--save-visited ()
  (hn--save-to-file *hn-visited* "hn-visited.el"))
(defun hn--load-visited ()
  (setq *hn-visited* (hn--load-from-file "hn-visited.el")))
(defun hn--save-starred ()
  (hn--save-to-file *hn-starred* "hn-starred.el"))
(defun hn--load-starred ()
  (setq *hn-starred* (hn--load-from-file "hn-starred.el")))

;; (pp *hn-user-table*)
;; (pp *hn-starred*)
;; (hn--save-user-table)

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
  (when (not (file-exists-p (concat hn-history-dir "/hn-starred.el")))
    (hn--save-starred))
  (when (not (file-exists-p (concat hn-history-dir "/hn-user-table.el")))
    (hn--save-user-table)))

(defun hn--save-to-file (obj filename)
  (with-temp-file (concat hn-history-dir "/" filename)
    (pp obj (current-buffer))))

(defun hn--load-from-file (filename)
  (with-temp-buffer
    (insert-file-contents (concat hn-history-dir "/" filename))
    (read (current-buffer))))

(defun hn--save ()
  "Save all information: history, user table, etc."
  (ensure-history-dir)
  (hn--save-visited)
  (hn--save-starred)
  (hn--save-user-table))

(defun hn--load ()
  "Load all info."
  (ensure-history-dir)
  (hn--load-visited)
  (hn--load-starred)
  (hn--load-user-table))


(defvar hn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hn-reload)
    (define-key map "G" #'hn-hard-reload)
    (define-key map "m" #'hn-load-more-stories)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    ;; (define-key map (kbd "ln") #'hn-list-new)
    ;; (define-key map (kbd "la") #'hn-list-all)
    ;; (define-key map (kbd "ls") #'hn-list-starred)
    (define-key map "l" #'hn-list-cycle)
    (define-key map "c" #'hn-browse-current-comment)
    (define-key map "u" #'hn-toggle-mark-as-read)
    (define-key map "s" #'hn-toggle-star)
    (define-key map (kbd "RET") #'hn-browse-current-comment)
    map)
  "Keymap used in hn buffer.")

(define-derived-mode hn-mode special-mode "HN"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

(define-derived-mode hn-all-mode hn-mode "HN-ALL"
  :group 'hn)
(define-derived-mode hn-starred-mode hn-mode "HN-STARRED"
  :group 'hn)
(define-derived-mode hn-new-mode hn-mode "HN-NEW"
  :group 'hn)

;; FIXME duplicate
(define-button-type 'hn-title-button
  'action                  #'browse-url-and-mark-action
  'face                    'hn-link
  'follow-link             t)

(define-button-type 'hn-debug-button
  'action                  #'browse-url-action
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

(defun hn-mark-as-unread (id)
  (setq *hn-visited* (remove id *hn-visited*))
  (hn--save-visited)
  (hn-reload))

(defun hn-toggle-mark-as-read ()
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (if (member id *hn-visited*)
        (hn-mark-as-unread id)
      (hn-mark-as-read id))))

(defun hn-toggle-star ()
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (if (member id *hn-starred*)
        (setq *hn-starred* (remove id *hn-starred*))
      (add-to-list '*hn-starred* id))
    (hn--save-starred)
    (hn-reload)))

(defun browse-url-action (button)
  "Browse url when click BUTTON."
  (browse-url (button-get button 'url)))

(defun browse-url-and-mark-action (button)
  "Browse url when click BUTTON."
  (let ((id (button-get button 'id))
        (url (button-get button 'url)))
    ;; CAUTION: after reload, the button is not there anymore, so
    ;; button-get must be called before hn-mark-as-read (which calls
    ;; hn-reload)
    (hn-mark-as-read id)
    (browse-url url)))

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

(defun hn--fields-to-format ()
  "Field list to format string."
  (concat (apply #'concat (mapcar (lambda (sym)
                                  (case sym
                                    (star "%-5s")
                                    (id "%-10s")
                                    (score "%-7s")
                                    (comment "%-9s")
                                    (user "%-20s")
                                    (title "%s")
                                    (t (error "Unsupported symbol in field"))))
                                hn-fields)) "\n"))

(defun hn--fields-to-headers ()
  (mapcar (lambda (sym)
            (case sym
              (star "Star")
              (id "ID")
              (score "Score")
              (comment (propertize "Comment" 'face 'hn-comment-count))
              (user (propertize "User (Karma)" 'face 'hn-user))
              (title (propertize "Title" 'face 'hn-title))
              (t (error "Unsupported symbol in field"))))
          hn-fields))

(defun display-header ()
  "Insert headers."
  (insert (apply #'format (hn--fields-to-format)
                 (hn--fields-to-headers))
          (make-string 80 ?-)
          "\n"))

(defun user-fontifier (user)
  "Fontify user if USER in hn-hl-users."
  (if (member user hn-hl-users)
      (propertize user 'face 'red-on-white)
    user))

(defun title-fontifier (title)
  (reduce (lambda (acc reg)
            ;; the FIXEDCASE has to be t, i.e. do not adjust case
            ;; automatically. Otherwise, if the string contains
            ;; non-ASCII code, the propertization might fail. E.g.
            ;; "Show HN: author markdeck – author", the dash is
            ;; unicode 8211. Try to replace "Show" is not working.
            (replace-regexp-in-string
             reg (lambda (s)
                   (propertize s 'face 'red-on-white))
             acc t))
          hn-hl-keywords
          :initial-value title))

(defun hn--fields-to-line (item)
  "Construct field line."
  (let* ((id (cdr (assoc 'id item)))
         (title (cdr (assoc 'title item)))
         (score (cdr (assoc 'score item)))
         (url (cdr (assoc 'url item)))
         (by (cdr (assoc 'by item)))
         (user-url (format "https://news.ycombinator.com/user?id=%s" by))
         (descendants  (cdr (assq 'descendants item))))
    (mapcar (lambda (sym)
              (case sym
                (star (propertize
                       (if (member id *hn-starred*)
                           "Y" " ")
                       'face
                       'gold-on-white))
                (id (make-text-button
                     (format "%s" id) nil
                     'type 'hn-debug-button
                     'id id
                     'help-echo (format (concat hn-api-prefix "/item/%s.json") id)
                     'url (format (concat hn-api-prefix "/item/%s.json") id)))
                (score (format "%s" score))
                (comment (make-text-button
                          (format "%s" (or descendants 0)) nil
                          'type (if (member id *hn-visited*)
                                    'hn-comment-visited
                                  'hn-comment-button)
                          'id id
                          'help-echo (hn--comment-web-url id)
                          'url (hn--comment-web-url id)))
                (user (make-text-button
                       (format "%s (%s)"
                               (user-fontifier by)
                               (hn-user-karma by))
                       nil
                       'type 'hn-user-button
                       'id by
                       ;; using item url, instead of user-url
                       ;; there is no score for a comment
                       'help-echo user-url
                       'url user-url))
                (title (make-text-button (title-fontifier title) nil
                                         'type (if (member id *hn-visited*)
                                                   'hn-title-visited
                                                 'hn-title-button)
                                         'id id
                                         'help-echo url
                                         'url url))
                (t (error "Unsupported symbol in field"))))
            hn-fields)))

(defun display-item (item)
  (insert
   (apply #'format (hn--fields-to-format)
          (hn--fields-to-line item))))

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
        (let* ((item-ids (case *hn-list-type*
                           (all (seq-take (hn-retrieve-top-stories)
                                          *hn-num-stories*))
                           (new (seq-filter (lambda (id) (not (member id *hn-visited*)))
                                            (seq-take (hn-retrieve-top-stories)
                                                      *hn-num-stories*)))
                           (starred *hn-starred*)
                           (t (error "Error on *hn-list-type*"))))
               (items (mapcar #'hn-retrieve-item item-ids)))
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
  ;; (setq *hn-num-stories* 20)
  (hn-reload))

;;;###autoload
(defun hn ()
  "Load hn interface."
  (interactive)
  (let ((buffer (get-buffer-create "*hn*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hn-all-mode)
      (hn--load)
      (hn-reload))
    ;; view comment
    (pop-to-buffer buffer)))

(provide 'hn)

;;; hn.el ends here
