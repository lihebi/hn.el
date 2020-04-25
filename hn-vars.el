;;; hn-vars.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

(defface gold-on-white
  '((t :foreground "dark orange")) "" :group 'hn)
(defface red-on-white
  '((t :foreground "red")) "" :group 'hn)
(defface blue-on-white
  '((t :foreground "blue")) "" :group 'hn)
(defface green-on-white
  '((t :foreground "green")) "" :group 'hn)

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

(defcustom hn-tags '()
  "A list of tags to mark articles."
  :group 'hn
  :type '(list string))
(defcustom hn-tag-faces '("red" "blue" "green" "gold")
  "A list of colors."
  :group 'hn
  :type '(list string))

(defun hn--string->face (color)
  (pcase color
    ("red" 'red-on-white)
    ("gold" 'gold-on-white)
    ("green" 'green-on-white)
    ("blue" 'blue-on-white)))

(defcustom hn-history-dir
  (locate-user-emacs-file "hn-history")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'hn
  :type '(choice file (const :tag "None" nil)))

(defcustom hn-export-json-file
  (concat (locate-user-emacs-file "hn-history") "/starred.json")
  "The exporting json file for web serving."
  :group 'hn
  :type '(choice file (const :tag "None" nil)))

(defcustom hn-fields
  '(star score comment user title)
  "Fields on article page.  Title should be at the end because its
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
(defvar *hn-visited-cache* ()
  "Will clear after each `g' command.")
(defvar *hn-num-stories* 0 "Number of stories")
(defvar *hn-tag-table* (make-hash-table :test 'equal))

;; possible values:
;; 'all, 'new, 'starred
(defvar *hn-list-type* 'new)
(defvar *hn-starred* '())



(defun hn--save-visited ()
  (hn--save-to-file (mapcar #'list *hn-visited*) "hn-visited.el"))
(defun hn--load-visited ()
  (setq *hn-visited* (apply #'append (hn--load-from-file "hn-visited.el"))))
(defun hn--save-starred ()
  (hn--save-to-file (mapcar #'list *hn-starred*) "hn-starred.el"))
(defun hn--load-starred ()
  (setq *hn-starred* (apply #'append (hn--load-from-file "hn-starred.el"))))

(defun ensure-history-dir ()
  (when (not (file-exists-p hn-history-dir))
    (make-directory hn-history-dir))
  (when (not (file-exists-p (concat hn-history-dir "/cache")))
    (make-directory (concat hn-history-dir "/cache")))
  (when (not (file-exists-p (concat hn-history-dir "/hn-visited.el")))
    (hn--save-visited))
  (when (not (file-exists-p (concat hn-history-dir "/hn-starred.el")))
    (hn--save-starred))
  (when (not (file-exists-p (concat hn-history-dir "/hn-user-table.el")))
    (hn--save-user-table))
  (when (not (file-exists-p (concat hn-history-dir "/hn-tag-table.el")))
    (hn--save-tag-table)))

(defun hn--save ()
  "Save all information: history, user table, etc."
  (ensure-history-dir)
  (hn--save-visited)
  (hn--save-starred)
  (hn--save-user-table)
  (hn--save-tag-table))

(defun hn--load ()
  "Load all info."
  (ensure-history-dir)
  (hn--load-visited)
  (hn--load-starred)
  (hn--load-user-table)
  (hn--load-tag-table))


(defvar *hn-user-table-saved-size* 0)
(defun hn--save-user-table ()
  (hn--save-to-file (hash-to-list *hn-user-table*) "hn-user-table.el")
  (setq *hn-user-table-saved-size* (hash-table-size *hn-user-table*)))
(defun hn--load-user-table ()
  (setq *hn-user-table* (list-to-hash (hn--load-from-file "hn-user-table.el")))
  (setq *hn-user-table-saved-size* (hash-table-size *hn-user-table*)))

(defun hn--save-tag-table ()
  (hn--save-to-file (hash-to-list *hn-tag-table*) "hn-tag-table.el"))
(defun hn--load-tag-table ()
  (setq *hn-tag-table* (list-to-hash (hn--load-from-file "hn-tag-table.el"))))

(defun hn--save-to-file (obj filename)
  (with-temp-file (concat hn-history-dir "/" filename)
    (pp obj (current-buffer))))

(defun hn--load-from-file (filename)
  (with-temp-buffer
    (insert-file-contents (concat hn-history-dir "/" filename))
    (read (current-buffer))))

(provide 'hn-vars)

;;; hn-vars.el ends here
