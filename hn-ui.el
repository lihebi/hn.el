;;; hn-ui.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hn-load-more-stories ()
  "Increase number; retrieve and display."
  (interactive)
  (setq *hn-num-stories* (+ *hn-num-stories* 50))
  (hn-reload))

(defun hn-load-all-stories ()
  "Increase number; retrieve and display."
  (interactive)
  (setq *hn-num-stories* (+ *hn-num-stories* 500))
  (hn-reload))

;; FIXME duplicate
(define-button-type 'hn-title-button
  'action                  #'browse-url-and-mark-action
  'face                    'hn-link
  'follow-link             t)

(define-button-type 'hn-title-visited
  'action                  #'browse-url-action
  'face                    'hn-link-visited
  'follow-link             t)

(define-button-type 'hn-comment-button
  'action                  #'browse-url-and-mark-action
  'face                    'font-lock-constant-face
  'follow-link             t)

(define-button-type 'hn-comment-visited
  'action                  #'browse-url-action
  'face                    'hn-link-visited
  'follow-link             t)

(define-button-type 'hn-user-button
  'action                  #'browse-url-action
  'face                    'font-lock-function-name-face
  'follow-link             t)

(define-button-type 'hn-debug-button
  'action                  #'browse-url-action
  'face                    'hn-link
  'follow-link             t)

(defun hn-mark-as-read (id)
  (add-to-list '*hn-visited* id)
  (add-to-list '*hn-visited-cache* id)
  (hn--save-visited)
  (hn-reload))

(defun hn-mark-as-unread (id)
  (setq *hn-visited* (remove id *hn-visited*))
  (setq *hn-visited-cache* (remove id *hn-visited-cache*))
  (hn--save-visited)
  (hn-reload))

(defun hn-toggle-mark-as-read ()
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (if (member id *hn-visited*)
        (hn-mark-as-unread id)
      (hn-mark-as-read id))))

(defun hn-mark-as-read-and-continue ()
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (hn-mark-as-read id)
    (next-line)))

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
  "Open comment viewer for current article."
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id)))
    (hn-mark-as-read id)
    (hn-comment id)))

(defun hn-browse-current-article ()
  "Browse the current article in browser."
  (interactive)
  (let* ((button (get-current-article-button))
         (id (button-get button 'id))
         (url (button-get button 'url)))
    (hn-mark-as-read id)
    (browse-url url)))

(defun get-current-article-button ()
  (previous-button (line-end-position)))

(defun hn--fields-to-format ()
  "Field list to format string."
  (concat (apply #'concat (mapcar (lambda (sym)
                                  (case sym
                                    (star "%-5s")
                                    (id "%-10s")
                                    (time "%-10s")
                                    (score "%-7s")
                                    (comment "%-9s")
                                    (user "%-20s")
                                    (tag "%-10s")
                                    (title "%s")
                                    (t (error "Unsupported symbol in field"))))
                                hn-fields)) "\n"))

(defun hn--fields-to-headers ()
  (mapcar (lambda (sym)
            (case sym
              (star "Star")
              (id "ID")
              (score "Score")
              (time "Time")
              (comment (propertize "Comment" 'face 'font-lock-constant-face))
              (user (propertize "User (Karma)" 'face 'font-lock-function-name-face))
              (tag "Tag")
              (title (propertize (format "Title (source: %s)"
                                         (if (equal *hn-source* 'current) *hn-source*
                                           (car (last (split-string *hn-source* "/")))))
                                 'face 'hn-title))
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

            ;; FIXME case insensitive?
            (replace-regexp-in-string
             reg (lambda (s)
                   (propertize s 'face 'red-on-white))
             acc t))
          hn-hl-keywords
          :initial-value title))

(defun hn--get-tag-face (tag)
  (assert (member tag hn-tags))
  (let ((idx (- (length hn-tags)
                (length (member tag hn-tags)))))
    (hn--string->face (elt hn-tag-faces idx))))

(defun hn--fields-to-line (item)
  "Construct field line."
  (let* ((id (cdr (assoc 'id item)))
         (title (cdr (assoc 'title item)))
         (score (cdr (assoc 'score item)))
         (url (cdr (assoc 'url item)))
         (by (cdr (assoc 'by item)))
         (time (cdr (assoc 'time item)))
         (tags (or (gethash id *hn-tag-table*) ""))
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
                (time (format-time-string "%m/%d/%y" (seconds-to-time time)))
                (comment (make-text-button
                          (format "%s" (or descendants 0)) nil
                          'type (if (member id *hn-visited*)
                                    'hn-comment-visited
                                  'hn-comment-button)
                          'id id
                          'help-echo (hn--comment-web-url id)
                          'url (hn--comment-web-url id)))
                ;; TODO assign random color to the tags
                ;; (tag (format "%s" tag))
                (tag (string-join
                      (mapcar (lambda (t)
                                (propertize t 'face
                                            (hn--get-tag-face t)))
                              tags) " "))
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

(defun hn-add-tag (tag)
  (interactive
   (list
    (completing-read "choose one tag: "
                     hn-tags nil t)))
  (assert (member tag hn-tags))
  (let ((id (button-get (get-current-article-button) 'id)))
    (let ((l (gethash id *hn-tag-table*)))
      (puthash id (remove-duplicates
                   (if l (cons tag l)
                     (list tag))
                   :test 'equal)
               *hn-tag-table*)))
  (hn--save-tag-table)
  (hn-reload))

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
                           (new (seq-filter (lambda (id) (or (not (member id *hn-visited*))
                                                             (member id *hn-visited-cache*)
                                                             (member id *hn-starred*)))
                                            (seq-take (hn-retrieve-top-stories)
                                                      *hn-num-stories*)))
                           (starred *hn-starred*)
                           (t (error "Error on *hn-list-type*"))))
               (items (mapcar #'hn-retrieve-item item-ids)))
          (mapc #'display-item items))))
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

(defun hn-reload-command ()
  (interactive)
  (setq *hn-visited-cache* '())
  (hn-reload))


(defun hn-hard-reload ()
  "Clear cache."
  (interactive)
  (setq *hn-top-stories* ())
  (clrhash *hn-item-table*)
  ;; (clrhash *hn-user-table*)
  ;; (hash-table-size *hn-user-table*)
  ;; (setq *hn-num-stories* 20)
  (hn-reload))

(provide 'hn-ui)

;;; hn-ui.el ends here
