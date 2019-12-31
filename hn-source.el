;;; hn-source.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; sources:
;; - current
;; - https://github.com/lihebi/hn-top  json/2019-07-23_04:00.json
;; In particular, this variable is either 'current or a json file path
(defvar *hn-source* 'current)

(defcustom hn-top-dir "/path/to/repo/of/hn-top/"
  "This is the local path to the hn-top repo.

It must have a json/ folder containing json files."
  :group 'hn
  :type 'string)

(define-button-type 'hn-select-source-button
  'action #'hn-select-source-action
  'face 'hn-link
  'follow-link t)

(defun hn-select-source-action (button)
  (let ((s (button-get button 'source)))
    (message (format "Source set to %s" s))
    (setq *hn-source* s)))


(define-derived-mode hn-select-source-mode special-mode "HN select source"
  :group 'hn
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun hn-select-source ()
  ;; display a list of sources to select
  ;; open a new buffer
  ;; insert options buttons
  ;; select and set
  (interactive)
  
  (let ((buffer (get-buffer-create "hn-source-selection-buffer")))
    (with-current-buffer buffer
      (hn-select-source-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Select a buffer:\n")
        (insert (make-text-button
                 "[ ] current" nil
                 'type 'hn-select-source-button
                 'source 'current
                 'help-echo "select the default current HN top"))
        (mapc (lambda (f)
                (insert "\n")

                (let* ((full-f (concat hn-top-dir "/json/" f))
                       (ids (json-read-file full-f))
                       (new-ids (seq-filter (lambda (id) (not (member id *hn-visited*)))
                                            ids))
                       (starred-ids (seq-filter (lambda (id) (member id *hn-starred*))
                                            ids)))
                  (insert (make-text-button
                           (concat (format "[ ] %s new: %s / starred: %s / all: %s"
                                           f (length new-ids)
                                           (length starred-ids) (length ids)))
                           nil
                           'type 'hn-select-source-button
                           'source (concat hn-top-dir "/json/" f)
                           'help-echo "select a json file")))
                
                )
              (seq-filter (lambda (f)
                            (string-suffix-p ".json" f))
                          (let ((dir (concat hn-top-dir "/json")))
                            (directory-files dir)))))
      (pop-to-buffer buffer))))

;; The "select source" buffer should be a date list in reversed order,
;; defaulting to recent 20 days. I should be able to list more.
;;
;; The file should be saved to /tmp maybe?
(defun list-dates (num)
  "List NUM of days, in reverse order."

  ;; this is today
  (format-time-string "%Y-%m-%d")
  ;; the reverse operation
  (date-to-time "2019-12-31")
  ;; this is current time of type Time
  (format-time-string "%Y-%m-%d" (current-time))
  ;; how to get yesterday?
  (format-time-string "%Y-%m-%d"
                      (time-add (current-time)
                                (- (* 24 60 60))))
  ;; then I can implement
  (mapcar (lambda (n)
            (format-time-string "%Y-%m-%d"
                                (time-add (current-time) (* n (- (* 24 60 60))))))
          (number-sequence 1 num)))

;; to test:
;; (list-dates 5)

(provide 'hn-source)


;;; hn-source.el ends here
