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

(require 'hn-vars)
(require 'json)

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

(defconst hn-api-prefix
  "https://hacker-news.firebaseio.com/v0")

(defun hn--comment-web-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun json-read-url (url)
  (condition-case nil
      (with-temp-buffer
        (url-insert-file-contents url)
        (json-read))
    (error (progn
             (message "Error occurred!, retrying ..")
             ;; calling itself FIXME stack overflow
             (json-read-url url)
             (error "...")))))

(defun hn-retrieve-item-internal (id)
  (when (not (gethash id *hn-item-table*))
    (let ((item-url
           (format (concat hn-api-prefix "/item/%s.json") id)))
      (puthash id (json-read-url item-url) *hn-item-table*)))
  (gethash id *hn-item-table*))

(defun hn--read-file (fname)
  "Read file FNAME into an s-exp."
  (with-temp-buffer
    (insert-file-contents fname)
    (read (current-buffer))))

(defun hn--write-file (obj fname)
  "Write (pretty-print) OBJ into file FNAME."
  (with-temp-file fname
    (pp obj (current-buffer))))

(defun hn--time-to-hours (t)
  "This is to implement time-to-hours.

time-to-number-of-days and time-to-seconds agree. time-to-days seems to be different."
  (/ (time-to-seconds t) 3600))

(defun hn-retrieve-item (id)
  ;; first check if it is cached
  "Retrieve item of ID from cache or HN server."
  ;; test with '(20604419 20578976 20604119 20605660 20585637 20589216)
  (let ((fname (concat hn-history-dir "/cache/" (number-to-string id))))
    (if (and (file-exists-p fname)
             (let ((now
                    (hn--time-to-hours (current-time)))
                   (orig
                    (hn--time-to-hours (seconds-to-time (cdr (assoc 'time (hn--read-file fname))))))
                   (last-modified
                    (hn--time-to-hours
                     (file-attribute-modification-time
                      (file-attributes fname)))))
               (< (- now orig)
                  (* 5 (- last-modified orig)))))
        (progn
          ;; (message "HN: Using cached version.")
          (hn--read-file fname))
      (progn
        ;; (message "HN: Retrieving and saving.")
        (let ((item (hn-retrieve-item-internal id)))
          (hn--write-file item fname)
          item)))))

(defun hn-retrieve-user (id)
  (when (not (gethash id *hn-user-table*))
    (let* ((item-url
            (format (concat hn-api-prefix "/user/%s.json") id))
           (item (json-read-url item-url))
           ;; I only care about id and karma for now
           (simple-item (list (assoc 'id item)
                              (assoc 'karma item))))
      (puthash id simple-item *hn-user-table*)))
  (gethash id *hn-user-table*))

(defun html-wrapper (text)
  "Special treatment of html tags and entities."
  ;; replace all &xxx;
  ;; replace all <a>...</a>
  ;; replace <p> with paragraph, remove </p>
  ;; FIXME if text is nil, just return nil
  (when text
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
          wrapper-1 text)))))))

(defun hash-to-list (hash-table)
  "Return a list that represent the HASH-TABLE
Each element is a list: (list key value)."
  (let (result)
    (maphash
     (lambda (k v)
       (push (list k v) result))
     hash-table)
    result))

(defun list-to-hash (hash-list)
  (let ((res (make-hash-table :test 'equal)))
    (mapc (lambda (v)
            (puthash (car v) (cadr v) res))
          hash-list)
    res))

(defun hn-retrieve-top-stories ()
  "Get a list of top stories."
  (when (not *hn-top-stories*)
    (setq *hn-top-stories*
          (if (equal *hn-source* 'current)
              (let ((top-story-url (concat hn-api-prefix "/topstories.json")))
                (json-read-url top-story-url))
            (json-read-file *hn-source*))))
  *hn-top-stories*)

(provide 'hn-utils)

;;; hn.el ends here
