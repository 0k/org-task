;;; org-task-export.el --- The task sync export code for Org

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

;; Author: Valentin Lab <valentin.lab@kalysto.org>
;; Keywords: task, calendar
;; URL: https://github.com/0k/org-task
;;
;;
;;; Commentary:
;;
;;
;; Requires dynamic bindings
;;
;;; Code:

(require 'org-macs)

(defun org-task-export-filter-src-block (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<pre "
      (concat "<pre style='"
        "color: #bbb;"
        "border-left: 0.5em solid #444;"
        "margin-top: 0.8em;"
        "padding: 0.1em 0.1em 0.1em 0.5em;"
        "border-radius: 0.2em;"
        "background-color: #121212;"
        "' ")
      text)))


(defun org-task-export-filter-fixed-width (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<pre "
      (concat "<pre style='"
        "color: #bbb;"
        "border-left: 0.5em solid #444;"
        "margin-top: 0.8em;"
        "padding: 0.1em 0.1em 0.1em 0.5em;"
        "border-radius: 0.2em;"
        "background-color: #121212;"
        "' ")
      text)))


(defun org-task-export-filter-example-block (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<pre "
      (concat "<pre style='"
        "color: #bbb;"
        "border-left: 0.5em solid #444;"
        "margin-top: 0.8em;"
        "padding: 0.1em 0.1em 0.1em 0.5em;"
        "border-radius: 0.2em;"
        "background-color: #121212;"
        "' ")
      text)))


(defun org-task-export-filter-underline (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<span "
      (concat "<span style='"
        "text-decoration: underline;"
        "' ")
      text)))


(defun org-task-export-filter-plain-list (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<ul "
      (concat "<ul style='"
        "padding-left: 1.5em;"
        "' ")
      text)))


(defun org-task-export-filter-paragraph (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (let ((out text))
      (mapc (lambda (replacement)
              (setq out
                (replace-regexp-in-string
                  (car replacement)
                  (cdr replacement) out)))
        (list
           (cons "<p>" (concat "<p style='"
                      "margin-top: 0.8em;"
                      "'>"))
           (cons "\\B@\\(\\w+\\)\\b" (concat "<span style='"
                                   "border-radius: 0.2em;"
                                   "font-size: 1em;"
                                 "background-color: #18182a;"
                                 "font-weight: bold;"
                                 "padding: 0.1em 0.4em 0.1em 0.4em;"
                                 "color: #aaa;"
                                 "'>👤 \\1</span>"))
           ))
      out)))


(defun org-task-export-filter-headline (text backend info)
  "Ensure correct styling of headline export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match-p "</h2>" text)
      (let ((out text))
        (mapc (lambda (replacement)
                (setq out
                  (replace-regexp-in-string
                    (car replacement)
                    (cdr replacement) out)))
          '(
             ("<h2\\([^>]*\\)>\\(.*\\)</h2>" . "<div style='margin-left: -0.3em; border-bottom: 2px dotted #666;'><h1\\1>◽\\2</h1></div>")
             ("<h3\\([^>]*\\)>\\(.*\\)</h3>" . "<h2\\1>◉ \\2</h2>")
             ("<h4\\([^>]*\\)>\\(.*\\)</h4>" . "<h3\\1>○ \\2</h3>")
             ("<h5\\([^>]*\\)>\\(.*\\)</h5>" . "<h4\\1>✸ \\2</h4>")
             ("<div class=\"outline-text-\\([3-9]\\)\" " . "<div class=\"outline-text-\\1\" style=\"padding-left: 1em;\" ")
             (" class=\"outline-\\([4-9]\\)\">" . " class=\"outline-\\1\" style=\"padding-left: 1em;\">")
             ("<span class=\"todo" . "<span style='color: pink;font-weight: bold;' class=\"todo")
             ("<span class=\"done" . "<span style='color: palegreen;font-weight: bold;' class=\"done")
             ("<span class=\"tag" . "<span style='float:right; font-size: 1.2rem; color: bisque; font-weight: normal; font-style: italic;'")
             ))
        out))))


(defun org-task-export-filter-timestamp (text backend info)
  "Ensure correct styling of headline export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<span "
      (concat "<span style='"
        "color: #7bb5ff;"
        "font-weight: bold;"
        "' ")
      text)))


(defun org-task-export-filter-keyword (text backend info)
  "Ensure correct styling of headline export of TEXT BACKEND INFO."
  (message "XXX received: %S" text)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<span class=\"todo"
      (concat "<span style='"
        "color: #ff7373;"
        "font-weight: bold;"
        "' class=\"todo")
      text)))


(defun org-task-export-filter-headline-style (text backend info)
  "Ensure correct styling of headline export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\\(<h[0-9] \\)"
      (concat "\\1 style='"
        "color: #ddd;"
        "margin-top: 0.6em;"
        "' ")
      text)))

(defun org-task-export-filter-quote-block (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<blockquote"
      (concat "<blockquote style='"
        "color: #bbb;"
        "border-left: 0.5em solid #446;"
        "margin-top: 0.8em;"
        "padding: 0.1em 0.1em 0.8em 0.5em;"
        "border-radius: 0.2em;"
        "background-color: #18182a;"
        "opacity: 0.9;"
        "font-style: italic;"
        "'")
      text)))

(defun org-task-export-filter-verbatim (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<code"
      (concat "<code style='"
        "color: white;"
        "font-size: 1em;"
        "'")
      text)))

(defun org-task-export-filter-code (text backend info)
  "Ensure correct styling of html export of TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<code"
      (concat "<code style='"
        "color: #ddd;"
        "font-size: 1em;"
        "border-radius: 0.2em;"
        "background-color: #222;"
        "padding: 0.2em;"
        "'")
      text)))


(defun org-task-export-to-html (text)
  "Output string html export of TEXT."
  (concat "<div style='background: black; color: #bbb; border-radius: 0.5em; padding: 1em;' >"
    "<style>pre.src { color: #bbb; }</style>"
    (let ((org-export-with-section-numbers nil)
           (org-export-with-properties nil)
           )
      (let* (
              (org-export-filter-src-block-functions
                (cons 'org-task-export-filter-src-block org-export-filter-src-block-functions))
              (org-export-filter-headline-functions
                (cons 'org-task-export-filter-headline
                  (cons 'org-task-export-filter-headline-style org-export-filter-headline-functions)))
              (org-export-filter-underline-functions
                (cons 'org-task-export-filter-underline org-export-filter-underline-functions))
              (org-export-filter-plain-list-functions
                (cons 'org-task-export-filter-plain-list org-export-filter-plain-list-functions))
              (org-export-filter-paragraph-functions
                (cons 'org-task-export-filter-paragraph org-export-filter-paragraph-functions))
              (org-export-filter-timestamp-functions
                (cons 'org-task-export-filter-timestamp org-export-filter-timestamp-functions))
              (org-export-filter-keyword-functions
                (cons 'org-task-export-filter-keyword org-export-filter-keyword-functions))
              (org-export-filter-fixed-width-functions
                (cons 'org-task-export-filter-fixed-width org-export-filter-fixed-width-functions))
              (org-export-filter-example-block-functions
                (cons 'org-task-export-filter-example-block org-export-filter-example-block-functions))
              (org-export-filter-quote-block-functions
                (cons 'org-task-export-filter-quote-block org-export-filter-quote-block-functions))
              (org-export-filter-verbatim-functions
                (cons 'org-task-export-filter-verbatim org-export-filter-verbatim-functions))
              (org-export-filter-code-functions
                (cons 'org-task-export-filter-code org-export-filter-code-functions))
              )
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (let ((org-inhibit-startup t)) (org-mode))
          (forward-line)
          (org-export-as 'html t nil t
            '(
               :with-toc nil  ;; links don't work because of odoo using hash
               :with-todo-keywords t
               :num nil
               :headline-levels 5
               )
            ))
        ))
    "</div>"))


(provide 'org-task-export)

;;; org-task-export.el ends here