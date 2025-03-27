;;; org-task.el --- The task sync code for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

;; Author: Valentin Lab <valentin.lab@kalysto.org>
;; Keywords: task, calendar
;; URL: https://github.com/0k/org-task
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'transient)
(when (not (require 'xterm-color nil 'noerror))
  (defun xterm-color-filter (input)
    input))
(require 'org-task-export)


(defcustom org-task-push-work-raw-fun 'org-task-push-work-raw-fun-cal
  "The default function to push raw works."
  :group 'org-task
  :type 'symbol)


(defcustom org-task-cal-cmdline "ecal work add_raw --start=%d --end=%d --connection=%s --task-id=%d %S"
  "The default cal cmdline to use."
  :group 'org-task
  :type 'string)


(defface org-task-process-error
  '((t (:weight bold :foreground "red")))
  "Face for process errors in `org-task-process-buffer'."
  :group 'org-task-faces)

(defface org-task-process-prefix
  '((t (:weight bold :background "#444444")))
  "Face for process prefix in `org-task-process-buffer'."
  :group 'org-task-faces)

(defface org-task-process-prefix-sep
  '((t (:weight bold :foreground "#444444")))
  "Face for process prefix separator in `org-task-process-buffer'."
  :group 'org-task-faces)

(defface org-task-process-info
  '((t (:weight bold :foreground "green")))
  "Face for process errors in `org-task-process-buffer'."
  :group 'org-task-faces)

(defface org-task-process-command
  '((t (:weight bold :foreground "white")))
  "Face for process commands in `org-task-process-buffer'."
  :group 'org-task-faces)


(defcustom org-task-process-buffer-name "*org-task-process*"
  "Name of buffer used for process output."
  :type 'string)


(defun org-task-backend-task-id-from-task-ref (task-ref)
  "Return backend and task-id from TASK-REF."
  (let ((re "^\\(\\([a-zA-Z0-9_-]+\\)/\\)?\\([0-9]+\\)"))
    (string-match re task-ref)
    (let* ((backend (if (match-end 2) (match-string 2 task-ref)))
            (task-id (match-string 3 task-ref)))
      (cons backend task-id))))


;; Push full specified work


(defun org-task-push-work (task-ref start stop project-name summary)
  "Push START STOP clock with given PROJECT-NAME and SUMMARY to TASK-REF."
  (let* ((backend-task-id (org-task-backend-task-id-from-task-ref task-ref))
          (task-id (cdr backend-task-id))
          (backend (car backend-task-id))
          (summary (substring-no-properties
                     (format "%s / %s" project-name summary))))
    (funcall org-task-push-work-raw-fun
      backend task-id (format "%d" start) (format "%d" stop) summary)))


(defun org-task-push-work-raw-fun-cal (backend task-id start stop summary)
  "Push START STOP clock with given SUMMARY to TASK-ID on BACKEND."
  (let* ((cmd (format org-task-cal-cmdline start stop backend task-id summary)))
    (shell-command-to-string cmd)))


(defun org-task--heading-pos (task-ref)
  "Return position of heading defining TASK-REF property."
  (let ((p (point)))
    (if (org-up-heading-safe)
      (let* ((cur-task-ref (org-entry-get nil "TASK_REF" t))
              (is-same-task-ref (string-equal cur-task-ref task-ref)))
        (if is-same-task-ref
          (org-task--heading-pos cur-task-ref)
          p))
      p)
    ))


(defun org-task-heading-pos ()
  "Return position of parent heading defining the TASK_REF property."
  (interactive)
  (org-with-wide-buffer
    (let ((task-ref (org-entry-get nil "TASK_REF" t)))
      (and task-ref
        (or (org-back-to-heading t)
          (org-up-heading-safe))
        (org-task--heading-pos task-ref)))))


(defun org--get-heading-path (task-ref sep)
  "Return list of task heading strings for TASK-REF, using SEP for inner headings.

Assume buffer is widened and point is on a headline.  This
function is meant to be called from another and is recursive."
  (let ((heading (let ((case-fold-search nil))
                   (looking-at org-complex-heading-regexp)
                   (if (not (match-end 4)) ""
                     ;; Remove statistics cookies.
                     (org-trim
                       (org-link-display-format
                         (replace-regexp-in-string
                           "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
                           (match-string-no-properties 4))))))))
    (if (org-up-heading-safe)
      (let* ((cur-task-ref (org-entry-get nil "TASK_REF" t))
              (is-same-task-ref (string-equal cur-task-ref task-ref)))
        (if cur-task-ref
          (if is-same-task-ref
            (let ((rest-heading-path (org--get-heading-path cur-task-ref sep)))
              (if rest-heading-path
                (cons (concat (car rest-heading-path) sep heading)
                  (cdr rest-heading-path))
                (list heading))
              )
            (cons heading (org--get-heading-path cur-task-ref sep)))
          (list heading)
          ))
      (list heading))))


(defun org-task-heading-path-full (sep)
  "Return list of task heading strings, using SEP to separate headings.

  A task is a heading with TASK_REF property.  This function will
  naviguate up from the current position to parent headings to
  collect heading and sub-heading that are not task with the SEP
  separator."
  (interactive)
  (org-with-wide-buffer
    (let ((task-ref (org-entry-get nil "TASK_REF" t)))
      (and task-ref
        (or (org-back-to-heading t)
          (org-up-heading-safe))
        (reverse (org--get-heading-path task-ref sep))))))


(defun org-task-heading-path-current (sep)
  "Return current task title, using SEP to concat parent heading."
  (interactive)
  (car (last (org-task-heading-path-full sep))))


(defun org-clockstr-to-ts (timestr)
  "Convert clock TIMESTR to UTC timestamp.

  Note that Emacs doesn't have a reliable way to read timezone information.
  And
    (equal
      (parse-time-string \"2025-01-13 Mon 19:00 CEST\")
      (parse-time-string \"2025-01-13 Mon 19:00 CET\"))

    (equal
      (date-to-time \"2025-01-13 Mon 19:00 CET\")
      (date-to-time \"2025-01-13 Mon 19:00 CEST\"))

  Are both wrongly true as of the writing of this function. So I'm
  using here `date' from the system."
  (let* ((out (string-trim
               (shell-command-to-string
                (format "date -d '%s' -u +%%s" timestr)))))
    (unless (string-match-p "\\`[0-9]+\\'" out)
      (user-error "Invalid timestamp output from date: %s" out))
    (string-to-number out)))


(defun org-task--clock-map (func)
  "Map FUNC on all next clock strings.

   FUNC is the function having one string argument that will be
   called upon each clock."
  (let* ((re (concat
               "[ \t]*"
               org-clock-string
               "[ \t]*\\[\\(.*?\\)]\\-+\\[\\(.*?\\)\\]"
               "[ \t]*=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)"
               )))
    (if (re-search-forward re nil t)
      (let* ((ts (save-match-data
                   (org-clockstr-to-ts (match-string 1))))
              (te (org-clockstr-to-ts (match-string 2))))
        (cons (funcall func ts te) (org-task--clock-map func)))
      nil)))


(defun org-task--clock-map-collect (task-ref func)
  "Map FUNC on each clock string of current task TASK-REF.

Assume current narrowing and point and on first char of
a subtree narrowed buffer of given task-ref."
  (org-task--map-collect
    task-ref
    (lambda (start end task-ref)
      (save-restriction
        (narrow-to-region start end)
        ;; XXXvlab: should build up summary, task-ref as we parse the
        ;; structure for huge performance perfs
        (org-task--clock-map func)))
    (lambda (current-heading-result children-heading-results)
      (append current-heading-result
        (apply 'append children-heading-results)
        )
      )))


(defun org-task--map-collect (task-ref func collect)
  "Map FUNC on each heading of current task TASK-REF and COLLECT them.

Assume current narrowing and point and on first char of
a subtree narrowed buffer of given task-ref.

FUNC will receive:
  - point of heading,
  - point of end of content of current heading (before first child or
    before next heading),
  - task-ref

COLLECT will receive:
  - result of current FUNC call on first heading/content
  - list of result of COLLECT on each sub-headings of same TASK-REF

The whole function will return the top-most COLLECT call."
  (let* ((current-heading-result
           (let* ((heading-content-end
                    (save-excursion
                      (or (org-goto-first-child)
                        (goto-char (point-max)))
                      (point)))
                   (p (point)))
             (funcall func p heading-content-end task-ref)))
          (task-children-headings
            (let ((children-headings nil)
                   (has-more-siblings t))
              (save-excursion
                (if (org-goto-first-child)
                  (progn
                    (while has-more-siblings
                      (while (and has-more-siblings
                               (not (string-equal task-ref
                                      (org-entry-get nil "TASK_REF" t))))
                        (setq has-more-siblings (org-get-next-sibling)))
                      (if has-more-siblings
                        (progn
                          (setq children-headings (cons (point) children-headings))
                          (setq has-more-siblings (org-get-next-sibling)))))
                    children-headings)
                  nil)))))
    (funcall collect current-heading-result
      (apply 'append (mapcar (lambda (p)
                               (save-excursion
                                 (goto-char p)
                                 (save-restriction
                                   (org-narrow-to-subtree)
                                   (org-task--map-collect task-ref func collect)
                                   )
                                 ))
                       task-children-headings
                       ) nil)
      ))
  )


(defun org-task-clock-map (func)
  "Map FUNC on each clock string of current task.

   FUNC is the function having one string argument that will be
   called upon each clock."
  (let ((task-ref (org-task-get-ref))
         (task-heading-pos (org-task-heading-pos)))
    (if task-heading-pos
      (save-excursion
        (goto-char task-heading-pos)

        (save-restriction
          (org-narrow-to-subtree)
          (org-task--clock-map-collect task-ref func)
          ))
      nil)))


(defun org-task-clock-push-current ()
  "Push current clock under point."
  (interactive)
  (org-with-wide-buffer
    (let* ((element (save-excursion (beginning-of-line)
                      (org-element-at-point-no-context)))
            (summary (org-task-heading-path-current " / "))
            (task-ref (org-task-get-ref))
            (project-name (org-task-get-categ))
            (type (org-element-type element)))
      (cond ((eq type 'clock)
              (save-excursion
                (beginning-of-line)
                (save-restriction
                  (narrow-to-region
                    (point)
                    (line-end-position))
                  (car (org-task--clock-map
                         (lambda (start stop)
                           (org-task-push-work task-ref start stop project-name summary)))
                    ))))
        (t (user-error "Not on a clock line"))))))


(defun org-task-get-ref ()
  "Return current org-task's reference or throws an error."
  (or (org-entry-get nil "TASK_REF" t)
    (user-error "Please set a TASK_REF property")))

(defun org-task-get-categ ()
  "Return current org-task's category or throws an error."
  (string-join
    (split-string (or (org-entry-get nil "TASK_CATEG" t)
                    ;;; XXXvlab: could offer to set it ?
                    (user-error "Please set a TASK_CATEG property")
                    )
      " / ")))


(defun org-task-clock-push-list ()
  "List current clocks of all headings belonging to current task."
  (interactive)
  (let* ((task-ref (org-task-get-ref))
          (org-task-push-work-raw-fun 'list))
    (org-task-clock-map
      (lambda (ts te)
        (let* ((summary (org-task-heading-path-current " / "))
                (project-name
                  (string-join
                    (split-string (or (org-entry-get nil "TASK_CATEG" t)
                                ;;; XXXvlab: could offer to set it ?
                                    (user-error "Please set a TASK_CATEG property")
                                    )
                      " / "))))
          (org-task-push-work task-ref ts te project-name summary))))))


(defun org-task-process-append-to-buffer (proc string)
  "Append to PROC buffer the given STRING."
  (let* ((pbuf (process-buffer proc)))
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (let* ((inhibit-read-only t))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (goto-char (process-mark proc))
          (let* ((pwin (get-buffer-window pbuf)))
            (if pwin
              (set-window-point pwin (point))))
          ))
      ))
  )


(defun org-task-cal-process-filter (proc string)
  "Filters PROC's STRING."
  (org-task-process-append-to-buffer proc (xterm-color-filter string)))


(defun org-task-cal-process-sentinel (proc event)
  "Reacts to PROC's EVENT to give a useful feedback."
  (let ((event-type (string-trim event))
         (pbuf (process-buffer proc)))
    (cond
      ((string-match event-type "finished")
        (org-task-process-append-to-buffer proc
          (format "\n%s%s process exited %s\n"
            (propertize "   "
              'face 'org-task-process-prefix)
            (propertize ""
              'face 'org-task-process-prefix-sep)
            (propertize "successfully"
              'face 'org-task-process-info)))
        (message "cal-process done.")
        )
      ((string-match "^exited abnormally with code \\([0-9]+\\)" event-type)
        (let ((exit-code (string-to-number (substring event-type (match-beginning 1) (match-end 1)))))
          (message "cal-process finished with errors (exit code: %d)." exit-code)
          (org-task-process-append-to-buffer proc
            (format "\n%s%s process %s (exit code: %d)\n"
              (propertize "   "
                'face 'org-task-process-prefix)
              (propertize ""
                'face 'org-task-process-prefix-sep)
              (propertize "failed"
                'face 'org-task-process-error)
               exit-code))
          (display-buffer pbuf 'display-buffer-at-bottom)
          (switch-to-buffer-other-window pbuf)))
      (t (message (format "Process: %s had the event '%s'" proc event-type))))))


(defun org-task-cal-start-process (command &optional callback)
  "Start COMMAND asynchronously and manage output.

It logs output in process windows, but also call CALLBACK with
2 arguments: output, errlvl

Logs of output are also redirected to `org-task-process-buffer' and
buffer is shown only on error."
  (let* ((pbuf (org-task--process-buffer))
          (callback-fn (or callback
                         (lambda (output errlvl) t))))
    (with-current-buffer pbuf
      (let* ((inhibit-read-only t))
        (insert
          (format "\n%s%s %s\n"
            (propertize "   "
              'face 'org-task-process-prefix)
            (propertize ""
              'face 'org-task-process-prefix-sep)
            (propertize
              (mapconcat #'shell-quote-argument command " ")
              'face 'org-task-process-command)))))
    (let (
           (output "")
           (errlvl 0))
      (make-process
        :name "cal-proc"
        :buffer pbuf
        :command command
        :connection-type nil
        :noquery t
        :filter
        (lambda (proc string)
          (org-task-cal-process-filter proc string)
          (setq output (concat output string))
          )
        :sentinel
        (lambda (proc event)
          (org-task-cal-process-sentinel proc event)
          (let ((event-type (string-trim event)))
            (cond ((string-match event-type "finished")
                    (funcall callback-fn output 0))
              ((string-match "^exited abnormally with code \\([0-9]+\\)"
                 event-type)
                (let ((exit-code (string-to-number (substring event-type (match-beginning 1) (match-end 1)))))
                  (funcall callback-fn output exit-code))
                ))
            ))))))


(defun org-task--process-buffer ()
  "Return `org-task-process-buffer-name' in `special-mode'."
  (with-current-buffer (get-buffer-create org-task-process-buffer-name)
    (unless (derived-mode-p 'special-mode) (special-mode))
    (current-buffer)))


;;;###autoload
(defun org-task-clock-push ()
  "Push current clocks of all headings belonging to current task."
  (interactive)
  (message "Sending task's clocks...")
  (let* (
          (clock-list (org-task-clock-push-list))
          (process-connection-type nil)
          (process (org-task-cal-start-process
                                        ;'("bash" "-c" "CAL_DEBUG=1 cal work add_batch -f")
                     '("cal" "work" "add_batch" "-f")
                     ))
          )  ; use a pipe
    (mapc (lambda (elt)
            (process-send-string process
              (format "%s\0"
                (string-join elt "\0"))))
      clock-list)
    (process-send-eof process)
    ))


(defun org-task--content-map-collect (task-ref)
  "Collect headings and content as strings contained in the current task TASK-REF.

Assume current narrowing and point and on first char of
a subtree narrowed buffer of given task-ref."
  (org-task--map-collect
    task-ref
    (lambda (start end task-ref)
      (save-restriction
        (narrow-to-region start end)
        (substring-no-properties (buffer-string))))
    (lambda (current-heading-result children-heading-results)
      (concat current-heading-result
        (apply 'concat (reverse children-heading-results))
        )
      )))



(defun org-task--content (task-heading-pos task-ref)
  "Collect task TASK-REF starting at TASK-HEADING-POS heading."
  (save-excursion
    (goto-char task-heading-pos)
    (save-restriction
      (org-narrow-to-subtree)
      (org-task--content-map-collect task-ref)
      )))


(defun org-task-content ()
  "Collect current task's headings."
  (let ((task-ref (org-task-get-ref))
         (task-heading-pos (org-task-heading-pos)))
    (if task-heading-pos
      (org-task--content task-heading-pos task-ref)
      nil)))

;;;###autoload
(defun org-task-set-task-ref-to-new-task ()
  "Create task on odoo's side and set task ref to it's number."
  (interactive)
  (let* ((summary (nth 4 (org-heading-components)))
          (project-name (org-task-get-categ))
          (pos (point))
          (org-task-set-task-ref
            (lambda (partner task-ref)
              (save-excursion
                (goto-char pos)
                (org-set-property "TASK_REF" (format "%s/%s" partner task-ref))
                )))
          (org-task-cal-create-task-from-summary
            (lambda (partner)
              (org-task-cal-start-process
                (list "cal" "task" "new" "-q" "-c"
                  partner (format "%s / %s" project-name summary))
                (lambda (output errlvl)
                  (if (zerop errlvl)
                    (let ((task-ref (string-to-number output)
                            ))
                      (if (zerop task-ref)
                        (user-error "Invalid task reference received")
                        (progn
                          (message "set taskref to %s/%s" partner task-ref)
                          (funcall org-task-set-task-ref partner task-ref)
                          )
                        ))
                    (user-error "Unexpected error when creating task")
                    )))))
          (partner (org-entry-get nil "TASK_PARTNER" t)))
    (if (eq partner nil)
      (org-task-cal-start-process
        '("cal" "config" "get_partners" "--with-odoo")
        (lambda (output errlvl)
          (if (eq errlvl 0)
            (progn
              (setq partner
                (completing-read "Create task for partner: "
                  (split-string output)
                  nil))
              (funcall org-task-cal-create-task-from-summary partner))
            )
          ))
      (funcall org-task-cal-create-task-from-summary)
      )
    ))


;;;###autoload
(defun org-task-edit-push-list ()
  "Get current task edit push list."
  (let* ((task-ref (org-task-get-ref))
         (backend-task-id (org-task-backend-task-id-from-task-ref task-ref))
         (task-id (cdr backend-task-id))
         (backend (car backend-task-id))
         (task-heading-pos (org-task-heading-pos))
          (summary (save-excursion
                     (goto-char task-heading-pos)
                     (nth 4 (org-heading-components))))
         (project-name (org-task-get-categ))
        )
    (if task-heading-pos
      (list
        (list backend task-id "description"
          (org-task-export-to-html (org-task--content task-heading-pos task-ref)))
        (list backend task-id "name"
          (format "%s / %s" project-name summary))
        )
      nil)))


;;;###autoload
(defun org-task-content-push ()
  "Push current heading's content to current task."
  (interactive)
  (message "Sending task's content...")
  (let* (
          (task-edit-list (org-task-edit-push-list))
          (process-connection-type nil)
          (process (org-task-cal-start-process
                     '("cal" "task" "edit_batch")
                     ))
          )  ; use a pipe
    (mapc (lambda (elt)
            (process-send-string process
              (format "%s\0"
                (string-join elt "\0"))))
      task-edit-list)
    (process-send-eof process)
    ))


;;;###autoload
(defun org-task-browse-url ()
  "Visit current task in browser."
  (interactive)
  (let* ((task-ref (org-task-get-ref))
          (url (shell-command-to-string (format "cal task url %s" task-ref))))
    (browse-url url)))


;;;###autoload (autoload 'org-task-transient "org-task")
(transient-define-prefix org-task-transient ()
  "Org Task"
  ;; ["Arguments"
  ;;  ("-s" "Switch" "--switch")
  ;;  ("-a" "Another switch" "--another")
  ;;  ("-m" "Message" "--message=")] ;; simpler
  ["Actions"
    ("w" "push to work" org-task-clock-push)
    ("d" "push to description" org-task-content-push)
    ("b" "open in browser" org-task-browse-url)
    ("C" "bind to new odoo task" org-task-set-task-ref-to-new-task)
    ])


(provide 'org-task)

;;; org-task.el ends here