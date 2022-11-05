;;; test-org-task.el --- tests for org-task.el

;; Copyright (c)  Valentin Lab
;; Authors: Valentin Lab

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(eval-and-compile (require 'cl-lib))


;;; Commentary:


(let ((org-test-file-path
        (concat
          (file-name-directory (file-chase-links (find-lisp-object-file-name 'org-mode 'defun)))
          "../testing/org-test.el"))
       )
  (if (file-readable-p org-test-file-path)
    (load org-test-file-path)
    (error "Can't find 'org-test.el'. Please provide a source version of 'org-mode'.")))



(ert-deftest test-org-task/org-task-push-work ()
  "Test `org-task-push-work' specifications."
  (should
    (equal '("b" "1" 0 3600 "foo / my summary")
      (let ((org-task-push-work-raw-fun 'list))
        (org-task-push-work "b/1" 0 3600 "foo" "my summary")))))


(ert-deftest test-org-task/org-task-push-work-raw-fun-cal ()
  "Test `org-task-push-work-raw-fun-cal' specifications."
  (should
    (equal "cal work add_raw --start=0 --end=3600 --connection=\"b\" --task-id=1 \"foo / my summary\"\n"
      (let ((org-task-cal-cmdline "echo 'cal work add_raw --start=%d --end=%d --connection=%S --task-id=%d %S'"))
        (org-task-push-work-raw-fun-cal "b" 1 0 3600 "foo / my summary")))))



(ert-deftest test-org-task/org-task-heading-pos ()
  "Test `org-task-heading-pos' specifications."
  (should
    (equal nil
      (org-test-with-temp-text "
* H

** H1

<point>
"
        (org-task-heading-pos))))
  ;;; XXXvlab: I don't understand when buffer level properties
  ;;; are supposed to be usable. Here there are not. I a file,
  ;;; I have to call C-c C-c to make them work.
  ;;   (should
  ;;     (equal 24
  ;;       (org-test-with-temp-text "#+PROPERTY: TASK_CATEG cal
  ;; #+PROPERTY: TASK_REF foo/1

  ;; * H

  ;; ** H1

  ;; <point>

  ;; "
  ;;         ;; (org-entry-get (point) "TASK_CATEG" t)
  ;;         (org-entry-get-with-inheritance "TASK_CATEG")
  ;;         ;; (org-task-heading-pos)
  ;;         )))
  (should
    (equal 2
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

<point>

** H1

"
        (org-task-heading-pos))))
  (should
    (equal 2
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

** H1

<point>
"
        (org-task-heading-pos))))
  (should
    (equal nil
      (org-test-with-temp-text "
* H

** H0

<point>

** H1

  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

"
        (org-task-heading-pos))))
  (should
    (equal 49
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

** H1
  :PROPERTIES:
  :TASK_REF: hop/2
  :END:

<point>

"
        (org-task-heading-pos))))
  (should
    (equal 1
      (org-test-with-temp-text "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
<point>

"
        (org-task-heading-pos))))

  )


(ert-deftest test-org-task/org-task-heading-path-full ()
  "Test `org-task-heading-path-full' specifications."
  (should
    (equal nil
      (org-test-with-temp-text "
* H

** H1

<point>
"
        (org-task-heading-path-full "%"))))
  (should
    (equal '("H")
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

<point>

** H1

"
        (org-task-heading-path-full "%"))))
  (should
    (equal '("H%H1")
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

** H1

<point>
"
        (org-task-heading-path-full "%"))))
  (should
    (equal nil
      (org-test-with-temp-text "
* H

** H0

<point>

** H1

  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

"
        (org-task-heading-path-full "%"))))
  (should
    (equal '("H" "H1")
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

** H1
  :PROPERTIES:
  :TASK_REF: hop/2
  :END:

<point>

"
        (org-task-heading-path-full "%"))))
  (should
    (equal '("H%Hb" "Hba")
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

** Ha
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** Hb

*** Hba
  :PROPERTIES:
  :TASK_REF: hop/2
  :END:

<point>

"
        (org-task-heading-path-full "%"))))
  (should
    (equal '("H" "Hb%Hba")
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: foo/1
  :END:

** Ha
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** Hb
  :PROPERTIES:
  :TASK_REF: hop/2
  :END:

*** Hba

<point>

"
        (org-task-heading-path-full "%"))))
  )


;;
;; org-task--clock-map
;;


(ert-deftest test-org-task/org-task--clock-map ()
  "Test `org-task--clock-map' specifications."
  (should
    (equal '((1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "
<point>
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task--clock-map (lambda (ts te) (cons ts te))))))
  (should
    (equal '((1662391800.0 . 1662393600.0) (1662391800.0 . 1662393660.0))
      (org-test-with-temp-text "
<point>
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:01 CEST] =>  0:30
"
        (org-task--clock-map (lambda (ts te) (cons ts te))))))

  )


;;
;; org-task-clock-map
;;

(ert-deftest test-org-task/org-task-clock-map ()
  "Test `org-task-clock-map' specifications."
  (should-error
    (org-test-with-temp-text "
* H
a<point>
** Ha
b
"
      (org-task-clock-map (lambda (ts te) (cons ts te))))
    :type 'user-error)

  (should
    (equal 'nil
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

a<point>
** Ha
b
"
        (org-task-clock-map (lambda (ts te) (cons ts te))))))
  (should
    (equal '((1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "
*<point> H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30

a
** Ha
b
"
        (org-task--clock-map-collect "bar/3" (lambda (ts te) (cons ts te)))
        )))
  ;; Get task heading clocks from root heading
  (should
    (equal '((1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30

a<point>
** Ha
b
"
        (org-task-clock-map (lambda (ts te) (cons ts te)))
        )))
  (should
    (equal '((1662391800.0 . 1662393600.0)
              (1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "
* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30

a<point>
** Ha
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
b
"
        (org-task-clock-map (lambda (ts te) (cons ts te))))))
  (should
    (equal '((1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "

* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30

a
** Ha
b
<point>
"
        (org-task-clock-map (lambda (ts te) (cons ts te))))))
  (should
    (equal '((0.0 . 60.0) (60.0 . 120.0))
      (org-test-with-temp-text "

* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01

a
** Ha
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01
b
<point>
"
        (org-task-clock-map (lambda (ts te) (cons ts te))))))
  (should
    (equal '((60.0 . 120.0))
      (org-test-with-temp-text "

* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01

a
** Ha
  :PROPERTIES:
  :TASK_REF: foo/2
  :END:
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01
b
<point>
"
        (org-task-clock-map (lambda (ts te) (cons ts te))))))
  )



(ert-deftest test-org-task/org-task--clock-map-collect ()
  "Test `org-task--clock-map-collect' specifications."
  (should
    (equal '((1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task--clock-map-collect "bar/3" (lambda (ts te) (cons ts te))))))
  (should
    (equal '((1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** H1
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task--clock-map-collect "bar/3" (lambda (ts te) (cons ts te))))))
  (should
    (equal '((1662391800.0 . 1662393600.0) (1662391800.0 . 1662393600.0))
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30

** H1
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task--clock-map-collect "bar/3" (lambda (ts te) (cons ts te))))))
  (should
    (equal '((0.0 . 60.0) (60.0 . 120.0))
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01

** H1
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01

** H2
  :PROPERTIES:
  :TASK_REF: foo/3
  :END:
  CLOCK: [1970-01-01 Thu 01:02 UTC]--[1970-01-01 Thu 01:03 UTC] =>  0:01

"
        (org-task--clock-map-collect "bar/3" (lambda (ts te) (cons ts te))))))

  )


;;
;; org-task--content-map-collect
;;


(ert-deftest test-org-task/org-task--content-map-collect ()
  "Test `org-task--content-map-collect' specifications."
  (should
    (equal "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task--content-map-collect "bar/3"))))

  (should
    (equal "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** H1
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** H1
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task--content-map-collect "bar/3"))))
  (should
    (equal "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01

** H1
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01
** H3

wiz
"
      (org-test-with-temp-text "<point>* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01

** H1
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01

** H2
  :PROPERTIES:
  :TASK_REF: foo/3
  :END:
  CLOCK: [1970-01-01 Thu 01:02 UTC]--[1970-01-01 Thu 01:03 UTC] =>  0:01

** H3

wiz
"
        (org-task--content-map-collect "bar/3"))))

  )


;;
;; org-task-content
;;


(ert-deftest test-org-task/org-task-content ()
  "Test `org-task-content' specifications."
  (should
    (equal "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30


"
      (org-test-with-temp-text "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
<point>

"
        (org-task-content))))

  (should
    (equal "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** H1
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
      (org-test-with-temp-text "* H<point>
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:

** H1
  CLOCK: [2022-09-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task-content))))
  (should
    (equal "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01

** H1
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01
** H3

wiz
"
      (org-test-with-temp-text "* H
  :PROPERTIES:
  :TASK_REF: bar/3
  :END:
  CLOCK: [1970-01-01 Thu 01:00 UTC]--[1970-01-01 Thu 01:01 UTC] =>  0:01
<point>
** H1
  CLOCK: [1970-01-01 Thu 01:01 UTC]--[1970-01-01 Thu 01:02 UTC] =>  0:01

** H2
  :PROPERTIES:
  :TASK_REF: foo/3
  :END:
  CLOCK: [1970-01-01 Thu 01:02 UTC]--[1970-01-01 Thu 01:03 UTC] =>  0:01

** H3

wiz
"
        (org-task-content))))

  )


;;
;; org-task-clock-push-current
;;


(ert-deftest test-org-task/org-task-clock-push-current ()
  "Test `org-task-clock-push-current' specifications."
  (let ((org-task-cal-cmdline "echo 'cal work add_raw --start=%d --end=%d --connection=%S --task-id=%s %S'"))
    ;;; Missing TASK_CATEG should be triggered
    (should-error
      (org-test-with-temp-text "
  CLOCK: [2022-0<point>9-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task-clock-push-current))
      :type 'user-error)
    ;;     (should
    ;;       (equal "cal work add_raw --start=0 --end=3600 --connection=\"b\" --task-id=1 \"foo / my summary\"\n"
    ;;         (org-test-with-temp-text "
    ;; #+PROPERTY: TASK_CATEG 3

    ;;   CLOCK: [2022-0<point>9-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
    ;; "
    ;;           (org-entry-get nil "TASK_CATEG" t)
    ;;           ;(org-task-clock-push-current)
    ;;           )))
    (should-error
      (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :END:


** local
  CLOCK: [2022-0<point>9-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task-clock-push-current)
        )
      :type 'user-error)

    (should
      (equal "cal work add_raw --start=0 --end=3600 --connection=\"foo\" --task-id=1 \"org-task / local\"\n"
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :END:


** local
  :PROPERTIES:
  :TASK_REF:    foo/1
  :END:
  CLOCK: [1970-0<point>1-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-current)
          )))

    (should
      (equal "cal work add_raw --start=0 --end=3600 --connection=\"foo\" --task-id=1 \"org-task / local\"\n"
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :END:


** local
  :PROPERTIES:
  :TASK_REF:    foo/1
  :END:
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-current)
          )))
    (should
      (equal "cal work add_raw --start=0 --end=3600 --connection=\"foo\" --task-id=1 \"org-task / global / local2\"\n"
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :TASK_REF:    foo/1
  :END:


** local1
  :PROPERTIES:
  :TASK_REF:    foo/2
  :END:

** local2
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-current)
          )))
    (should
      (equal "cal work add_raw --start=0 --end=3600 --connection=\"foo\" --task-id=1 \"org-task / global / local2\"\n"
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :TASK_REF:    foo/1
  :END:


** local1
  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
** local2
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-current)
          )))
    ))


;;
;; org-task-clock-push-list
;;


(ert-deftest test-org-task/org-task-clock-push-list ()
  "Test `org-task-clock-push-list' specifications."
  (let ((org-task-cal-cmdline "echo 'cal work add_raw --start=%d --end=%d --connection=%S --task-id=%d %S'"))
    ;;; Missing TASK_CATEG should be triggered
    (should-error
      (org-test-with-temp-text "
  CLOCK: [2022-0<point>9-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task-clock-push-list))
      :type 'user-error)
    ;;     (should
    ;;       (equal "cal work add_raw --start=0 --end=3600 --connection=\"b\" --task-id=1 \"foo / my summary\"\n"
    ;;         (org-test-with-temp-text "
    ;; #+PROPERTY: TASK_CATEG 3

    ;;   CLOCK: [2022-0<point>9-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
    ;; "
    ;;           (org-entry-get nil "TASK_CATEG" t)
    ;;           ;(org-task-clock-push-list-current)
    ;;           )))


    ;;; Missing TASK_REF should be triggered
    (should-error
      (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :END:


** local
  CLOCK: [2022-0<point>9-05 Mon 17:30 CEST]--[2022-09-05 Mon 18:00 CEST] =>  0:30
"
        (org-task-clock-push-list)
        )
      :type 'user-error)

    (should
      (equal '(("foo" "1" 0.0 3600.0 "org-task / local"))
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :END:


** local
  :PROPERTIES:
  :TASK_REF:    foo/1
  :END:
  CLOCK: [1970-0<point>1-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-list)
          )))

    (should
      (equal '(("foo" "1" 0.0 3600.0 "org-task / local"))
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :END:


** local
  :PROPERTIES:
  :TASK_REF:    foo/1
  :END:
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-list)
          )))
    (should
      (equal '(("foo" "1" 0.0 3600.0 "org-task / global / local2"))
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :TASK_REF:    foo/1
  :END:


** local1
  :PROPERTIES:
  :TASK_REF:    foo/2
  :END:

** local2
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-list)
          )))
    (should
      (equal '(("foo" "1" 0.0 3600.0 "org-task / global / local2"))
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :TASK_REF:    foo/1
  :END:


** local1
  :PROPERTIES:
  :TASK_REF:    foo/2
  :END:
CLOCK: [1970-01-01 Thu 02:00 CEST]--[1970-01-01 Thu 03:00 CEST] =>  0:01
** local2
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-list)
          )))
    (should
      (equal '(("foo" "1" 7200.0 10800.0 "org-task / global")
                ("foo" "1" 0.0 3600.0 "org-task / global / local2")
                )
        (org-test-with-temp-text "

* global
  :PROPERTIES:
  :TASK_CATEG:    org-task
  :TASK_REF:    foo/1
  :END:
CLOCK: [1970-01-01 Thu 03:00 CEST]--[1970-01-01 Thu 04:00 CEST] =>  0:01


** local1
  :PROPERTIES:
  :TASK_REF:    foo/2
  :END:
CLOCK: [1970-01-01 Thu 02:00 CEST]--[1970-01-01 Thu 03:00 CEST] =>  0:01
** local2
<point>  CLOCK: [1970-01-01 Thu 01:00 CEST]--[1970-01-01 Thu 02:00 CEST] =>  0:01
"
          (org-task-clock-push-list)
          )))
    ))


(defun org-task-test-run-batch-tests (&optional org-test-selector)
  "Run all tests matching ORG-TEST-SELECTOR regex defaults to \"\\(org\\|ob\\)\".

Load all test files first."
  (interactive)
  (let ((org-test-selector
          (if org-test-selector org-test-selector "org-task"))
         org-confirm-babel-evaluate org-startup-folded vc-handled-backends)
    (message "selected tests: %s" org-test-selector)
    (ert-run-tests-batch-and-exit org-test-selector)))

;;; test-org-task.el ends here
