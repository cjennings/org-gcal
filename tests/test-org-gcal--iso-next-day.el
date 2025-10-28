;;; test-org-gcal--iso-next-day.el --- Tests for org-gcal--iso-next-day -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--iso-next-day
;; Adds one day to an ISO date string

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--iso-next-day-normal-date-returns-next-day ()
  "Test getting next day for normal date returns day+1."
  (should (equal (org-gcal--iso-next-day "2021-03-15")
                 "2021-03-16")))

(ert-deftest test-org-gcal--iso-next-day-normal-datetime-returns-next-day-with-time ()
  "Test getting next day for datetime returns day+1 preserving time."
  (should (equal (org-gcal--iso-next-day "2021-03-15T14:30")
                 "2021-03-16T14:30")))

;;; Boundary Cases

(ert-deftest test-org-gcal--iso-next-day-boundary-end-of-month-rolls-to-next-month ()
  "Test getting next day at end of month rolls to next month."
  (should (equal (org-gcal--iso-next-day "2021-03-31")
                 "2021-04-01")))

(ert-deftest test-org-gcal--iso-next-day-boundary-end-of-year-rolls-to-next-year ()
  "Test getting next day at end of year rolls to next year."
  (should (equal (org-gcal--iso-next-day "2021-12-31")
                 "2022-01-01")))

(ert-deftest test-org-gcal--iso-next-day-boundary-leap-year-handles-feb-28-29 ()
  "Test getting next day on leap year handles Feb 28->29->Mar 1."
  (should (equal (org-gcal--iso-next-day "2020-02-28")
                 "2020-02-29"))
  (should (equal (org-gcal--iso-next-day "2020-02-29")
                 "2020-03-01")))

(ert-deftest test-org-gcal--iso-next-day-boundary-non-leap-year-feb-28-to-mar-1 ()
  "Test getting next day on non-leap year Feb 28 goes to Mar 1."
  (should (equal (org-gcal--iso-next-day "2021-02-28")
                 "2021-03-01")))

;;; Error Cases

(ert-deftest test-org-gcal--iso-next-day-error-nil-input-returns-nil ()
  "Test getting next day with nil input returns nil."
  (should (equal (org-gcal--iso-next-day nil) nil)))

(ert-deftest test-org-gcal--iso-next-day-error-empty-string-returns-nil ()
  "Test getting next day with empty string returns nil."
  (should (equal (org-gcal--iso-next-day "") nil)))

(ert-deftest test-org-gcal--iso-next-day-error-malformed-date-produces-date ()
  "Test getting next day with malformed date still produces date string."
  ;; Malformed strings still parse (with zeros) and produce dates
  ;; This documents current behavior - could validate input better
  (let ((result (org-gcal--iso-next-day "not-a-date")))
    (should (stringp result))
    (should (string-prefix-p "-" result))))

(ert-deftest test-org-gcal--iso-next-day-error-non-iso-format-produces-date ()
  "Test getting next day with non-ISO format produces date string."
  ;; Non-ISO strings partially parse and produce strange but valid dates
  ;; This documents current behavior - could validate input better
  (let ((result (org-gcal--iso-next-day "03/15/2021")))
    (should (stringp result))
    ;; Parses "03" as year 3, returns strange date
    (should (string-match-p "^[0-9-]+" result))))

(provide 'test-org-gcal--iso-next-day)
;;; test-org-gcal--iso-next-day.el ends here
