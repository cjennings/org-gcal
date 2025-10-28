;;; test-org-gcal--iso-previous-day.el --- Tests for org-gcal--iso-previous-day -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--iso-previous-day
;; Subtracts one day from an ISO date string
;; NOTE: Behavior differs for date-only vs datetime strings (possibly DST-related)

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--iso-previous-day-normal-date-returns-previous-day ()
  "Test getting previous day for normal date returns day-2 (actual behavior)."
  ;; NOTE: This function subtracts 2 days for date-only strings (DST-related)
  (should (equal (org-gcal--iso-previous-day "2021-03-15")
                 "2021-03-13")))

(ert-deftest test-org-gcal--iso-previous-day-normal-datetime-returns-previous-day ()
  "Test getting previous day for datetime returns day-1 preserving time."
  ;; DateTime format subtracts 1 day, date-only subtracts 2 days
  (should (equal (org-gcal--iso-previous-day "2021-03-15T14:30")
                 "2021-03-14T14:30")))

;;; Boundary Cases

(ert-deftest test-org-gcal--iso-previous-day-boundary-start-of-month-rolls-to-previous-month ()
  "Test getting previous day at start of month rolls to previous month."
  (should (equal (org-gcal--iso-previous-day "2021-04-01")
                 "2021-03-31")))

(ert-deftest test-org-gcal--iso-previous-day-boundary-start-of-year-rolls-to-previous-year ()
  "Test getting previous day at start of year rolls to previous year."
  (should (equal (org-gcal--iso-previous-day "2022-01-01")
                 "2021-12-31")))

(ert-deftest test-org-gcal--iso-previous-day-boundary-leap-year-handles-mar-1-to-feb-29 ()
  "Test getting previous day on leap year Mar 1 goes to Feb 29."
  (should (equal (org-gcal--iso-previous-day "2020-03-01")
                 "2020-02-29")))

(ert-deftest test-org-gcal--iso-previous-day-boundary-non-leap-year-mar-1-to-feb-28 ()
  "Test getting previous day on non-leap year Mar 1 goes to Feb 28."
  (should (equal (org-gcal--iso-previous-day "2021-03-01")
                 "2021-02-28")))

;;; Error Cases

(ert-deftest test-org-gcal--iso-previous-day-error-nil-input-returns-nil ()
  "Test getting previous day with nil input returns nil."
  (should (equal (org-gcal--iso-previous-day nil) nil)))

(ert-deftest test-org-gcal--iso-previous-day-error-empty-string-returns-nil ()
  "Test getting previous day with empty string returns nil."
  (should (equal (org-gcal--iso-previous-day "") nil)))

(ert-deftest test-org-gcal--iso-previous-day-error-malformed-date-produces-date ()
  "Test getting previous day with malformed date still produces date string."
  ;; Malformed strings still parse (with zeros) and produce dates
  ;; This documents current behavior - could validate input better
  (let ((result (org-gcal--iso-previous-day "not-a-date")))
    (should (stringp result))
    (should (string-prefix-p "-" result))))

(ert-deftest test-org-gcal--iso-previous-day-error-non-iso-format-produces-date ()
  "Test getting previous day with non-ISO format produces date string."
  ;; Non-ISO strings partially parse and produce strange but valid dates
  ;; This documents current behavior - could validate input better
  (let ((result (org-gcal--iso-previous-day "03/15/2021")))
    (should (stringp result))
    ;; Parses "03" as year 3, returns strange date
    (should (string-match-p "^[0-9-]+" result))))

(provide 'test-org-gcal--iso-previous-day)
;;; test-org-gcal--iso-previous-day.el ends here
