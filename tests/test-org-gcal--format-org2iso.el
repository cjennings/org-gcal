;;; test-org-gcal--format-org2iso.el --- Tests for org-gcal--format-org2iso -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--format-org2iso
;; Converts org date/time components to ISO 8601 format

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--format-org2iso-normal-full-datetime-returns-iso ()
  "Test converting org datetime to ISO format returns ISO string."
  (should (equal (org-gcal--format-org2iso 2021 3 15 14 30)
                 "2021-03-15T14:30:00Z")))

(ert-deftest test-org-gcal--format-org2iso-normal-date-only-returns-iso-date ()
  "Test converting org date (no time) to ISO format returns ISO date."
  (should (equal (org-gcal--format-org2iso 2021 3 15)
                 "2021-03-15")))

;;; Boundary Cases

(ert-deftest test-org-gcal--format-org2iso-boundary-midnight-returns-zero-time ()
  "Test converting midnight to ISO format returns 00:00:00."
  (should (equal (org-gcal--format-org2iso 2021 3 15 0 0)
                 "2021-03-15T00:00:00Z")))

(ert-deftest test-org-gcal--format-org2iso-boundary-end-of-day-returns-max-time ()
  "Test converting end of day to ISO format returns 23:59:00."
  (should (equal (org-gcal--format-org2iso 2021 3 15 23 59)
                 "2021-03-15T23:59:00Z")))

(ert-deftest test-org-gcal--format-org2iso-boundary-single-digit-month-day-returns-padded ()
  "Test formatting with single digit month and day returns zero-padded."
  (should (equal (org-gcal--format-org2iso 2021 1 5)
                 "2021-01-05")))

(ert-deftest test-org-gcal--format-org2iso-boundary-leap-year-returns-feb-29 ()
  "Test formatting leap year date returns February 29."
  (should (equal (org-gcal--format-org2iso 2020 2 29)
                 "2020-02-29")))

(ert-deftest test-org-gcal--format-org2iso-boundary-month-13-throws-error ()
  "Test formatting with month > 12 throws user-error."
  (should-error (org-gcal--format-org2iso 2021 13 15)
                :type 'user-error))

(ert-deftest test-org-gcal--format-org2iso-boundary-day-32-throws-error ()
  "Test formatting with day > 31 throws user-error."
  (should-error (org-gcal--format-org2iso 2021 3 32)
                :type 'user-error))

(ert-deftest test-org-gcal--format-org2iso-boundary-hour-24-throws-error ()
  "Test formatting with hour > 23 throws user-error."
  (should-error (org-gcal--format-org2iso 2021 3 15 25 0)
                :type 'user-error))

(ert-deftest test-org-gcal--format-org2iso-boundary-month-0-throws-error ()
  "Test formatting with month < 1 throws user-error."
  (should-error (org-gcal--format-org2iso 2021 0 15)
                :type 'user-error))

(ert-deftest test-org-gcal--format-org2iso-boundary-day-0-throws-error ()
  "Test formatting with day < 1 throws user-error."
  (should-error (org-gcal--format-org2iso 2021 3 0)
                :type 'user-error))

;;; Error Cases

(ert-deftest test-org-gcal--format-org2iso-error-nil-year-throws-error ()
  "Test formatting with nil year throws user-error."
  (should-error (org-gcal--format-org2iso nil 3 15)
                :type 'user-error))

(ert-deftest test-org-gcal--format-org2iso-error-nil-month-throws-error ()
  "Test formatting with nil month throws user-error."
  (should-error (org-gcal--format-org2iso 2021 nil 15)
                :type 'user-error))

(ert-deftest test-org-gcal--format-org2iso-error-nil-day-throws-error ()
  "Test formatting with nil day throws user-error."
  (should-error (org-gcal--format-org2iso 2021 3 nil)
                :type 'user-error))

(provide 'test-org-gcal--format-org2iso)
;;; test-org-gcal--format-org2iso.el ends here
