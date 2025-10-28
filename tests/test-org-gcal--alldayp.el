;;; test-org-gcal--alldayp.el --- Tests for org-gcal--alldayp -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--alldayp
;; Determines if an event is an all-day event based on start/end times

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--alldayp-normal-true-single-day-returns-true ()
  "Test alldayp returns true for single full day event."
  (should (org-gcal--alldayp "2021-03-15" "2021-03-16")))

(ert-deftest test-org-gcal--alldayp-normal-false-with-times-returns-false ()
  "Test alldayp returns false when times are present."
  (should-not (org-gcal--alldayp "2021-03-15T10:00:00" "2021-03-15T11:00:00")))

(ert-deftest test-org-gcal--alldayp-normal-false-multiple-days-returns-false ()
  "Test alldayp returns false for multiple day event."
  (should-not (org-gcal--alldayp "2021-03-15" "2021-03-17")))

;;; Boundary Cases

(ert-deftest test-org-gcal--alldayp-boundary-month-boundary-returns-true ()
  "Test alldayp across month boundary returns true for single day."
  (should (org-gcal--alldayp "2021-03-31" "2021-04-01")))

(ert-deftest test-org-gcal--alldayp-boundary-year-boundary-returns-true ()
  "Test alldayp across year boundary returns true for single day."
  (should (org-gcal--alldayp "2021-12-31" "2022-01-01")))

(ert-deftest test-org-gcal--alldayp-boundary-leap-year-returns-true ()
  "Test alldayp on leap year boundary returns true for single day."
  (should (org-gcal--alldayp "2020-02-29" "2020-03-01")))

;;; Error Cases

(ert-deftest test-org-gcal--alldayp-error-empty-strings-returns-false ()
  "Test alldayp with empty strings returns false."
  (should-not (org-gcal--alldayp "" "")))

(ert-deftest test-org-gcal--alldayp-error-malformed-dates-returns-false ()
  "Test alldayp with malformed date strings returns false."
  (should-not (org-gcal--alldayp "not-a-date" "also-not-a-date")))

(ert-deftest test-org-gcal--alldayp-error-reversed-dates-returns-false ()
  "Test alldayp with reversed date order (end before start) returns false."
  (should-not (org-gcal--alldayp "2021-03-16" "2021-03-15")))

(provide 'test-org-gcal--alldayp)
;;; test-org-gcal--alldayp.el ends here
