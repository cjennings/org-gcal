;;; test-org-gcal--parse-date.el --- Tests for org-gcal--parse-date -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--parse-date
;; Parses ISO 8601 date strings into plist with year/mon/day/hour/min/sec

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--parse-date-normal-full-datetime-returns-plist ()
  "Test parsing full ISO 8601 datetime string returns complete plist."
  (let ((result (org-gcal--parse-date "2021-03-15T14:30:45")))
    (should (equal (plist-get result :year) 2021))
    (should (equal (plist-get result :mon) 3))
    (should (equal (plist-get result :day) 15))
    (should (equal (plist-get result :hour) 14))
    (should (equal (plist-get result :min) 30))
    (should (equal (plist-get result :sec) 45))))

(ert-deftest test-org-gcal--parse-date-normal-date-only-returns-zero-time ()
  "Test parsing date-only string returns zero for time fields."
  (let ((result (org-gcal--parse-date "2021-12-25")))
    (should (equal (plist-get result :year) 2021))
    (should (equal (plist-get result :mon) 12))
    (should (equal (plist-get result :day) 25))
    (should (equal (plist-get result :hour) 0))
    (should (equal (plist-get result :min) 0))
    (should (equal (plist-get result :sec) 0))))

;;; Boundary Cases

(ert-deftest test-org-gcal--parse-date-boundary-midnight-returns-zeros ()
  "Test parsing datetime at midnight returns zero time values."
  (let ((result (org-gcal--parse-date "2021-01-01T00:00:00")))
    (should (equal (plist-get result :year) 2021))
    (should (equal (plist-get result :mon) 1))
    (should (equal (plist-get result :day) 1))
    (should (equal (plist-get result :hour) 0))
    (should (equal (plist-get result :min) 0))
    (should (equal (plist-get result :sec) 0))))

(ert-deftest test-org-gcal--parse-date-boundary-end-of-day-returns-max-time ()
  "Test parsing datetime at end of day returns 23:59:59."
  (let ((result (org-gcal--parse-date "2021-12-31T23:59:59")))
    (should (equal (plist-get result :year) 2021))
    (should (equal (plist-get result :mon) 12))
    (should (equal (plist-get result :day) 31))
    (should (equal (plist-get result :hour) 23))
    (should (equal (plist-get result :min) 59))
    (should (equal (plist-get result :sec) 59))))

(ert-deftest test-org-gcal--parse-date-boundary-leap-year-returns-feb-29 ()
  "Test parsing leap year date returns February 29."
  (let ((result (org-gcal--parse-date "2020-02-29T12:00:00")))
    (should (equal (plist-get result :year) 2020))
    (should (equal (plist-get result :mon) 2))
    (should (equal (plist-get result :day) 29))))

(ert-deftest test-org-gcal--parse-date-boundary-year-2000-returns-correct-date ()
  "Test parsing year 2000 (Y2K boundary) returns correct values."
  (let ((result (org-gcal--parse-date "2000-01-01T00:00:00")))
    (should (equal (plist-get result :year) 2000))
    (should (equal (plist-get result :mon) 1))
    (should (equal (plist-get result :day) 1))))

(ert-deftest test-org-gcal--parse-date-boundary-far-future-returns-correct-date ()
  "Test parsing far future date returns correct values."
  (let ((result (org-gcal--parse-date "2099-12-31T23:59:59")))
    (should (equal (plist-get result :year) 2099))
    (should (equal (plist-get result :mon) 12))
    (should (equal (plist-get result :day) 31))))

;;; Error Cases

(ert-deftest test-org-gcal--parse-date-error-nil-input-returns-zeros ()
  "Test parsing nil input returns zeros for all components."
  ;; nil treated as empty by safe-substring, string-to-number returns 0
  (let ((result (org-gcal--parse-date nil)))
    (should (equal (plist-get result :year) 0))
    (should (equal (plist-get result :mon) 0))
    (should (equal (plist-get result :day) 0))
    (should (equal (plist-get result :hour) 0))
    (should (equal (plist-get result :min) 0))
    (should (equal (plist-get result :sec) 0))))

(ert-deftest test-org-gcal--parse-date-error-empty-string-returns-zeros ()
  "Test parsing empty string returns zeros due to safe-substring."
  (let ((result (org-gcal--parse-date "")))
    (should (equal (plist-get result :year) 0))
    (should (equal (plist-get result :mon) 0))
    (should (equal (plist-get result :day) 0))))

(ert-deftest test-org-gcal--parse-date-error-short-string-returns-partial-zeros ()
  "Test parsing string too short returns partial values."
  (let ((result (org-gcal--parse-date "2021")))
    (should (equal (plist-get result :year) 2021))
    (should (equal (plist-get result :mon) 0))
    (should (equal (plist-get result :day) 0))))

(ert-deftest test-org-gcal--parse-date-error-malformed-date-returns-zeros ()
  "Test parsing malformed date string returns zeros."
  (let ((result (org-gcal--parse-date "abcd-ef-ghTij:kl:mn")))
    ;; string-to-number returns 0 for non-numeric strings
    (should (equal (plist-get result :year) 0))
    (should (equal (plist-get result :mon) 0))
    (should (equal (plist-get result :day) 0))))

(provide 'test-org-gcal--parse-date)
;;; test-org-gcal--parse-date.el ends here
