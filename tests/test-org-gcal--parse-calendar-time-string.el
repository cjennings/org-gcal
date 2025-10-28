;;; test-org-gcal--parse-calendar-time-string.el --- Tests for org-gcal--parse-calendar-time-string -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--parse-calendar-time-string
;; Parses time strings from Google Calendar API to Emacs time values

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--parse-calendar-time-string-normal-full-datetime-returns-time-list ()
  "Test parsing full datetime from calendar API returns Emacs time value."
  (let ((result (org-gcal--parse-calendar-time-string "2021-03-15T14:30:00Z")))
    ;; Result should be an Emacs time value (list of integers)
    (should (listp result))
    (should (cl-every #'integerp result))))

(ert-deftest test-org-gcal--parse-calendar-time-string-normal-date-only-returns-time-list ()
  "Test parsing date-only from calendar API (all-day event) returns time value."
  (let ((result (org-gcal--parse-calendar-time-string "2021-03-15")))
    (should (listp result))
    (should (cl-every #'integerp result))))

;;; Boundary Cases

(ert-deftest test-org-gcal--parse-calendar-time-string-boundary-midnight-returns-time-list ()
  "Test parsing midnight time returns valid time list."
  (let ((result (org-gcal--parse-calendar-time-string "2021-03-15T00:00:00Z")))
    (should (listp result))
    (should (cl-every #'integerp result))))

(ert-deftest test-org-gcal--parse-calendar-time-string-boundary-exactly-11-chars-returns-time-list ()
  "Test boundary where string length is exactly 11 characters returns time list."
  ;; Date-only is 10 chars, datetime is > 11
  (let ((result (org-gcal--parse-calendar-time-string "2021-03-15")))
    (should (listp result))
    (should (cl-every #'integerp result))))

;;; Error Cases

(ert-deftest test-org-gcal--parse-calendar-time-string-error-nil-input-throws-error ()
  "Test parsing nil input throws type error."
  (should-error (org-gcal--parse-calendar-time-string nil)
                :type 'wrong-type-argument))

(ert-deftest test-org-gcal--parse-calendar-time-string-error-empty-string-throws-error ()
  "Test parsing empty string throws type error."
  ;; Empty string has length < 11, goes to encode-time path with nil components
  (should-error (org-gcal--parse-calendar-time-string "")
                :type 'wrong-type-argument))

(ert-deftest test-org-gcal--parse-calendar-time-string-error-malformed-time-throws-error ()
  "Test parsing malformed time string throws error."
  ;; parse-time-string returns nils for unrecognized format, encode-time rejects
  (should-error (org-gcal--parse-calendar-time-string "not-a-time")
                :type 'wrong-type-argument))

(ert-deftest test-org-gcal--parse-calendar-time-string-error-short-malformed-throws-error ()
  "Test parsing short malformed string throws error."
  (should-error (org-gcal--parse-calendar-time-string "abc")
                :type 'wrong-type-argument))

(ert-deftest test-org-gcal--parse-calendar-time-string-boundary-space-format-parses-successfully ()
  "Test parsing datetime with space instead of T parses successfully."
  ;; Format like "2021-03-15 14:30:00" is actually handled by parse-time-string
  (let ((result (org-gcal--parse-calendar-time-string "2021-03-15 14:30:00")))
    (should (listp result))
    (should (cl-every #'integerp result))))

(provide 'test-org-gcal--parse-calendar-time-string)
;;; test-org-gcal--parse-calendar-time-string.el ends here
