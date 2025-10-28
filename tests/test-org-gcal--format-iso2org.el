;;; test-org-gcal--format-iso2org.el --- Tests for org-gcal--format-iso2org -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--format-iso2org
;; Converts ISO 8601 datetime strings to org-mode timestamp format

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--format-iso2org-normal-datetime-returns-org-timestamp ()
  "Test converting ISO datetime to org format returns org timestamp."
  (let ((result (org-gcal--format-iso2org "2021-03-15T14:30:00")))
    (should (string-match-p "<2021-03-15 Mon 14:30>" result))))

(ert-deftest test-org-gcal--format-iso2org-normal-date-only-returns-org-date ()
  "Test converting ISO date (no time) to org format returns org date."
  (let ((result (org-gcal--format-iso2org "2021-03-15")))
    (should (string-match-p "<2021-03-15 Mon>" result))))

;;; Boundary Cases

(ert-deftest test-org-gcal--format-iso2org-boundary-midnight-returns-org-timestamp ()
  "Test converting midnight time to org format returns correct timestamp."
  (let ((result (org-gcal--format-iso2org "2021-03-15T00:00:00")))
    (should (string-match-p "<2021-03-15 Mon 00:00>" result))))

(ert-deftest test-org-gcal--format-iso2org-boundary-end-of-day-returns-org-timestamp ()
  "Test converting end of day time to org format returns correct timestamp."
  (let ((result (org-gcal--format-iso2org "2021-03-15T23:59:59")))
    (should (string-match-p "<2021-03-15 Mon 23:59>" result))))

;;; Error Cases

(ert-deftest test-org-gcal--format-iso2org-error-nil-input-returns-nil ()
  "Test converting nil input returns nil."
  (should (equal (org-gcal--format-iso2org nil) nil)))

(ert-deftest test-org-gcal--format-iso2org-error-empty-string-returns-nil ()
  "Test converting empty string returns nil."
  (should (equal (org-gcal--format-iso2org "") nil)))

(ert-deftest test-org-gcal--format-iso2org-error-malformed-date-returns-timestamp ()
  "Test converting malformed date string still returns timestamp."
  ;; Malformed strings still parse (with zeros) and produce timestamps
  ;; This documents current behavior - could be improved to validate
  (let ((result (org-gcal--format-iso2org "not-a-date")))
    (should (stringp result))
    (should (string-prefix-p "<" result))
    (should (string-suffix-p ">" result))))

(ert-deftest test-org-gcal--format-iso2org-error-invalid-date-components-produces-timestamp ()
  "Test converting date with invalid components still produces timestamp."
  ;; String parses, encode-time rolls over: 2021-99-99 becomes valid future date
  ;; This documents current behavior - parse-date could validate better
  (let ((result (org-gcal--format-iso2org "2021-99-99")))
    (should (stringp result))
    (should (string-prefix-p "<" result))
    (should (string-suffix-p ">" result))
    ;; Should roll to a date in 2029 (99 months = 8 years + 3 months, plus 99 days)
    (should (string-match-p "<20[2-3][0-9]" result))))

(provide 'test-org-gcal--format-iso2org)
;;; test-org-gcal--format-iso2org.el ends here
