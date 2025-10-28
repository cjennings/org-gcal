;;; test-org-gcal--convert-time-to-local-timezone.el --- Tests for org-gcal--convert-time-to-local-timezone -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--convert-time-to-local-timezone
;; Converts datetime strings to specified local timezone

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--convert-time-to-local-timezone-normal-utc-returns-formatted ()
  "Test converting UTC time to local timezone returns formatted string."
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-00:00" "")
           "2021-03-03T11:30:00+0000")))

(ert-deftest test-org-gcal--convert-time-to-local-timezone-normal-offset-adjusts-time ()
  "Test converting time with timezone offset adjusts time correctly."
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "")
           "2021-03-03T19:30:00+0000")))

(ert-deftest test-org-gcal--convert-time-to-local-timezone-normal-named-timezone-converts ()
  "Test converting to named timezone converts correctly."
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "Europe/London")
           "2021-03-03T19:30:00+0000")))

;;; Boundary Cases

(ert-deftest test-org-gcal--convert-time-to-local-timezone-boundary-nil-time-returns-nil ()
  "Test with nil time returns nil."
  (should (equal
           (org-gcal--convert-time-to-local-timezone nil "Europe/London")
           nil)))

(ert-deftest test-org-gcal--convert-time-to-local-timezone-boundary-nil-timezone-returns-original ()
  "Test with nil timezone returns original time string."
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-00:00" nil)
           "2021-03-03T11:30:00-00:00")))

;;; Error Cases

(ert-deftest test-org-gcal--convert-time-to-local-timezone-error-empty-string-returns-empty ()
  "Test with empty string time returns empty string."
  (should (equal
           (org-gcal--convert-time-to-local-timezone "" nil)
           "")))

(ert-deftest test-org-gcal--convert-time-to-local-timezone-error-malformed-time-throws-error ()
  "Test with malformed time string throws error."
  (should-error
   (org-gcal--convert-time-to-local-timezone "not-a-time" "Europe/London")))

(provide 'test-org-gcal--convert-time-to-local-timezone)
;;; test-org-gcal--convert-time-to-local-timezone.el ends here
