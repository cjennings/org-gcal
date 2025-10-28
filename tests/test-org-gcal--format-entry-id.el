;;; test-org-gcal--format-entry-id.el --- Tests for org-gcal--format-entry-id -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--format-entry-id
;; Formats calendar ID and event ID into combined entry ID

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--format-entry-id-normal-valid-inputs-returns-formatted-string ()
  "Test formatting valid calendar and event IDs returns formatted string."
  (should (equal (org-gcal--format-entry-id "calendar@example.com" "event123")
                 "event123/calendar@example.com")))

(ert-deftest test-org-gcal--format-entry-id-normal-complex-ids-returns-formatted-string ()
  "Test formatting complex IDs returns correctly formatted string."
  (should (equal (org-gcal--format-entry-id "user+tag@gmail.com" "evt_abc-123_xyz")
                 "evt_abc-123_xyz/user+tag@gmail.com")))

;;; Boundary Cases

(ert-deftest test-org-gcal--format-entry-id-boundary-single-char-ids-returns-formatted-string ()
  "Test formatting single character IDs returns formatted string."
  (should (equal (org-gcal--format-entry-id "a" "b")
                 "b/a")))

(ert-deftest test-org-gcal--format-entry-id-boundary-long-ids-returns-formatted-string ()
  "Test formatting very long IDs returns formatted string."
  (let ((long-calendar (make-string 100 ?x))
        (long-event (make-string 100 ?y)))
    (should (equal (org-gcal--format-entry-id long-calendar long-event)
                   (format "%s/%s" long-event long-calendar)))))

;;; Error Cases

(ert-deftest test-org-gcal--format-entry-id-error-nil-calendar-returns-nil ()
  "Test formatting with nil calendar ID returns nil."
  (should (equal (org-gcal--format-entry-id nil "event123")
                 nil)))

(ert-deftest test-org-gcal--format-entry-id-error-nil-event-returns-nil ()
  "Test formatting with nil event ID returns nil."
  (should (equal (org-gcal--format-entry-id "calendar@example.com" nil)
                 nil)))

(ert-deftest test-org-gcal--format-entry-id-error-both-nil-returns-nil ()
  "Test formatting with both IDs nil returns nil."
  (should (equal (org-gcal--format-entry-id nil nil)
                 nil)))

(ert-deftest test-org-gcal--format-entry-id-error-empty-strings-returns-formatted-string ()
  "Test formatting with empty strings returns formatted string with slash."
  (should (equal (org-gcal--format-entry-id "" "")
                 "/")))

(provide 'test-org-gcal--format-entry-id)
;;; test-org-gcal--format-entry-id.el ends here
