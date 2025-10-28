;;; test-org-gcal--event-id-from-entry-id.el --- Tests for org-gcal--event-id-from-entry-id -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--event-id-from-entry-id
;; Parses combined entry IDs (event-id/calendar-id) to extract event ID

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--event-id-from-entry-id-normal-valid-entry-returns-event-id ()
  "Test parsing valid entry ID returns event ID portion."
  (should (equal (org-gcal--event-id-from-entry-id "event123/calendar@example.com")
                 "event123")))

(ert-deftest test-org-gcal--event-id-from-entry-id-normal-complex-ids-returns-event-id ()
  "Test parsing entry ID with complex characters returns correct event ID."
  (should (equal (org-gcal--event-id-from-entry-id "foobar1234_xyz/foo@bar.com")
                 "foobar1234_xyz")))

(ert-deftest test-org-gcal--event-id-from-entry-id-normal-uuid-style-returns-event-id ()
  "Test parsing UUID-style event ID returns correct value."
  (should (equal (org-gcal--event-id-from-entry-id "a1b2c3d4-e5f6-7890/user@example.org")
                 "a1b2c3d4-e5f6-7890")))

;;; Boundary Cases

(ert-deftest test-org-gcal--event-id-from-entry-id-boundary-single-char-event-id-returns-char ()
  "Test parsing entry ID with single character event ID returns that char."
  (should (equal (org-gcal--event-id-from-entry-id "a/calendar@example.com")
                 "a")))

(ert-deftest test-org-gcal--event-id-from-entry-id-boundary-long-ids-returns-event-id ()
  "Test parsing very long IDs returns correct event ID."
  (let ((long-event-id (make-string 100 ?x))
        (calendar-id "cal@example.com"))
    (should (equal (org-gcal--event-id-from-entry-id
                    (format "%s/%s" long-event-id calendar-id))
                   long-event-id))))

;;; Error Cases

(ert-deftest test-org-gcal--event-id-from-entry-id-error-nil-returns-nil ()
  "Test parsing nil entry ID returns nil."
  (should (equal (org-gcal--event-id-from-entry-id nil)
                 nil)))

(ert-deftest test-org-gcal--event-id-from-entry-id-error-empty-string-returns-nil ()
  "Test parsing empty string returns nil."
  (should (equal (org-gcal--event-id-from-entry-id "")
                 nil)))

(ert-deftest test-org-gcal--event-id-from-entry-id-error-no-slash-returns-nil ()
  "Test parsing entry ID without slash separator returns nil."
  (should (equal (org-gcal--event-id-from-entry-id "event123calendar")
                 nil)))

(ert-deftest test-org-gcal--event-id-from-entry-id-error-multiple-slashes-returns-first-part ()
  "Test parsing entry ID with multiple slashes returns first part."
  (should (equal (org-gcal--event-id-from-entry-id "event/calendar/extra")
                 nil)))

(ert-deftest test-org-gcal--event-id-from-entry-id-error-only-slash-returns-nil ()
  "Test parsing entry ID that is only a slash returns nil."
  (should (equal (org-gcal--event-id-from-entry-id "/")
                 nil)))

(ert-deftest test-org-gcal--event-id-from-entry-id-error-slash-at-end-returns-nil ()
  "Test parsing entry ID ending with slash returns nil."
  (should (equal (org-gcal--event-id-from-entry-id "event123/")
                 nil)))

(ert-deftest test-org-gcal--event-id-from-entry-id-error-slash-at-start-returns-nil ()
  "Test parsing entry ID starting with slash returns nil."
  (should (equal (org-gcal--event-id-from-entry-id "/calendar@example.com")
                 nil)))

(provide 'test-org-gcal--event-id-from-entry-id)
;;; test-org-gcal--event-id-from-entry-id.el ends here
