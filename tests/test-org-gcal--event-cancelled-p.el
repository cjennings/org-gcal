;;; test-org-gcal--event-cancelled-p.el --- Tests for org-gcal--event-cancelled-p -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--event-cancelled-p
;; Checks if a calendar event has been cancelled

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--event-cancelled-p-normal-cancelled-event-returns-true ()
  "Test checking cancelled event returns true."
  (should (org-gcal--event-cancelled-p '(:status "cancelled"))))

(ert-deftest test-org-gcal--event-cancelled-p-normal-confirmed-event-returns-false ()
  "Test checking confirmed event returns false."
  (should-not (org-gcal--event-cancelled-p '(:status "confirmed"))))

(ert-deftest test-org-gcal--event-cancelled-p-normal-tentative-event-returns-false ()
  "Test checking tentative event returns false."
  (should-not (org-gcal--event-cancelled-p '(:status "tentative"))))

;;; Boundary Cases

(ert-deftest test-org-gcal--event-cancelled-p-boundary-empty-plist-returns-false ()
  "Test checking event with empty plist returns false."
  (should-not (org-gcal--event-cancelled-p '())))

(ert-deftest test-org-gcal--event-cancelled-p-boundary-nil-status-returns-false ()
  "Test checking event with nil status returns false."
  (should-not (org-gcal--event-cancelled-p '(:status nil))))

;;; Error Cases

(ert-deftest test-org-gcal--event-cancelled-p-error-nil-event-returns-false ()
  "Test checking nil event returns false."
  (should-not (org-gcal--event-cancelled-p nil)))

(ert-deftest test-org-gcal--event-cancelled-p-error-missing-status-returns-false ()
  "Test checking event without status key returns false."
  (should-not (org-gcal--event-cancelled-p '(:other-key "value"))))

(ert-deftest test-org-gcal--event-cancelled-p-error-empty-string-status-returns-false ()
  "Test checking event with empty status string returns false."
  (should-not (org-gcal--event-cancelled-p '(:status ""))))

(ert-deftest test-org-gcal--event-cancelled-p-error-wrong-case-cancelled-returns-false ()
  "Test checking event with wrong case 'Cancelled' returns false."
  (should-not (org-gcal--event-cancelled-p '(:status "Cancelled"))))

(ert-deftest test-org-gcal--event-cancelled-p-error-partial-match-returns-false ()
  "Test checking event with partial match 'cancelled-extra' returns false."
  (should-not (org-gcal--event-cancelled-p '(:status "cancelled-extra"))))

(provide 'test-org-gcal--event-cancelled-p)
;;; test-org-gcal--event-cancelled-p.el ends here
