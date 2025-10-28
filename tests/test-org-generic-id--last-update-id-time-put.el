;;; test-org-generic-id--last-update-id-time-put.el --- Tests for org-generic-id--last-update-id-time-put -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-generic-id--last-update-id-time-put
;; Stores last update time for an ID property in global plist
;; Converts string ID-PROP to symbol and handles nil plist case

;;; Code:

(require 'org-generic-id)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-generic-id--last-update-id-time-put-normal-new-id-stores-time ()
  "Test storing time for new ID-PROP creates plist entry."
  (let ((org-generic-id--last-update-id-time '()))
    (org-generic-id--last-update-id-time-put "new-id" '(25000 0 0 0))
    (should (equal (plist-get org-generic-id--last-update-id-time 'new-id)
                   '(25000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-normal-existing-id-updates-time ()
  "Test storing time for existing ID-PROP updates the value."
  (let ((org-generic-id--last-update-id-time '(existing-id (25000 0 0 0))))
    (org-generic-id--last-update-id-time-put "existing-id" '(26000 0 0 0))
    (should (equal (plist-get org-generic-id--last-update-id-time 'existing-id)
                   '(26000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-normal-multiple-ids-preserved ()
  "Test storing time for one ID preserves other IDs in plist."
  (let ((org-generic-id--last-update-id-time
         '(id1 (25000 0 0 0) id2 (26000 0 0 0))))
    (org-generic-id--last-update-id-time-put "id3" '(27000 0 0 0))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id1) '(25000 0 0 0)))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id2) '(26000 0 0 0)))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id3) '(27000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-normal-round-trip-preserves-value ()
  "Test storing time and retrieving it returns same value."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "test-id" '(25000 0 0 0))
    (should (equal (org-generic-id--last-update-id-time-get "test-id")
                   '(25000 0 0 0)))))

;;; Boundary Cases

(ert-deftest test-org-generic-id--last-update-id-time-put-boundary-nil-plist-creates-new-plist ()
  "Test storing time when plist is nil creates new plist with entry."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "first-id" '(25000 0 0 0))
    (should (not (null org-generic-id--last-update-id-time)))
    (should (equal (plist-get org-generic-id--last-update-id-time 'first-id)
                   '(25000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-boundary-id-with-special-characters ()
  "Test storing time for ID with special characters works correctly."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "id-with-dashes" '(25000 0 0 0))
    (org-generic-id--last-update-id-time-put "id_with_underscores" '(26000 0 0 0))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id-with-dashes)
                   '(25000 0 0 0)))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id_with_underscores)
                   '(26000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-boundary-large-time-value ()
  "Test storing large time value works correctly."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "id" '(999999 999999 999999 999999))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id)
                   '(999999 999999 999999 999999)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-boundary-zero-time-value ()
  "Test storing zero time value works correctly."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "id" '(0 0 0 0))
    (should (equal (plist-get org-generic-id--last-update-id-time 'id)
                   '(0 0 0 0)))))

;;; Error Cases

(ert-deftest test-org-generic-id--last-update-id-time-put-error-empty-string-id-stores-with-empty-symbol ()
  "Test storing time for empty string ID creates entry with empty symbol."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "" '(25000 0 0 0))
    ;; Empty string becomes symbol with empty name
    (should (equal (plist-get org-generic-id--last-update-id-time (intern ""))
                   '(25000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-put-error-nil-time-stores-nil ()
  "Test storing nil time value stores nil in plist."
  (let ((org-generic-id--last-update-id-time nil))
    (org-generic-id--last-update-id-time-put "id" nil)
    (should (equal (plist-get org-generic-id--last-update-id-time 'id) nil))))

(provide 'test-org-generic-id--last-update-id-time-put)
;;; test-org-generic-id--last-update-id-time-put.el ends here
