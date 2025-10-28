;;; test-org-generic-id--last-update-id-time-get.el --- Tests for org-generic-id--last-update-id-time-get -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-generic-id--last-update-id-time-get
;; Retrieves last update time for an ID property from global plist
;; Converts string ID-PROP to symbol for plist-get

;;; Code:

(require 'org-generic-id)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-generic-id--last-update-id-time-get-normal-existing-id-returns-time ()
  "Test getting time for existing ID-PROP returns stored time value."
  (let ((org-generic-id--last-update-id-time '(test-id (25000 0 0 0))))
    (let ((result (org-generic-id--last-update-id-time-get "test-id")))
      (should (equal result '(25000 0 0 0))))))

(ert-deftest test-org-generic-id--last-update-id-time-get-normal-multiple-ids-returns-correct-time ()
  "Test getting time from plist with multiple IDs returns correct value."
  (let ((org-generic-id--last-update-id-time
         '(id1 (25000 0 0 0) id2 (26000 0 0 0) id3 (27000 0 0 0))))
    (should (equal (org-generic-id--last-update-id-time-get "id1") '(25000 0 0 0)))
    (should (equal (org-generic-id--last-update-id-time-get "id2") '(26000 0 0 0)))
    (should (equal (org-generic-id--last-update-id-time-get "id3") '(27000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-get-normal-string-id-converted-to-symbol ()
  "Test that string ID-PROP is correctly converted to symbol for plist lookup."
  (let ((org-generic-id--last-update-id-time '(my-id (25000 0 0 0))))
    ;; Verify we can retrieve using string
    (should (equal (org-generic-id--last-update-id-time-get "my-id") '(25000 0 0 0)))))

;;; Boundary Cases

(ert-deftest test-org-generic-id--last-update-id-time-get-boundary-empty-plist-returns-nil ()
  "Test getting time from empty plist returns nil."
  (let ((org-generic-id--last-update-id-time '()))
    (should (equal (org-generic-id--last-update-id-time-get "any-id") nil))))

(ert-deftest test-org-generic-id--last-update-id-time-get-boundary-single-entry-plist-returns-value ()
  "Test getting time from plist with single entry returns that value."
  (let ((org-generic-id--last-update-id-time '(only-id (25000 0 0 0))))
    (should (equal (org-generic-id--last-update-id-time-get "only-id") '(25000 0 0 0)))))

(ert-deftest test-org-generic-id--last-update-id-time-get-boundary-id-with-special-characters ()
  "Test getting time for ID with special characters works correctly."
  (let ((org-generic-id--last-update-id-time
         '(id-with-dashes (25000 0 0 0) id_with_underscores (26000 0 0 0))))
    (should (equal (org-generic-id--last-update-id-time-get "id-with-dashes")
                   '(25000 0 0 0)))
    (should (equal (org-generic-id--last-update-id-time-get "id_with_underscores")
                   '(26000 0 0 0)))))

;;; Error Cases

(ert-deftest test-org-generic-id--last-update-id-time-get-error-nil-plist-returns-nil ()
  "Test getting time when plist is nil returns nil."
  (let ((org-generic-id--last-update-id-time nil))
    (should (equal (org-generic-id--last-update-id-time-get "any-id") nil))))

(ert-deftest test-org-generic-id--last-update-id-time-get-error-nonexistent-id-returns-nil ()
  "Test getting time for ID that doesn't exist in plist returns nil."
  (let ((org-generic-id--last-update-id-time '(existing-id (25000 0 0 0))))
    (should (equal (org-generic-id--last-update-id-time-get "nonexistent-id") nil))))

(ert-deftest test-org-generic-id--last-update-id-time-get-error-empty-string-id-returns-nil ()
  "Test getting time for empty string ID returns nil."
  (let ((org-generic-id--last-update-id-time '(some-id (25000 0 0 0))))
    (should (equal (org-generic-id--last-update-id-time-get "") nil))))

(provide 'test-org-generic-id--last-update-id-time-get)
;;; test-org-generic-id--last-update-id-time-get.el ends here
