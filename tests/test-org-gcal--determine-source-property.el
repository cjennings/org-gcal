;;; test-org-gcal--determine-source-property.el --- Tests for org-gcal--determine-source-property -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--determine-source-property
;; Determines which property to use for event source link

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--determine-source-property-normal-no-link-no-roam-no-title-returns-roam-refs ()
  "Test with no existing link/roam and no title returns ROAM_REFS."
  (let* ((source '(:url "https://example.com" :title nil))
         (result (org-gcal--determine-source-property source nil nil)))
    (should (equal (plist-get result :property-name) "ROAM_REFS"))
    (should (equal (plist-get result :property-value) "https://example.com"))))

(ert-deftest test-org-gcal--determine-source-property-normal-with-title-returns-link ()
  "Test with title present returns link property."
  (let* ((source '(:url "https://example.com" :title "Example Site"))
         (result (org-gcal--determine-source-property source nil nil)))
    (should (equal (plist-get result :property-name) "link"))
    (should (string-match-p "https://example.com" (plist-get result :property-value)))
    (should (string-match-p "Example Site" (plist-get result :property-value)))))

(ert-deftest test-org-gcal--determine-source-property-normal-existing-link-returns-link ()
  "Test with existing link property returns link."
  (let* ((source '(:url "https://example.com" :title nil))
         (result (org-gcal--determine-source-property source nil "https://old.com")))
    (should (equal (plist-get result :property-name) "link"))))

(ert-deftest test-org-gcal--determine-source-property-normal-multiple-roam-refs-returns-link ()
  "Test with multiple existing ROAM_REFS returns link."
  (let* ((source '(:url "https://example.com" :title nil))
         (result (org-gcal--determine-source-property source '("ref1" "ref2") nil)))
    (should (equal (plist-get result :property-name) "link"))))

;;; Boundary Cases

(ert-deftest test-org-gcal--determine-source-property-boundary-one-roam-ref-no-link-returns-roam-refs ()
  "Test with one existing ROAM_REF and no link returns ROAM_REFS."
  (let* ((source '(:url "https://example.com" :title nil))
         (result (org-gcal--determine-source-property source '("ref1") nil)))
    (should (equal (plist-get result :property-name) "ROAM_REFS"))
    (should (equal (plist-get result :property-value) "https://example.com"))))

(ert-deftest test-org-gcal--determine-source-property-boundary-empty-roam-refs-list-returns-roam-refs ()
  "Test with empty ROAM_REFS list returns ROAM_REFS."
  (let* ((source '(:url "https://example.com" :title nil))
         (result (org-gcal--determine-source-property source '() nil)))
    (should (equal (plist-get result :property-name) "ROAM_REFS"))
    (should (equal (plist-get result :property-value) "https://example.com"))))

(ert-deftest test-org-gcal--determine-source-property-boundary-empty-title-string-returns-roam-refs ()
  "Test with empty title string returns ROAM_REFS."
  (let* ((source '(:url "https://example.com" :title ""))
         (result (org-gcal--determine-source-property source nil nil)))
    (should (equal (plist-get result :property-name) "ROAM_REFS"))
    (should (equal (plist-get result :property-value) "https://example.com"))))

(ert-deftest test-org-gcal--determine-source-property-boundary-whitespace-title-returns-link ()
  "Test with whitespace-only title returns link property."
  (let* ((source '(:url "https://example.com" :title "   "))
         (result (org-gcal--determine-source-property source nil nil)))
    (should (equal (plist-get result :property-name) "link"))))

(ert-deftest test-org-gcal--determine-source-property-boundary-two-roam-refs-returns-link ()
  "Test with exactly two ROAM_REFS returns link."
  (let* ((source '(:url "https://example.com" :title nil))
         (result (org-gcal--determine-source-property source '("ref1" "ref2") nil)))
    (should (equal (plist-get result :property-name) "link"))))

;;; Error Cases

(ert-deftest test-org-gcal--determine-source-property-error-nil-source-returns-nil ()
  "Test with nil source returns nil."
  (should (equal (org-gcal--determine-source-property nil nil nil) nil)))

(ert-deftest test-org-gcal--determine-source-property-error-source-missing-url-returns-link-with-nil ()
  "Test with source missing url but with title returns link property."
  (let* ((source '(:title "Example"))
         (result (org-gcal--determine-source-property source nil nil)))
    (should (plistp result))
    (should (equal (plist-get result :property-name) "link"))
    ;; org-link-make-string with nil url and title creates a link
    (should (stringp (plist-get result :property-value)))))

(ert-deftest test-org-gcal--determine-source-property-error-empty-source-plist-returns-nil ()
  "Test with empty source plist returns nil (empty list is nil)."
  (let* ((source '())
         (result (org-gcal--determine-source-property source nil nil)))
    (should (equal result nil))))

(provide 'test-org-gcal--determine-source-property)
;;; test-org-gcal--determine-source-property.el ends here
