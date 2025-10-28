;;; test-org-gcal--format-description-for-drawer.el --- Tests for org-gcal--format-description-for-drawer -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--format-description-for-drawer
;; Formats description for insertion into org drawer

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--format-description-for-drawer-normal-text-returns-text-with-newline ()
  "Test formatting normal text returns text with trailing newline."
  (should (equal (org-gcal--format-description-for-drawer "This is a description")
                 "This is a description\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-normal-leading-asterisk-replaced ()
  "Test formatting text with leading asterisk replaces it with ✱."
  (should (equal (org-gcal--format-description-for-drawer "* Important note")
                 "✱ Important note\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-normal-multiple-leading-asterisks-replaced ()
  "Test formatting text with multiple leading asterisks replaces first."
  (should (equal (org-gcal--format-description-for-drawer "** Very important")
                 "✱* Very important\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-normal-mid-line-asterisks-unchanged ()
  "Test formatting text with mid-line asterisks leaves them unchanged."
  (should (equal (org-gcal--format-description-for-drawer "This * is * important")
                 "This * is * important\n")))

;;; Boundary Cases

(ert-deftest test-org-gcal--format-description-for-drawer-boundary-trailing-newline-preserved ()
  "Test formatting text with trailing newline preserves single newline."
  (should (equal (org-gcal--format-description-for-drawer "Text with newline\n")
                 "Text with newline\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-boundary-multiple-trailing-newlines-preserved ()
  "Test formatting text with multiple trailing newlines preserves them."
  (should (equal (org-gcal--format-description-for-drawer "Text\n\n")
                 "Text\n\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-boundary-single-asterisk-replaced ()
  "Test formatting single asterisk replaces it with ✱."
  (should (equal (org-gcal--format-description-for-drawer "*")
                 "✱\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-boundary-multiline-all-line-start-asterisks-replaced ()
  "Test formatting multiline text replaces asterisks at start of each line."
  (should (equal (org-gcal--format-description-for-drawer "* First\n* Second")
                 "✱ First\n✱ Second\n")))

(ert-deftest test-org-gcal--format-description-for-drawer-boundary-whitespace-only-returns-with-newline ()
  "Test formatting whitespace-only text returns it with newline."
  (should (equal (org-gcal--format-description-for-drawer "   ")
                 "   \n")))

(ert-deftest test-org-gcal--format-description-for-drawer-boundary-newline-only-preserved ()
  "Test formatting newline-only returns single newline."
  (should (equal (org-gcal--format-description-for-drawer "\n")
                 "\n")))

;;; Error Cases

(ert-deftest test-org-gcal--format-description-for-drawer-error-nil-input-returns-nil ()
  "Test formatting nil input returns nil."
  (should (equal (org-gcal--format-description-for-drawer nil) nil)))

(ert-deftest test-org-gcal--format-description-for-drawer-error-empty-string-returns-newline ()
  "Test formatting empty string returns just newline."
  (should (equal (org-gcal--format-description-for-drawer "")
                 "\n")))

(provide 'test-org-gcal--format-description-for-drawer)
;;; test-org-gcal--format-description-for-drawer.el ends here
