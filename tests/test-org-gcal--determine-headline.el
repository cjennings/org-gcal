;;; test-org-gcal--determine-headline.el --- Tests for org-gcal--determine-headline -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--determine-headline
;; Determines what headline to use for event with given summary

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--determine-headline-normal-summary-provided-returns-summary ()
  "Test with summary provided returns the summary."
  (should (equal (org-gcal--determine-headline "Team Meeting" "Old Headline" "CANCELLED")
                 "Team Meeting")))

(ert-deftest test-org-gcal--determine-headline-normal-no-summary-returns-existing ()
  "Test with no summary returns existing headline."
  (should (equal (org-gcal--determine-headline nil "Existing Headline" "CANCELLED")
                 "Existing Headline")))

(ert-deftest test-org-gcal--determine-headline-normal-summary-empty-returns-empty ()
  "Test with empty summary returns empty string (actual behavior)."
  (should (equal (org-gcal--determine-headline "" "Existing Headline" "CANCELLED")
                 "")))

;;; Boundary Cases

(ert-deftest test-org-gcal--determine-headline-boundary-summary-equals-cancelled-returns-existing ()
  "Test when summary equals cancelled keyword returns existing headline."
  (should (equal (org-gcal--determine-headline "CANCELLED" "Original Event" "CANCELLED")
                 "Original Event")))

(ert-deftest test-org-gcal--determine-headline-boundary-no-summary-no-existing-returns-busy ()
  "Test with no summary and no existing headline returns 'busy'."
  (should (equal (org-gcal--determine-headline nil nil "CANCELLED")
                 "busy")))

(ert-deftest test-org-gcal--determine-headline-boundary-no-summary-empty-existing-returns-busy ()
  "Test with no summary and empty existing headline returns 'busy'."
  (should (equal (org-gcal--determine-headline nil "" "CANCELLED")
                 "busy")))

(ert-deftest test-org-gcal--determine-headline-boundary-empty-summary-nil-existing-returns-empty ()
  "Test with empty summary and nil existing headline returns empty string."
  (should (equal (org-gcal--determine-headline "" nil "CANCELLED")
                 "")))

(ert-deftest test-org-gcal--determine-headline-boundary-empty-summary-empty-existing-returns-empty ()
  "Test with empty summary and empty existing headline returns empty string."
  (should (equal (org-gcal--determine-headline "" "" "CANCELLED")
                 "")))

(ert-deftest test-org-gcal--determine-headline-boundary-summary-whitespace-only-returns-whitespace ()
  "Test with whitespace-only summary returns the whitespace."
  (should (equal (org-gcal--determine-headline "   " "Old Headline" "CANCELLED")
                 "   ")))

(ert-deftest test-org-gcal--determine-headline-boundary-cancelled-keyword-nil-allows-cancelled-summary ()
  "Test with nil cancelled keyword allows CANCELLED as valid summary."
  (should (equal (org-gcal--determine-headline "CANCELLED" "Old Headline" nil)
                 "CANCELLED")))

;;; Error Cases

(ert-deftest test-org-gcal--determine-headline-error-all-nil-returns-nil ()
  "Test with all nil arguments returns nil (summary equals cancelled-keyword)."
  (should (equal (org-gcal--determine-headline nil nil nil)
                 nil)))

(ert-deftest test-org-gcal--determine-headline-error-summary-nil-existing-whitespace-returns-whitespace ()
  "Test with nil summary and whitespace existing returns whitespace."
  (should (equal (org-gcal--determine-headline nil "  " "CANCELLED")
                 "  ")))

(provide 'test-org-gcal--determine-headline)
;;; test-org-gcal--determine-headline.el ends here
