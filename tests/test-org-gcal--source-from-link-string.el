;;; test-org-gcal--source-from-link-string.el --- Tests for org-gcal--source-from-link-string -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--source-from-link-string
;; Parses org-mode link strings into source alists for Google Calendar API

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--source-from-link-string-normal-link-with-title-returns-alist ()
  "Test parsing link with title returns alist with url and title."
  (let ((result (org-gcal--source-from-link-string "[[https://example.com][Example Title]]")))
    (should (equal (alist-get 'url result) "https://example.com"))
    (should (equal (alist-get 'title result) "Example Title"))))

(ert-deftest test-org-gcal--source-from-link-string-normal-link-without-title-returns-alist-with-url ()
  "Test parsing link without title returns alist with only url."
  (let ((result (org-gcal--source-from-link-string "[[https://example.com]]")))
    (should (equal (alist-get 'url result) "https://example.com"))
    (should (equal (alist-get 'title result) nil))))

(ert-deftest test-org-gcal--source-from-link-string-normal-http-link-returns-alist ()
  "Test parsing HTTP (non-HTTPS) link returns alist."
  (let ((result (org-gcal--source-from-link-string "[[http://example.com][Title]]")))
    (should (equal (alist-get 'url result) "http://example.com"))
    (should (equal (alist-get 'title result) "Title"))))

;;; Boundary Cases

(ert-deftest test-org-gcal--source-from-link-string-boundary-empty-title-returns-nil ()
  "Test parsing link with empty title returns nil (no valid link found)."
  (let ((result (org-gcal--source-from-link-string "[[https://example.com][]]")))
    (should (equal result nil))))

(ert-deftest test-org-gcal--source-from-link-string-boundary-title-with-special-chars-returns-correct-title ()
  "Test parsing link with special characters in title preserves them."
  (let ((result (org-gcal--source-from-link-string "[[https://example.com][Title & <Special> \"Chars\"]]")))
    (should (equal (alist-get 'url result) "https://example.com"))
    (should (equal (alist-get 'title result) "Title & <Special> \"Chars\""))))

(ert-deftest test-org-gcal--source-from-link-string-boundary-long-url-returns-full-url ()
  "Test parsing link with very long URL returns complete URL."
  (let* ((long-url (concat "https://example.com/" (make-string 200 ?x)))
         (link-string (format "[[%s][Title]]" long-url))
         (result (org-gcal--source-from-link-string link-string)))
    (should (equal (alist-get 'url result) long-url))))

;;; Error Cases

(ert-deftest test-org-gcal--source-from-link-string-error-nil-throws-error ()
  "Test parsing nil throws error."
  (should-error (org-gcal--source-from-link-string nil)))

(ert-deftest test-org-gcal--source-from-link-string-error-empty-string-returns-nil ()
  "Test parsing empty string returns nil."
  (should (equal (org-gcal--source-from-link-string "")
                 nil)))

(ert-deftest test-org-gcal--source-from-link-string-error-plain-text-returns-nil ()
  "Test parsing plain text (not a link) returns nil."
  (should (equal (org-gcal--source-from-link-string "just plain text")
                 nil)))

(ert-deftest test-org-gcal--source-from-link-string-error-malformed-link-returns-nil ()
  "Test parsing malformed link returns nil."
  (should (equal (org-gcal--source-from-link-string "[https://example.com]")
                 nil)))

(ert-deftest test-org-gcal--source-from-link-string-error-incomplete-link-returns-nil ()
  "Test parsing incomplete link structure returns nil."
  (should (equal (org-gcal--source-from-link-string "[[https://example.com")
                 nil)))

(provide 'test-org-gcal--source-from-link-string)
;;; test-org-gcal--source-from-link-string.el ends here
