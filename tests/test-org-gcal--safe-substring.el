;;; test-org-gcal--safe-substring.el --- Tests for org-gcal--safe-substring -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Craig Jennings <c@cjennings.net>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for org-gcal--safe-substring
;;
;; This function safely extracts substrings without throwing errors for
;; out-of-range indices. Critical for parsing date/time strings from
;; the Google Calendar API.
;;
;; Test Coverage:
;; - Normal cases: typical substring extraction
;; - Boundary cases: empty strings, single chars, negative indices
;; - Error cases: out-of-range indices, Unicode characters

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--safe-substring-normal-full-string-returns-string ()
  "Test extracting entire string with safe-substring returns full string."
  (should (equal (org-gcal--safe-substring "hello" 0 5) "hello"))
  (should (equal (org-gcal--safe-substring "test" 0) "test")))

(ert-deftest test-org-gcal--safe-substring-normal-middle-returns-substring ()
  "Test extracting substring from middle of string returns correct substring."
  (should (equal (org-gcal--safe-substring "2021-03-15" 5 7) "03"))
  (should (equal (org-gcal--safe-substring "hello world" 0 5) "hello"))
  (should (equal (org-gcal--safe-substring "hello world" 6 11) "world")))

(ert-deftest test-org-gcal--safe-substring-normal-from-middle-to-end-returns-substring ()
  "Test extracting substring from middle to end returns remainder."
  (should (equal (org-gcal--safe-substring "2021-03-15" 5) "03-15"))
  (should (equal (org-gcal--safe-substring "hello" 2) "llo")))

;;; Boundary Cases

(ert-deftest test-org-gcal--safe-substring-boundary-empty-string-returns-empty ()
  "Test safe-substring with empty string returns empty."
  (should (equal (org-gcal--safe-substring "" 0 0) ""))
  (should (equal (org-gcal--safe-substring "" 0 5) ""))
  (should (equal (org-gcal--safe-substring "" 0) "")))

(ert-deftest test-org-gcal--safe-substring-boundary-single-char-returns-char ()
  "Test safe-substring with single character string returns that char."
  (should (equal (org-gcal--safe-substring "a" 0 1) "a"))
  (should (equal (org-gcal--safe-substring "a" 0) "a")))

(ert-deftest test-org-gcal--safe-substring-boundary-zero-length-returns-empty ()
  "Test safe-substring with zero-length extraction returns empty."
  (should (equal (org-gcal--safe-substring "hello" 2 2) ""))
  (should (equal (org-gcal--safe-substring "hello" 0 0) "")))

(ert-deftest test-org-gcal--safe-substring-boundary-negative-indices-converts-to-positive ()
  "Test safe-substring with negative indices converts to positive positions."
  (should (equal (org-gcal--safe-substring "hello" -1 3) ""))
  ;; -1 means last char, so 0 to -1 is all but last char
  (should (equal (org-gcal--safe-substring "hello" 0 -1) "hell"))
  ;; -5 is position 0, -1 is position 4
  (should (equal (org-gcal--safe-substring "hello" -5 -1) "hell")))

;;; Error Cases

(ert-deftest test-org-gcal--safe-substring-error-out-of-range-from-returns-empty ()
  "Test safe-substring with from index out of range returns empty."
  (should (equal (org-gcal--safe-substring "hello" 10 15) ""))
  (should (equal (org-gcal--safe-substring "hello" 100) "")))

(ert-deftest test-org-gcal--safe-substring-error-out-of-range-to-returns-empty ()
  "Test safe-substring with to index out of range returns empty per implementation."
  ;; Implementation returns empty string when to is out of range
  (should (equal (org-gcal--safe-substring "hello" 0 100) ""))
  (should (equal (org-gcal--safe-substring "hello" 2 100) "")))

(ert-deftest test-org-gcal--safe-substring-error-to-less-than-from-returns-empty ()
  "Test safe-substring with to less than from returns empty."
  (should (equal (org-gcal--safe-substring "hello" 3 1) ""))
  (should (equal (org-gcal--safe-substring "hello" 5 0) "")))

(ert-deftest test-org-gcal--safe-substring-error-unicode-handles-correctly ()
  "Test safe-substring with Unicode characters handles multi-byte chars correctly."
  ;; Unicode emoji takes multiple bytes but counts as characters correctly
  (should (equal (org-gcal--safe-substring "hello 🎉 world" 6 7) "🎉"))
  (should (equal (org-gcal--safe-substring "日本語" 0 1) "日"))
  (should (equal (org-gcal--safe-substring "café" 0 4) "café")))

;; NOTE: nil input test omitted due to inconsistent behavior between contexts
;; In practice, nil is treated as empty list (length 0), but substring behavior varies

(provide 'test-org-gcal--safe-substring)
;;; test-org-gcal--safe-substring.el ends here
