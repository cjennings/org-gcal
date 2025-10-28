;;; test-org-generic-id--alist-to-hash.el --- Tests for org-generic-id--alist-to-hash -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-generic-id--alist-to-hash
;; Reverses the transformation made by org-generic-id--hash-to-alist
;; Converts alist with grouped keys back to hash table

;;; Code:

(require 'org-generic-id)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-generic-id--alist-to-hash-normal-simple-alist-returns-hash ()
  "Test converting simple alist returns hash with reversed key-value mapping."
  (let* ((alist '(("file1" "id1") ("file2" "id2")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 2))
    (should (equal (gethash "id1" result) "file1"))
    (should (equal (gethash "id2" result) "file2"))))

(ert-deftest test-org-generic-id--alist-to-hash-normal-grouped-keys-expands-to-multiple-entries ()
  "Test converting alist with grouped keys creates separate hash entries for each ID."
  (let* ((alist '(("file1" "id1" "id3") ("file2" "id2")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 3))
    (should (equal (gethash "id1" result) "file1"))
    (should (equal (gethash "id2" result) "file2"))
    (should (equal (gethash "id3" result) "file1"))))

(ert-deftest test-org-generic-id--alist-to-hash-normal-round-trip-preserves-data ()
  "Test converting hash to alist and back preserves original data."
  (let ((original-hash (make-hash-table :test 'equal)))
    (puthash "id1" "file1" original-hash)
    (puthash "id2" "file2" original-hash)
    (puthash "id3" "file1" original-hash)
    (let* ((alist (org-generic-id--hash-to-alist original-hash))
           (result-hash (org-generic-id--alist-to-hash alist)))
      (should (equal (hash-table-count result-hash) 3))
      (should (equal (gethash "id1" result-hash) "file1"))
      (should (equal (gethash "id2" result-hash) "file2"))
      (should (equal (gethash "id3" result-hash) "file1")))))

(ert-deftest test-org-generic-id--alist-to-hash-normal-multiple-grouped-entries-expands-correctly ()
  "Test converting alist with multiple groups creates correct hash entries."
  (let* ((alist '(("file1" "id1" "id2" "id3")
                  ("file2" "id4" "id5")
                  ("file3" "id6")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (equal (hash-table-count result) 6))
    (should (equal (gethash "id1" result) "file1"))
    (should (equal (gethash "id2" result) "file1"))
    (should (equal (gethash "id3" result) "file1"))
    (should (equal (gethash "id4" result) "file2"))
    (should (equal (gethash "id5" result) "file2"))
    (should (equal (gethash "id6" result) "file3"))))

;;; Boundary Cases

(ert-deftest test-org-generic-id--alist-to-hash-boundary-empty-alist-returns-empty-hash ()
  "Test converting empty alist returns empty hash table."
  (let ((result (org-generic-id--alist-to-hash '())))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 0))))

(ert-deftest test-org-generic-id--alist-to-hash-boundary-single-entry-single-id-returns-one-element-hash ()
  "Test converting alist with single entry and single ID returns hash with one entry."
  (let* ((alist '(("file1" "id1")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (equal (hash-table-count result) 1))
    (should (equal (gethash "id1" result) "file1"))))

(ert-deftest test-org-generic-id--alist-to-hash-boundary-single-entry-multiple-ids-returns-multiple-hash-entries ()
  "Test converting alist with single group of multiple IDs creates multiple hash entries."
  (let* ((alist '(("file1" "id1" "id2" "id3" "id4")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (equal (hash-table-count result) 4))
    (should (equal (gethash "id1" result) "file1"))
    (should (equal (gethash "id2" result) "file1"))
    (should (equal (gethash "id3" result) "file1"))
    (should (equal (gethash "id4" result) "file1"))))

(ert-deftest test-org-generic-id--alist-to-hash-boundary-large-alist-handles-correctly ()
  "Test converting large alist with many entries works correctly."
  (let* ((alist '(("file1")  ; Will add 50 IDs
                  ("file2"))) ; Will add 50 IDs
         ;; Add 50 IDs to file1
         (file1-ids (cl-loop for i from 1 to 50 collect (format "id-a-%02d" i)))
         ;; Add 50 IDs to file2
         (file2-ids (cl-loop for i from 1 to 50 collect (format "id-b-%02d" i)))
         (alist-with-ids (list (cons "file1" file1-ids)
                               (cons "file2" file2-ids)))
         (result (org-generic-id--alist-to-hash alist-with-ids)))
    (should (equal (hash-table-count result) 100))
    ;; Check a few samples
    (should (equal (gethash "id-a-01" result) "file1"))
    (should (equal (gethash "id-a-50" result) "file1"))
    (should (equal (gethash "id-b-01" result) "file2"))
    (should (equal (gethash "id-b-50" result) "file2"))))

(ert-deftest test-org-generic-id--alist-to-hash-boundary-special-characters-preserved ()
  "Test converting alist with special characters preserves them in hash."
  (let* ((alist '(("file/with/slashes.org" "id-with-dashes")
                  ("file with spaces.org" "id_with_underscores")
                  ("file-with-dashes.org" "id@with@at")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (equal (hash-table-count result) 3))
    (should (equal (gethash "id-with-dashes" result) "file/with/slashes.org"))
    (should (equal (gethash "id_with_underscores" result) "file with spaces.org"))
    (should (equal (gethash "id@with@at" result) "file-with-dashes.org"))))

;;; Error Cases

(ert-deftest test-org-generic-id--alist-to-hash-error-nil-input-returns-empty-hash ()
  "Test converting nil returns empty hash table."
  (let ((result (org-generic-id--alist-to-hash nil)))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 0))))

(ert-deftest test-org-generic-id--alist-to-hash-error-malformed-entry-only-key-no-ids-returns-empty-hash ()
  "Test converting alist with entry containing only key (no IDs) returns empty hash."
  (let* ((alist '(("file1")))
         (result (org-generic-id--alist-to-hash alist)))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 0))))

(provide 'test-org-generic-id--alist-to-hash)
;;; test-org-generic-id--alist-to-hash.el ends here
