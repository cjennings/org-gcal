;;; test-org-generic-id--locations-alist-to-hash.el --- Tests for org-generic-id--locations-alist-to-hash -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-generic-id--locations-alist-to-hash
;; Reverses transformation from org-generic-id--locations-hash-to-alist
;; Converts nested alist structure back to nested hash tables

;;; Code:

(require 'org-generic-id)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-generic-id--locations-alist-to-hash-normal-simple-nested-alist-returns-nested-hash ()
  "Test converting simple nested alist returns nested hash structure."
  (let* ((alist '(("file1" ("loc1" "id1"))))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 1))
    (let ((inner-hash (gethash "file1" result)))
      (should (hash-table-p inner-hash))
      (should (equal (hash-table-count inner-hash) 1))
      (should (equal (gethash "id1" inner-hash) "loc1")))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-normal-multiple-files-returns-hash-with-multiple-entries ()
  "Test converting alist with multiple files returns outer hash with multiple entries."
  (let* ((alist '(("file1" ("loc1" "id1"))
                  ("file2" ("loc2" "id2"))))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (should (equal (hash-table-count result) 2))
    (let ((inner-hash1 (gethash "file1" result))
          (inner-hash2 (gethash "file2" result)))
      (should (equal (gethash "id1" inner-hash1) "loc1"))
      (should (equal (gethash "id2" inner-hash2) "loc2")))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-normal-grouped-ids-expands-correctly ()
  "Test converting nested alist with grouped IDs creates separate hash entries for each."
  (let* ((alist '(("file1" ("loc1" "id1" "id3") ("loc2" "id2"))))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (should (equal (hash-table-count result) 1))
    (let ((inner-hash (gethash "file1" result)))
      (should (equal (hash-table-count inner-hash) 3))
      (should (equal (gethash "id1" inner-hash) "loc1"))
      (should (equal (gethash "id2" inner-hash) "loc2"))
      (should (equal (gethash "id3" inner-hash) "loc1")))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-normal-round-trip-preserves-data ()
  "Test converting nested hash to alist and back preserves original data."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash1 (make-hash-table :test 'equal))
        (inner-hash2 (make-hash-table :test 'equal)))
    (puthash "id1" "loc1" inner-hash1)
    (puthash "id2" "loc1" inner-hash1)
    (puthash "id3" "loc2" inner-hash2)
    (puthash "file1.org" inner-hash1 outer-hash)
    (puthash "file2.org" inner-hash2 outer-hash)
    (let* ((alist (org-generic-id--locations-hash-to-alist outer-hash))
           (result-hash (org-generic-id--locations-alist-to-hash alist)))
      (should (equal (hash-table-count result-hash) 2))
      (let ((result-inner1 (gethash "file1.org" result-hash))
            (result-inner2 (gethash "file2.org" result-hash)))
        (should (equal (hash-table-count result-inner1) 2))
        (should (equal (gethash "id1" result-inner1) "loc1"))
        (should (equal (gethash "id2" result-inner1) "loc1"))
        (should (equal (hash-table-count result-inner2) 1))
        (should (equal (gethash "id3" result-inner2) "loc2"))))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-normal-complex-nested-structure-converts-correctly ()
  "Test converting complex nested structure with multiple files and grouped IDs."
  (let* ((alist '(("file1.org" ("loc1" "id1" "id2") ("loc2" "id3"))
                  ("file2.org" ("loc3" "id4"))
                  ("file3.org" ("loc4" "id5" "id6" "id7"))))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (should (equal (hash-table-count result) 3))
    (let ((inner1 (gethash "file1.org" result))
          (inner2 (gethash "file2.org" result))
          (inner3 (gethash "file3.org" result)))
      (should (equal (hash-table-count inner1) 3))
      (should (equal (gethash "id1" inner1) "loc1"))
      (should (equal (gethash "id2" inner1) "loc1"))
      (should (equal (gethash "id3" inner1) "loc2"))
      (should (equal (hash-table-count inner2) 1))
      (should (equal (gethash "id4" inner2) "loc3"))
      (should (equal (hash-table-count inner3) 3))
      (should (equal (gethash "id5" inner3) "loc4"))
      (should (equal (gethash "id6" inner3) "loc4"))
      (should (equal (gethash "id7" inner3) "loc4")))))

;;; Boundary Cases

(ert-deftest test-org-generic-id--locations-alist-to-hash-boundary-empty-alist-returns-empty-hash ()
  "Test converting empty alist returns empty hash table."
  (let ((result (org-generic-id--locations-alist-to-hash '())))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 0))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-boundary-single-file-single-id-returns-minimal-hash ()
  "Test converting alist with single file and single ID returns minimal nested hash."
  (let* ((alist '(("file1" ("loc1" "id1"))))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (should (equal (hash-table-count result) 1))
    (let ((inner-hash (gethash "file1" result)))
      (should (equal (hash-table-count inner-hash) 1))
      (should (equal (gethash "id1" inner-hash) "loc1")))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-boundary-file-with-empty-inner-alist-returns-empty-inner-hash ()
  "Test converting alist with file entry but no IDs returns outer hash with empty inner hash."
  (let* ((alist '(("file1")))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (should (equal (hash-table-count result) 1))
    (let ((inner-hash (gethash "file1" result)))
      (should (hash-table-p inner-hash))
      (should (equal (hash-table-count inner-hash) 0)))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-boundary-large-nested-structure-handles-correctly ()
  "Test converting large nested structure with many files and IDs works correctly."
  (let* ((alist nil))
    ;; Create 10 files, each with 10 IDs all mapping to same location
    (dotimes (i 10)
      (let ((ids (cl-loop for j from 0 to 9 collect (format "id-%d-%d" i j))))
        (push (cons (format "file%02d.org" i)
                    (list (cons (format "loc-%d" i) ids)))
              alist)))
    (let ((result (org-generic-id--locations-alist-to-hash alist)))
      (should (equal (hash-table-count result) 10))
      ;; Check one file entry
      (let ((inner-hash (gethash "file00.org" result)))
        (should (not (null inner-hash)))
        (should (equal (hash-table-count inner-hash) 10))
        (should (equal (gethash "id-0-0" inner-hash) "loc-0"))))))

(ert-deftest test-org-generic-id--locations-alist-to-hash-boundary-special-characters-preserved ()
  "Test converting nested alist with special characters preserves them in hash."
  (let* ((alist '(("file with spaces.org" ("loc/with/slashes" "id-with-dashes"))))
         (result (org-generic-id--locations-alist-to-hash alist)))
    (let ((inner-hash (gethash "file with spaces.org" result)))
      (should (not (null inner-hash)))
      (should (equal (gethash "id-with-dashes" inner-hash) "loc/with/slashes")))))

;;; Error Cases

(ert-deftest test-org-generic-id--locations-alist-to-hash-error-nil-input-returns-empty-hash ()
  "Test converting nil returns empty hash table."
  (let ((result (org-generic-id--locations-alist-to-hash nil)))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 0))))

(provide 'test-org-generic-id--locations-alist-to-hash)
;;; test-org-generic-id--locations-alist-to-hash.el ends here
