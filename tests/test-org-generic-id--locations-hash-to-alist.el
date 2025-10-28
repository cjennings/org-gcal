;;; test-org-generic-id--locations-hash-to-alist.el --- Tests for org-generic-id--locations-hash-to-alist -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-generic-id--locations-hash-to-alist
;; Converts nested hash table structure to nested alist for file persistence
;; Outer hash maps locations to inner hash tables of id->location mappings

;;; Code:

(require 'org-generic-id)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-generic-id--locations-hash-to-alist-normal-simple-nested-hash-returns-nested-alist ()
  "Test converting simple nested hash returns nested alist structure."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash1 (make-hash-table :test 'equal)))
    (puthash "id1" "loc1" inner-hash1)
    (puthash "file1" inner-hash1 outer-hash)
    (let ((result (org-generic-id--locations-hash-to-alist outer-hash)))
      (should (equal (length result) 1))
      (should (equal (car (car result)) "file1"))
      (let ((inner-alist (cdr (car result))))
        (should (equal (assoc "loc1" inner-alist) '("loc1" "id1")))))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-normal-multiple-files-returns-sorted-alist ()
  "Test converting hash with multiple files returns alist sorted by file names."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash1 (make-hash-table :test 'equal))
        (inner-hash2 (make-hash-table :test 'equal)))
    (puthash "id1" "loc1" inner-hash1)
    (puthash "id2" "loc2" inner-hash2)
    (puthash "zebra.org" inner-hash1 outer-hash)
    (puthash "apple.org" inner-hash2 outer-hash)
    (let ((result (org-generic-id--locations-hash-to-alist outer-hash)))
      (should (equal (length result) 2))
      (should (equal (car (nth 0 result)) "apple.org"))
      (should (equal (car (nth 1 result)) "zebra.org")))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-normal-inner-hash-with-multiple-ids-groups-correctly ()
  "Test converting nested hash where inner hash has multiple IDs grouped by location."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash (make-hash-table :test 'equal)))
    (puthash "id1" "loc1" inner-hash)
    (puthash "id2" "loc2" inner-hash)
    (puthash "id3" "loc1" inner-hash)
    (puthash "file1" inner-hash outer-hash)
    (let* ((result (org-generic-id--locations-hash-to-alist outer-hash))
           (file1-entry (assoc "file1" result))
           (inner-alist (cdr file1-entry)))
      (should (equal (length inner-alist) 2))
      (let ((loc1-entry (assoc "loc1" inner-alist)))
        (should (equal (car loc1-entry) "loc1"))
        (should (equal (sort (cdr loc1-entry) #'string<) '("id1" "id3"))))
      (should (equal (assoc "loc2" inner-alist) '("loc2" "id2"))))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-normal-multiple-files-each-with-ids-returns-complete-structure ()
  "Test converting complex nested structure with multiple files and IDs."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash1 (make-hash-table :test 'equal))
        (inner-hash2 (make-hash-table :test 'equal)))
    (puthash "id1" "loc1" inner-hash1)
    (puthash "id2" "loc1" inner-hash1)
    (puthash "id3" "loc2" inner-hash2)
    (puthash "file1.org" inner-hash1 outer-hash)
    (puthash "file2.org" inner-hash2 outer-hash)
    (let ((result (org-generic-id--locations-hash-to-alist outer-hash)))
      (should (equal (length result) 2))
      (let* ((file1-entry (assoc "file1.org" result))
             (file1-alist (cdr file1-entry))
             (file2-entry (assoc "file2.org" result))
             (file2-alist (cdr file2-entry)))
        (should (equal (length file1-alist) 1))
        (should (equal (assoc "loc1" file1-alist) '("loc1" "id1" "id2")))
        (should (equal (length file2-alist) 1))
        (should (equal (assoc "loc2" file2-alist) '("loc2" "id3")))))))

;;; Boundary Cases

(ert-deftest test-org-generic-id--locations-hash-to-alist-boundary-empty-outer-hash-returns-empty-list ()
  "Test converting empty outer hash returns empty list."
  (let ((outer-hash (make-hash-table :test 'equal)))
    (should (equal (org-generic-id--locations-hash-to-alist outer-hash) nil))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-boundary-single-file-single-id-returns-minimal-structure ()
  "Test converting hash with single file and single ID returns minimal nested structure."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash (make-hash-table :test 'equal)))
    (puthash "id1" "loc1" inner-hash)
    (puthash "file1" inner-hash outer-hash)
    (let ((result (org-generic-id--locations-hash-to-alist outer-hash)))
      (should (equal (length result) 1))
      (should (equal (car result) '("file1" ("loc1" "id1")))))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-boundary-inner-hash-empty-returns-file-with-empty-alist ()
  "Test converting outer hash with empty inner hash returns file entry with nil value."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash (make-hash-table :test 'equal)))
    (puthash "file1" inner-hash outer-hash)
    (let ((result (org-generic-id--locations-hash-to-alist outer-hash)))
      (should (equal (length result) 1))
      (should (equal (car (car result)) "file1"))
      (should (equal (cdr (car result)) nil)))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-boundary-large-nested-structure-handles-correctly ()
  "Test converting large nested structure with many files and IDs works correctly."
  (let ((outer-hash (make-hash-table :test 'equal)))
    ;; Create 10 files, each with an inner hash of 10 IDs
    (dotimes (i 10)
      (let ((inner-hash (make-hash-table :test 'equal)))
        (dotimes (j 10)
          (puthash (format "id-%d-%d" i j) (format "loc-%d" i) inner-hash))
        (puthash (format "file%02d.org" i) inner-hash outer-hash)))
    (let ((result (org-generic-id--locations-hash-to-alist outer-hash)))
      (should (equal (length result) 10))
      ;; Check one file entry
      (let* ((file0-entry (assoc "file00.org" result))
             (file0-alist (cdr file0-entry)))
        (should (not (null file0-entry)))
        (should (equal (length file0-alist) 1))))))

(ert-deftest test-org-generic-id--locations-hash-to-alist-boundary-special-characters-preserved ()
  "Test converting nested hash with special characters preserves them."
  (let ((outer-hash (make-hash-table :test 'equal))
        (inner-hash (make-hash-table :test 'equal)))
    (puthash "id-with-dashes" "loc/with/slashes" inner-hash)
    (puthash "file with spaces.org" inner-hash outer-hash)
    (let* ((result (org-generic-id--locations-hash-to-alist outer-hash))
           (file-entry (assoc "file with spaces.org" result))
           (inner-alist (cdr file-entry)))
      (should (assoc "loc/with/slashes" inner-alist)))))

;;; Error Cases

(ert-deftest test-org-generic-id--locations-hash-to-alist-error-nil-input-throws-error ()
  "Test converting nil throws error."
  (should-error (org-generic-id--locations-hash-to-alist nil)))

(provide 'test-org-generic-id--locations-hash-to-alist)
;;; test-org-generic-id--locations-hash-to-alist.el ends here
