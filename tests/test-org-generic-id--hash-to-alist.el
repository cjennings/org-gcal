;;; test-org-generic-id--hash-to-alist.el --- Tests for org-generic-id--hash-to-alist -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-generic-id--hash-to-alist
;; Converts hash table to alist while reversing keys and values
;; Groups all keys with same value together

;;; Code:

(require 'org-generic-id)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-generic-id--hash-to-alist-normal-simple-hash-returns-reversed-alist ()
  "Test converting simple hash with unique values returns reversed alist."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id1" "file1" hash)
    (puthash "id2" "file2" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 2))
      (should (equal (assoc "file1" result) '("file1" "id1")))
      (should (equal (assoc "file2" result) '("file2" "id2"))))))

(ert-deftest test-org-generic-id--hash-to-alist-normal-multiple-keys-same-value-groups-keys ()
  "Test converting hash with multiple keys sharing same value groups them together."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id1" "file1" hash)
    (puthash "id2" "file2" hash)
    (puthash "id3" "file1" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 2))
      (let ((file1-entry (assoc "file1" result)))
        (should (equal (car file1-entry) "file1"))
        (should (equal (sort (cdr file1-entry) #'string<) '("id1" "id3"))))
      (should (equal (assoc "file2" result) '("file2" "id2"))))))

(ert-deftest test-org-generic-id--hash-to-alist-normal-result-sorted-by-keys ()
  "Test converting hash returns alist sorted alphabetically by keys (values from hash)."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id1" "zebra" hash)
    (puthash "id2" "apple" hash)
    (puthash "id3" "mango" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 3))
      (should (equal (car (nth 0 result)) "apple"))
      (should (equal (car (nth 1 result)) "mango"))
      (should (equal (car (nth 2 result)) "zebra")))))

(ert-deftest test-org-generic-id--hash-to-alist-normal-grouped-ids-sorted ()
  "Test converting hash with grouped IDs sorts IDs within each group."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id-z" "file1" hash)
    (puthash "id-a" "file1" hash)
    (puthash "id-m" "file1" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 1))
      (let ((file1-entry (assoc "file1" result)))
        (should (equal (cdr file1-entry) '("id-a" "id-m" "id-z")))))))

;;; Boundary Cases

(ert-deftest test-org-generic-id--hash-to-alist-boundary-empty-hash-returns-empty-list ()
  "Test converting empty hash returns empty list."
  (let ((hash (make-hash-table :test 'equal)))
    (should (equal (org-generic-id--hash-to-alist hash) nil))))

(ert-deftest test-org-generic-id--hash-to-alist-boundary-single-entry-returns-single-element-alist ()
  "Test converting hash with single entry returns one-element alist."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id1" "file1" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 1))
      (should (equal (car result) '("file1" "id1"))))))

(ert-deftest test-org-generic-id--hash-to-alist-boundary-all-keys-same-value-returns-single-group ()
  "Test converting hash where all keys map to same value returns single alist entry."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id1" "file1" hash)
    (puthash "id2" "file1" hash)
    (puthash "id3" "file1" hash)
    (puthash "id4" "file1" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 1))
      (let ((file1-entry (assoc "file1" result)))
        (should (equal (car file1-entry) "file1"))
        (should (equal (length (cdr file1-entry)) 4))
        (should (equal (cdr file1-entry) '("id1" "id2" "id3" "id4")))))))

(ert-deftest test-org-generic-id--hash-to-alist-boundary-large-hash-handles-correctly ()
  "Test converting large hash with many entries works correctly."
  (let ((hash (make-hash-table :test 'equal)))
    ;; Create 100 entries: 50 mapping to file1, 50 to file2
    (dotimes (i 50)
      (puthash (format "id-a-%02d" i) "file1" hash)
      (puthash (format "id-b-%02d" i) "file2" hash))
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 2))
      (let ((file1-entry (assoc "file1" result))
            (file2-entry (assoc "file2" result)))
        (should (equal (length (cdr file1-entry)) 50))
        (should (equal (length (cdr file2-entry)) 50))))))

(ert-deftest test-org-generic-id--hash-to-alist-boundary-special-characters-in-keys-and-values ()
  "Test converting hash with special characters in keys and values preserves them."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "id-with-dashes" "file/with/slashes.org" hash)
    (puthash "id_with_underscores" "file with spaces.org" hash)
    (puthash "id@with@at" "file-with-dashes.org" hash)
    (let ((result (org-generic-id--hash-to-alist hash)))
      (should (equal (length result) 3))
      (should (assoc "file/with/slashes.org" result))
      (should (assoc "file with spaces.org" result))
      (should (assoc "file-with-dashes.org" result)))))

;;; Error Cases

(ert-deftest test-org-generic-id--hash-to-alist-error-nil-input-throws-error ()
  "Test converting nil throws error."
  (should-error (org-generic-id--hash-to-alist nil)))

(provide 'test-org-generic-id--hash-to-alist)
;;; test-org-generic-id--hash-to-alist.el ends here
