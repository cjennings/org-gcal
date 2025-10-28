;;; test-org-gcal--filter.el --- Tests for org-gcal--filter -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--filter
;; Filters calendar events based on org-gcal-fetch-event-filters predicates

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--filter-normal-no-filters-returns-all-items ()
  "Test filtering with no filters configured returns all items."
  (let ((org-gcal-fetch-event-filters nil)
        (items '(item1 item2 item3)))
    (should (equal (org-gcal--filter items)
                   items))))

(ert-deftest test-org-gcal--filter-normal-single-filter-accepts-all-returns-all ()
  "Test filtering with single filter that accepts all returns all items."
  (let ((org-gcal-fetch-event-filters (list (lambda (_item) t)))
        (items '(item1 item2 item3)))
    (should (equal (org-gcal--filter items)
                   items))))

(ert-deftest test-org-gcal--filter-normal-single-filter-rejects-all-returns-empty ()
  "Test filtering with single filter that rejects all returns empty list."
  (let ((org-gcal-fetch-event-filters (list (lambda (_item) nil)))
        (items '(item1 item2 item3)))
    (should (equal (org-gcal--filter items)
                   nil))))

(ert-deftest test-org-gcal--filter-normal-single-filter-selective-returns-subset ()
  "Test filtering with selective filter returns matching subset."
  (let ((org-gcal-fetch-event-filters
         (list (lambda (item) (member item '(item1 item3)))))
        (items '(item1 item2 item3 item4)))
    (should (equal (org-gcal--filter items)
                   '(item1 item3)))))

(ert-deftest test-org-gcal--filter-normal-multiple-filters-all-accept-returns-all ()
  "Test filtering with multiple filters all accepting returns all items."
  (let ((org-gcal-fetch-event-filters
         (list (lambda (_item) t)
               (lambda (_item) t)))
        (items '(item1 item2)))
    (should (equal (org-gcal--filter items)
                   items))))

(ert-deftest test-org-gcal--filter-normal-multiple-filters-and-logic-returns-intersection ()
  "Test filtering with multiple filters uses AND logic (intersection)."
  (let ((org-gcal-fetch-event-filters
         (list (lambda (item) (member item '(item1 item2 item3)))
               (lambda (item) (member item '(item2 item3 item4)))))
        (items '(item1 item2 item3 item4)))
    (should (equal (org-gcal--filter items)
                   '(item2 item3)))))

;;; Boundary Cases

(ert-deftest test-org-gcal--filter-boundary-empty-items-list-returns-empty ()
  "Test filtering empty items list returns empty list."
  (let ((org-gcal-fetch-event-filters (list (lambda (_item) t)))
        (items '()))
    (should (equal (org-gcal--filter items)
                   nil))))

(ert-deftest test-org-gcal--filter-boundary-single-item-accepted-returns-item ()
  "Test filtering single item that is accepted returns that item."
  (let ((org-gcal-fetch-event-filters (list (lambda (_item) t)))
        (items '(item1)))
    (should (equal (org-gcal--filter items)
                   '(item1)))))

(ert-deftest test-org-gcal--filter-boundary-single-item-rejected-returns-empty ()
  "Test filtering single item that is rejected returns empty list."
  (let ((org-gcal-fetch-event-filters (list (lambda (_item) nil)))
        (items '(item1)))
    (should (equal (org-gcal--filter items)
                   nil))))

;;; Error Cases

(ert-deftest test-org-gcal--filter-error-nil-items-returns-nil ()
  "Test filtering nil items list returns nil."
  (let ((org-gcal-fetch-event-filters (list (lambda (_item) t)))
        (items nil))
    (should (equal (org-gcal--filter items)
                   nil))))

(provide 'test-org-gcal--filter)
;;; test-org-gcal--filter.el ends here
