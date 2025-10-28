;;; test-org-gcal--format-event-timestamp.el --- Tests for org-gcal--format-event-timestamp -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Unit tests for org-gcal--format-event-timestamp
;; Formats timestamp string for event from START to END dates

;;; Code:

(require 'org-gcal)
(require 'ert)

;;; Normal Cases

(ert-deftest test-org-gcal--format-event-timestamp-normal-single-point-returns-timestamp ()
  "Test formatting single point in time returns simple timestamp."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15T14:30:00" "2021-03-15T14:30:00")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon 14:30>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-normal-same-day-range-returns-time-range ()
  "Test formatting same-day time range returns time range format."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15T14:30:00" "2021-03-15T16:00:00")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon 14:30-16:00>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-normal-multi-day-range-returns-date-range ()
  "Test formatting multi-day range returns date range format."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15T14:30:00" "2021-03-17T16:00:00")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon 14:30>--<2021-03-17 Wed 16:00>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-normal-all-day-single-date-returns-date ()
  "Test formatting all-day single date returns date only."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15" "2021-03-16")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-normal-all-day-range-returns-date-range ()
  "Test formatting all-day multi-day range returns date range."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15" "2021-03-18")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon>--<2021-03-17 Wed>" result))))

;;; Boundary Cases

(ert-deftest test-org-gcal--format-event-timestamp-boundary-recurring-preserves-old-timestamps ()
  "Test recurring events preserve old start/end timestamps."
  (let ((result (org-gcal--format-event-timestamp
                 "2021-03-15T14:30:00" "2021-03-15T16:00:00"
                 "RRULE:FREQ=WEEKLY"
                 "2021-03-08T14:30:00" "2021-03-08T16:00:00")))
    (should (stringp result))
    ;; Should use old-start and old-end, not start and end
    (should (string-match-p "<2021-03-08 Mon 14:30-16:00>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-boundary-midnight-start-returns-00-00 ()
  "Test formatting event starting at midnight returns 00:00 time."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15T00:00:00" "2021-03-15T01:00:00")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon 00:00-01:00>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-boundary-end-of-day-returns-23-59 ()
  "Test formatting event ending at end of day returns 23:59 time."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15T23:00:00" "2021-03-15T23:59:00")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15 Mon 23:00-23:59>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-boundary-year-boundary-returns-correct-range ()
  "Test formatting range crossing year boundary returns correct dates."
  (let ((result (org-gcal--format-event-timestamp "2021-12-31T22:00:00" "2022-01-01T02:00:00")))
    (should (stringp result))
    (should (string-match-p "<2021-12-31.*>--<2022-01-01.*>" result))))

(ert-deftest test-org-gcal--format-event-timestamp-boundary-recurring-without-old-timestamps-uses-new ()
  "Test recurring events without old timestamps use new ones."
  (let ((result (org-gcal--format-event-timestamp
                 "2021-03-15T14:30:00" "2021-03-15T16:00:00"
                 "RRULE:FREQ=WEEKLY"
                 nil nil)))
    (should (stringp result))
    ;; Should use start and end since old timestamps are nil
    (should (string-match-p "<2021-03-15 Mon 14:30-16:00>" result))))

;;; Error Cases

(ert-deftest test-org-gcal--format-event-timestamp-error-nil-start-returns-nil ()
  "Test nil start date returns nil."
  (should (equal (org-gcal--format-event-timestamp nil "2021-03-15T14:30:00") nil)))

(ert-deftest test-org-gcal--format-event-timestamp-error-nil-end-returns-nil ()
  "Test nil end date returns nil."
  (should (equal (org-gcal--format-event-timestamp "2021-03-15T14:30:00" nil) nil)))

(ert-deftest test-org-gcal--format-event-timestamp-error-both-nil-returns-nil ()
  "Test nil start and end returns nil."
  (should (equal (org-gcal--format-event-timestamp nil nil) nil)))

(ert-deftest test-org-gcal--format-event-timestamp-error-empty-start-returns-formatted ()
  "Test empty start string processes through (format-iso2org returns nil for empty)."
  (let ((result (org-gcal--format-event-timestamp "" "2021-03-15T14:30:00")))
    (should (stringp result))
    (should (string-match-p "nil--<2021-03-15" result))))

(ert-deftest test-org-gcal--format-event-timestamp-error-empty-end-returns-formatted ()
  "Test empty end string processes through (format-iso2org returns nil for empty)."
  (let ((result (org-gcal--format-event-timestamp "2021-03-15T14:30:00" "")))
    (should (stringp result))
    (should (string-match-p "<2021-03-15.*--nil" result))))

(provide 'test-org-gcal--format-event-timestamp)
;;; test-org-gcal--format-event-timestamp.el ends here
