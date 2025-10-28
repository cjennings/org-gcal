;;; test-integration-recurring-events.el --- Integration tests for recurring events -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integration tests for recurring event handling.
;;
;; Tests the full workflow of creating, updating, and syncing recurring events
;; from Google Calendar to org-mode entries. Recurring events have special
;; timestamp preservation logic to avoid changing dates on every sync.
;;
;; Components integrated:
;; - org-gcal--update-entry (entry creation/update orchestration)
;; - org-gcal--format-event-timestamp (timestamp formatting with recurrence logic)
;; - org-gcal--determine-headline (headline text selection)
;; - org-gcal--format-description-for-drawer (description formatting)
;; - org-element-at-point (org-mode property extraction)
;; - org-entry-get (property retrieval)
;;
;; Key behaviors tested:
;; - Recurrence property (RRULE) storage in org properties
;; - Old timestamp preservation for existing recurring events
;; - New timestamp usage for first-time recurring event creation
;; - Different recurrence patterns (WEEKLY, DAILY, COUNT-based)

;;; Code:

(require 'org-gcal)
(require 'ert)

;; Setup similar to org-gcal-test.el
(unless (and (boundp 'org-gcal-client-id) org-gcal-client-id
             (boundp 'org-gcal-client-secret) org-gcal-client-secret)
  (setq org-gcal-client-id "test_client_id"
        org-gcal-client-secret "test_client_secret"))

(defconst test-integration-recurring-events-calendar-id "recurring@test.com")

(defconst test-integration-recurring-events-weekly-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"weekly123\\\"\",
 \"id\": \"weekly-event-id\",
 \"status\": \"confirmed\",
 \"htmlLink\": \"https://www.google.com/calendar/event?eid=weeklyeid\",
 \"created\": \"2025-01-01T10:00:00.000Z\",
 \"updated\": \"2025-01-01T10:00:00.000Z\",
 \"summary\": \"Weekly Team Meeting\",
 \"description\": \"Recurring weekly meeting\",
 \"start\": {
  \"dateTime\": \"2025-01-06T14:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-06T15:00:00-08:00\"
 },
 \"recurrence\": [
  \"RRULE:FREQ=WEEKLY;BYDAY=MO\"
 ],
 \"reminders\": {
  \"useDefault\": true
 }
}
")

(defconst test-integration-recurring-events-daily-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"daily456\\\"\",
 \"id\": \"daily-event-id\",
 \"status\": \"confirmed\",
 \"htmlLink\": \"https://www.google.com/calendar/event?eid=dailyeid\",
 \"created\": \"2025-01-01T10:00:00.000Z\",
 \"updated\": \"2025-01-01T10:00:00.000Z\",
 \"summary\": \"Daily Standup\",
 \"description\": \"Daily standup meeting\",
 \"start\": {
  \"dateTime\": \"2025-01-06T09:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-06T09:15:00-08:00\"
 },
 \"recurrence\": [
  \"RRULE:FREQ=DAILY;COUNT=10\"
 ],
 \"reminders\": {
  \"useDefault\": true
 }
}
")

(defmacro test-integration-recurring-events--with-temp-buffer (contents &rest body)
  "Create an org-mode temp buffer with CONTENTS and execute BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun test-integration-recurring-events--json-read-string (json)
  "Parse JSON string into event plist."
  (with-temp-buffer
    (insert json)
    (org-gcal--json-read)))

(defconst test-integration-recurring-events-weekly-event
  (test-integration-recurring-events--json-read-string test-integration-recurring-events-weekly-json))

(defconst test-integration-recurring-events-daily-event
  (test-integration-recurring-events--json-read-string test-integration-recurring-events-daily-json))

;;; Normal Cases - Recurring Event Creation

(ert-deftest test-integration-recurring-events-weekly-creates-with-recurrence ()
  "Test that weekly recurring event is created with recurrence property.

Creates a new recurring event from Google Calendar JSON and verifies that
the RRULE recurrence pattern is properly stored in org properties.

Components integrated:
- org-gcal--update-entry (creates entry from event data)
- org-gcal--format-event-timestamp (formats timestamp)
- org-gcal--determine-headline (sets headline from summary)
- org-gcal--format-description-for-drawer (formats description)
- org-element-at-point (extracts org properties)

Validates:
- RECURRENCE property contains RRULE string
- Headline is set from event summary
- Timestamp is formatted correctly
- Entry-id combines event-id and calendar-id"
  (test-integration-recurring-events--with-temp-buffer
      "* "
    (org-gcal--update-entry test-integration-recurring-events-calendar-id
                            test-integration-recurring-events-weekly-event)
    (org-back-to-heading)
    (let ((elem (org-element-at-point)))
      ;; Verify headline
      (should (equal (org-element-property :title elem)
                     '("Weekly Team Meeting")))
      ;; Verify recurrence property is stored
      (should (equal (org-element-property :RECURRENCE elem)
                     "[\"RRULE:FREQ=WEEKLY;BYDAY=MO\"]"))
      ;; Verify calendar-id and entry-id
      (should (equal (org-element-property :CALENDAR-ID elem)
                     "recurring@test.com"))
      (should (equal (org-element-property :ENTRY-ID elem)
                     "weekly-event-id/recurring@test.com")))
    ;; Check drawer contains timestamp
    (re-search-forward ":org-gcal:")
    (let ((elem (org-element-at-point)))
      (should (equal (org-element-property :drawer-name elem)
                     "org-gcal"))
      (let ((contents (buffer-substring-no-properties
                       (org-element-property :contents-begin elem)
                       (org-element-property :contents-end elem))))
        ;; Should have timestamp
        (should (string-match-p "<2025-01-06" contents))
        ;; Should have description
        (should (string-match-p "Recurring weekly meeting" contents))))))

(ert-deftest test-integration-recurring-events-daily-creates-with-count ()
  "Test that daily recurring event with COUNT creates correctly."
  (test-integration-recurring-events--with-temp-buffer
      "* "
    (org-gcal--update-entry test-integration-recurring-events-calendar-id
                            test-integration-recurring-events-daily-event)
    (org-back-to-heading)
    (let ((elem (org-element-at-point)))
      ;; Verify headline
      (should (equal (org-element-property :title elem)
                     '("Daily Standup")))
      ;; Verify recurrence property with COUNT
      (should (equal (org-element-property :RECURRENCE elem)
                     "[\"RRULE:FREQ=DAILY;COUNT=10\"]"))
      ;; Verify the short duration is captured (15 minutes)
      (re-search-forward ":org-gcal:")
      (let ((elem (org-element-at-point)))
        (let ((contents (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))))
          (should (string-match-p "09:00-09:15" contents)))))))

;;; Boundary Cases - Recurring Event Updates

(ert-deftest test-integration-recurring-events-update-preserves-recurrence ()
  "Test that updating a recurring event preserves the recurrence property."
  (test-integration-recurring-events--with-temp-buffer
   "\
* Old Weekly Meeting
:PROPERTIES:
:ETag:     \"old123\"
:RECURRENCE: [\"RRULE:FREQ=WEEKLY;BYDAY=MO\"]
:calendar-id: recurring@test.com
:entry-id:   weekly-event-id/recurring@test.com
:END:
:org-gcal:
<2024-12-30 Mon 14:00-15:00>

Old description
:END:
"
   (org-gcal--update-entry test-integration-recurring-events-calendar-id
                           test-integration-recurring-events-weekly-event)
   (org-back-to-heading)
   (let ((elem (org-element-at-point)))
     ;; Verify headline updated
     (should (equal (org-element-property :title elem)
                    '("Weekly Team Meeting")))
     ;; Verify recurrence property still present
     (should (equal (org-element-property :RECURRENCE elem)
                    "[\"RRULE:FREQ=WEEKLY;BYDAY=MO\"]"))
     ;; Verify description updated
     (re-search-forward ":org-gcal:")
     (let ((elem (org-element-at-point)))
       (let ((contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
         (should (string-match-p "Recurring weekly meeting" contents)))))))

(ert-deftest test-integration-recurring-events-preserves-old-timestamps ()
  "Test that recurring events preserve original timestamps across updates.

**KEY TEST**: This validates the core refactored timestamp preservation logic.

When a recurring event is updated with a new instance date from Google Calendar
(e.g., updating the 2025-01-06 instance), the timestamp in the org entry should
remain the original series start date (2024-12-30), NOT jump to the current
instance date. This prevents the timestamp from changing on every sync.

Components integrated:
- org-gcal--update-entry (orchestrates update, extracts old-start/old-end)
- org-gcal--format-event-timestamp (KEY: preserves old timestamps when recurrence exists)
- org-gcal--get-time-and-desc (extracts existing timestamp from drawer)
- Workflow: JSON event → extract old timestamp → format with preservation → org entry

Validates the refactored logic from org-gcal--format-event-timestamp:
  (when (and recurrence old-start old-end)  ;; All three must exist
    (setq start old-start                    ;; Use old, not new
          end old-end))

Expected behavior:
- Event JSON has start: 2025-01-06 (new instance)
- Org entry has timestamp: <2024-12-30> (original)
- After update: timestamp stays <2024-12-30> (preserved!)
- Without this logic: timestamp would become <2025-01-06> (wrong!)"
  (test-integration-recurring-events--with-temp-buffer
   "\
* Weekly Team Meeting
:PROPERTIES:
:ETag:     \"weekly123\"
:RECURRENCE: [\"RRULE:FREQ=WEEKLY;BYDAY=MO\"]
:calendar-id: recurring@test.com
:entry-id:   weekly-event-id/recurring@test.com
:END:
:org-gcal:
<2024-12-30 Mon 14:00-15:00>

Recurring weekly meeting
:END:
"
   ;; Update with new event data (different date)
   (org-gcal--update-entry test-integration-recurring-events-calendar-id
                           test-integration-recurring-events-weekly-event)
   (org-back-to-heading)
   (re-search-forward ":org-gcal:")
   (let ((elem (org-element-at-point)))
     (let ((contents (buffer-substring-no-properties
                      (org-element-property :contents-begin elem)
                      (org-element-property :contents-end elem))))
       ;; Should preserve the OLD timestamp (2024-12-30) not the new one (2025-01-06)
       ;; This is the key behavior from org-gcal--format-event-timestamp
       (should (string-match-p "<2024-12-30 Mon 14:00-15:00>" contents))
       (should-not (string-match-p "2025-01-06" contents))))))

;;; Edge Cases - Missing or Invalid Recurrence

(ert-deftest test-integration-recurring-events-no-recurrence-uses-new-timestamps ()
  "Test that events without recurrence use new timestamps on update."
  (test-integration-recurring-events--with-temp-buffer
   "\
* Weekly Team Meeting
:PROPERTIES:
:ETag:     \"weekly123\"
:calendar-id: recurring@test.com
:entry-id:   weekly-event-id/recurring@test.com
:END:
:org-gcal:
<2024-12-30 Mon 14:00-15:00>

Recurring weekly meeting
:END:
"
   ;; Update with new event data that HAS recurrence
   (org-gcal--update-entry test-integration-recurring-events-calendar-id
                           test-integration-recurring-events-weekly-event)
   (org-back-to-heading)
   (re-search-forward ":org-gcal:")
   (let ((elem (org-element-at-point)))
     (let ((contents (buffer-substring-no-properties
                      (org-element-property :contents-begin elem)
                      (org-element-property :contents-end elem))))
       ;; Without old recurrence property, should NOT preserve old timestamp
       ;; Should use new timestamp from event
       (should (string-match-p "2025-01-06" contents))
       (should-not (string-match-p "2024-12-30" contents))))))

(ert-deftest test-integration-recurring-events-missing-old-timestamps-uses-new ()
  "Test that recurring events without old timestamps use new ones."
  (test-integration-recurring-events--with-temp-buffer
      "* "
    ;; First create the event
    (org-gcal--update-entry test-integration-recurring-events-calendar-id
                            test-integration-recurring-events-weekly-event)
    (org-back-to-heading)
    (re-search-forward ":org-gcal:")
    (let ((elem (org-element-at-point)))
      (let ((contents (buffer-substring-no-properties
                       (org-element-property :contents-begin elem)
                       (org-element-property :contents-end elem))))
        ;; First creation should use the event's timestamp
        (should (string-match-p "2025-01-06" contents))))))

(provide 'test-integration-recurring-events)
;;; test-integration-recurring-events.el ends here
