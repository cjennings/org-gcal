;;; test-integration-multi-event-sync.el --- Integration tests for multi-event sync scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integration tests for scenarios involving multiple events.
;;
;; Tests how multiple Google Calendar events coexist and interact within the
;; same org-mode buffer. Validates event isolation (updates to one don't affect
;; others), multi-calendar support (work + personal), different event types
;; together (regular, recurring, all-day), and nested heading structures.
;;
;; Components integrated:
;; - org-gcal--update-entry (updates entries at different buffer positions)
;; - org-element-at-point (extracts properties from multiple entries)
;; - Buffer navigation (org-back-to-heading, re-search-forward)
;; - Property management (multiple entries with different properties)
;; - Org-mode hierarchy (nested headings, different levels)
;; - Entry identification (calendar-id + entry-id combinations)
;;
;; Key behaviors tested:
;; - Multiple events in same buffer don't interfere with each other
;; - Updates to one event preserve others unchanged
;; - Same calendar ID with different event IDs works correctly
;; - Different calendar IDs can coexist in same buffer
;; - Mixed event types (regular, recurring, all-day) coexist properly
;; - Nested heading structures maintain correct hierarchy
;; - Event isolation across buffer positions

;;; Code:

(require 'org-gcal)
(require 'ert)

(unless (and (boundp 'org-gcal-client-id) org-gcal-client-id
             (boundp 'org-gcal-client-secret) org-gcal-client-secret)
  (setq org-gcal-client-id "test_client_id"
        org-gcal-client-secret "test_client_secret"))

(defconst test-integration-multi-event-calendar-id "multi@test.com")

(defmacro test-integration-multi-event--with-temp-buffer (contents &rest body)
  "Create an org-mode temp buffer with CONTENTS and execute BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun test-integration-multi-event--json-read-string (json)
  "Parse JSON string into event plist."
  (with-temp-buffer
    (insert json)
    (org-gcal--json-read)))

;;; Test Events

(defconst test-integration-multi-event-event1-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"event1-etag\\\"\",
 \"id\": \"event1-id\",
 \"status\": \"confirmed\",
 \"summary\": \"Morning Standup\",
 \"start\": {
  \"dateTime\": \"2025-02-01T09:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-02-01T09:30:00-08:00\"
 }
}
")

(defconst test-integration-multi-event-event2-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"event2-etag\\\"\",
 \"id\": \"event2-id\",
 \"status\": \"confirmed\",
 \"summary\": \"Lunch Meeting\",
 \"location\": \"Cafe\",
 \"start\": {
  \"dateTime\": \"2025-02-01T12:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-02-01T13:00:00-08:00\"
 }
}
")

(defconst test-integration-multi-event-event3-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"event3-etag\\\"\",
 \"id\": \"event3-id\",
 \"status\": \"confirmed\",
 \"summary\": \"Afternoon Review\",
 \"description\": \"Weekly review meeting\",
 \"start\": {
  \"dateTime\": \"2025-02-01T15:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-02-01T16:00:00-08:00\"
 }
}
")

(defconst test-integration-multi-event-event1
  (test-integration-multi-event--json-read-string test-integration-multi-event-event1-json))

(defconst test-integration-multi-event-event2
  (test-integration-multi-event--json-read-string test-integration-multi-event-event2-json))

(defconst test-integration-multi-event-event3
  (test-integration-multi-event--json-read-string test-integration-multi-event-event3-json))

;;; Test: Multiple Events in Same Buffer

(ert-deftest test-integration-multi-event-three-events-all-created ()
  "Test creating three separate events in the same buffer.

Creates three different events at different positions in the same org buffer,
verifying that each is created independently with correct properties and that
they don't interfere with each other.

Components integrated:
- org-gcal--update-entry (called 3 times at different positions)
- Buffer navigation (goto-char, re-search-forward to position at each heading)
- org-element-at-point (verify each event's properties independently)
- Entry identification (verify unique entry-id for each)
- Workflow: Navigate to heading 1 → update → navigate to 2 → update → navigate to 3 → update → verify all 3

Validates:
- All three events created successfully
- Each has correct headline from its event summary
- Each has unique entry-id combining event-id/calendar-id
- Event 2 has LOCATION property (\"Cafe\")
- All events coexist without interfering
- Buffer can hold multiple events from same calendar"
  (test-integration-multi-event--with-temp-buffer
   "\
* Event 1
* Event 2
* Event 3
"
   ;; Update each heading with its event
   (goto-char (point-min))
   (re-search-forward "^\\* Event 1")
   (org-gcal--update-entry test-integration-multi-event-calendar-id
                           test-integration-multi-event-event1)

   (goto-char (point-min))
   (re-search-forward "^\\* Event 2")
   (org-gcal--update-entry test-integration-multi-event-calendar-id
                           test-integration-multi-event-event2)

   (goto-char (point-min))
   (re-search-forward "^\\* Event 3")
   (org-gcal--update-entry test-integration-multi-event-calendar-id
                           test-integration-multi-event-event3)

   ;; Verify all three events are present
   (goto-char (point-min))
   (re-search-forward "^\\* Morning Standup")
   (should (org-at-heading-p))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event1-id/multi@test.com")))

   (goto-char (point-min))
   (re-search-forward "^\\* Lunch Meeting")
   (should (org-at-heading-p))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event2-id/multi@test.com"))
     (should (equal (org-element-property :LOCATION elem)
                    "Cafe")))

   (goto-char (point-min))
   (re-search-forward "^\\* Afternoon Review")
   (should (org-at-heading-p))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event3-id/multi@test.com")))))

(ert-deftest test-integration-multi-event-updates-dont-affect-others ()
  "Test that updating one event doesn't affect others in same buffer.

When multiple events exist in the same buffer, updating one should leave the
others completely unchanged. This validates event isolation and prevents
cross-contamination during updates.

Components integrated:
- org-gcal--update-entry (updates first event only)
- Buffer navigation (position at correct event for update)
- org-element-at-point (verify both updated and unchanged events)
- Property preservation (second event keeps original ETag and LOCATION)
- Workflow: Create 2 events → update event 1 → verify event 1 changed → verify event 2 unchanged

Validates:
- First event updated successfully (headline and ETag changed)
- Second event completely unchanged (same ETag, LOCATION preserved)
- Update operation scoped to single entry only
- No side effects on other buffer entries
- Event isolation maintained during updates"
  (test-integration-multi-event--with-temp-buffer
   "\
* Morning Standup
:PROPERTIES:
:ETag:     \"event1-etag\"
:calendar-id: multi@test.com
:entry-id:   event1-id/multi@test.com
:END:
:org-gcal:
<2025-02-01 Sat 09:00-09:30>
:END:

* Lunch Meeting
:PROPERTIES:
:ETag:     \"event2-etag\"
:LOCATION: Cafe
:calendar-id: multi@test.com
:entry-id:   event2-id/multi@test.com
:END:
:org-gcal:
<2025-02-01 Sat 12:00-13:00>
:END:
"
   ;; Update first event
   (goto-char (point-min))
   (re-search-forward "^\\* Morning Standup")
   (let ((updated-event (test-integration-multi-event--json-read-string
                         "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"event1-updated\\\"\",
 \"id\": \"event1-id\",
 \"status\": \"confirmed\",
 \"summary\": \"Updated Morning Standup\",
 \"description\": \"Now with description\",
 \"start\": {
  \"dateTime\": \"2025-02-01T09:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-02-01T09:30:00-08:00\"
 }
}
")))
     (org-gcal--update-entry test-integration-multi-event-calendar-id updated-event))

   ;; Verify first event updated
   (goto-char (point-min))
   (re-search-forward "^\\* Updated Morning Standup")
   (should (org-at-heading-p))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :ETAG elem)
                    "\"event1-updated\"")))

   ;; Verify second event unchanged
   (goto-char (point-min))
   (re-search-forward "^\\* Lunch Meeting")
   (should (org-at-heading-p))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :ETAG elem)
                    "\"event2-etag\""))
     (should (equal (org-element-property :LOCATION elem)
                    "Cafe")))))

;;; Test: Same Calendar ID, Different Events

(ert-deftest test-integration-multi-event-same-calendar-different-ids ()
  "Test multiple events from same calendar ID with different event IDs.

Multiple events from the same calendar (same calendar-id) should coexist
properly, distinguished by their unique event IDs. The entry-id combines
both to create unique identifiers.

Components integrated:
- org-gcal--update-entry (creates 2 events with same calendar-id)
- Entry identification (calendar-id + event-id → entry-id)
- org-element-at-point (verify properties for each event)
- Workflow: Create event A → create event B (same calendar) → verify both have same calendar-id, different entry-ids

Validates:
- Both events have identical CALENDAR-ID (\"multi@test.com\")
- Each has unique ENTRY-ID (\"event1-id/multi@test.com\" vs \"event2-id/multi@test.com\")
- Entry-id format: event-id/calendar-id
- Same calendar can have multiple events in buffer
- Events distinguished by event-id portion"
  (test-integration-multi-event--with-temp-buffer
   "\
* Event A
* Event B
"
   ;; Create both events with same calendar-id
   (goto-char (point-min))
   (re-search-forward "^\\* Event A")
   (org-gcal--update-entry test-integration-multi-event-calendar-id
                           test-integration-multi-event-event1)

   (goto-char (point-min))
   (re-search-forward "^\\* Event B")
   (org-gcal--update-entry test-integration-multi-event-calendar-id
                           test-integration-multi-event-event2)

   ;; Verify both have same calendar-id but different entry-ids
   (goto-char (point-min))
   (re-search-forward "^\\* Morning Standup")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "multi@test.com"))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event1-id/multi@test.com")))

   (goto-char (point-min))
   (re-search-forward "^\\* Lunch Meeting")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "multi@test.com"))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event2-id/multi@test.com")))))

;;; Test: Different Calendar IDs in Same Buffer

(ert-deftest test-integration-multi-event-different-calendars-same-buffer ()
  "Test events from different calendar IDs in same buffer.

A single org buffer can contain events from multiple calendars (work, personal,
etc.). Each event maintains its own calendar-id, and entry-ids reflect the
correct calendar source.

Components integrated:
- org-gcal--update-entry (called with different calendar-ids)
- Entry identification (entry-id includes calendar-id for namespacing)
- org-element-at-point (verify each event's calendar-id)
- Multi-calendar support (buffer holds mixed calendar sources)
- Workflow: Create work event (work@test.com) → create personal event (personal@test.com) → verify different calendar-ids

Validates:
- First event has CALENDAR-ID \"work@test.com\"
- First event has ENTRY-ID \"event1-id/work@test.com\"
- Second event has CALENDAR-ID \"personal@test.com\"
- Second event has ENTRY-ID \"event2-id/personal@test.com\"
- Calendar-id properly namespaces events
- Multiple calendars coexist in single buffer"
  (test-integration-multi-event--with-temp-buffer
   "\
* Work Event
* Personal Event
"
   ;; Create event with first calendar
   (goto-char (point-min))
   (re-search-forward "^\\* Work Event")
   (org-gcal--update-entry "work@test.com"
                           test-integration-multi-event-event1)

   ;; Create event with second calendar
   (goto-char (point-min))
   (re-search-forward "^\\* Personal Event")
   (org-gcal--update-entry "personal@test.com"
                           test-integration-multi-event-event2)

   ;; Verify different calendar-ids
   (goto-char (point-min))
   (re-search-forward "^\\* Morning Standup")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "work@test.com"))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event1-id/work@test.com")))

   (goto-char (point-min))
   (re-search-forward "^\\* Lunch Meeting")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "personal@test.com"))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "event2-id/personal@test.com")))))

;;; Test: Mixed Event Types in Same Buffer

(ert-deftest test-integration-multi-event-mixed-types-coexist ()
  "Test that different event types coexist in same buffer.

Regular events, recurring events, and all-day events should all work together
in the same buffer without conflicts. Each type has distinct characteristics
(time formats, properties, etc.) that must be preserved independently.

Components integrated:
- org-gcal--update-entry (handles all event types)
- org-gcal--format-event-timestamp (different formatting for each type)
- Recurrence handling (RECURRENCE property for recurring events)
- All-day detection (no time component in timestamp)
- Workflow: Create regular → create recurring → create all-day → verify each type's characteristics

Validates:
- Regular event has time component (\"09:00-09:30\")
- Recurring event has RECURRENCE property (\"[\\\"RRULE:FREQ=WEEKLY\\\"]\")
- All-day event has no time component (just date \"<2025-02-04\")
- All-day timestamp lacks HH:MM format
- Different event types don't interfere with each other
- Each type maintains its distinct formatting and properties"
  (let ((recurring-event (test-integration-multi-event--json-read-string
                          "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"recurring-etag\\\"\",
 \"id\": \"recurring-id\",
 \"status\": \"confirmed\",
 \"summary\": \"Weekly Meeting\",
 \"start\": {
  \"dateTime\": \"2025-02-03T10:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-02-03T11:00:00-08:00\"
 },
 \"recurrence\": [
  \"RRULE:FREQ=WEEKLY\"
 ]
}
"))
        (allday-event (test-integration-multi-event--json-read-string
                       "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"allday-etag\\\"\",
 \"id\": \"allday-id\",
 \"status\": \"confirmed\",
 \"summary\": \"All Day Event\",
 \"start\": {
  \"date\": \"2025-02-04\"
 },
 \"end\": {
  \"date\": \"2025-02-05\"
 }
}
")))
    (test-integration-multi-event--with-temp-buffer
     "\
* Regular Event
* Recurring Event
* All-Day Event
"
     ;; Create regular event
     (goto-char (point-min))
     (re-search-forward "^\\* Regular Event")
     (org-gcal--update-entry test-integration-multi-event-calendar-id
                             test-integration-multi-event-event1)

     ;; Create recurring event
     (goto-char (point-min))
     (re-search-forward "^\\* Recurring Event")
     (org-gcal--update-entry test-integration-multi-event-calendar-id
                             recurring-event)

     ;; Create all-day event
     (goto-char (point-min))
     (re-search-forward "^\\* All-Day Event")
     (org-gcal--update-entry test-integration-multi-event-calendar-id
                             allday-event)

     ;; Verify regular event (has time)
     (goto-char (point-min))
     (re-search-forward "^\\* Morning Standup")
     (re-search-forward ":org-gcal:")
     (let ((elem (org-element-at-point)))
       (let ((contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
         (should (string-match-p "09:00-09:30" contents))))

     ;; Verify recurring event (has recurrence property)
     (goto-char (point-min))
     (re-search-forward "^\\* Weekly Meeting")
     (let ((elem (org-element-at-point)))
       (should (equal (org-element-property :RECURRENCE elem)
                      "[\"RRULE:FREQ=WEEKLY\"]")))

     ;; Verify all-day event (no time)
     (goto-char (point-min))
     (re-search-forward "^\\* All Day Event")
     (re-search-forward ":org-gcal:")
     (let ((elem (org-element-at-point)))
       (let ((contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
         (should (string-match-p "<2025-02-04" contents))
         (should-not (string-match-p "[0-9][0-9]:[0-9][0-9]" contents)))))))

;;; Test: Events with Nested Headings

(ert-deftest test-integration-multi-event-nested-headings-isolated ()
  "Test that events under different parent headings are isolated.

Events can exist at different nesting levels in the org hierarchy. Level 2
events under different level 1 parents should be completely isolated from
each other, maintaining correct hierarchy and calendar associations.

Components integrated:
- org-gcal--update-entry (updates nested headings)
- Org-mode hierarchy (level 1 and level 2 headings)
- org-element-at-point (extracts level and properties)
- Buffer navigation (navigate through nested structure)
- Entry identification (calendar-id isolation across hierarchy)
- Workflow: Create work event (level 2 under Work Calendar) → create personal event (level 2 under Personal Calendar) → verify hierarchy + isolation

Validates:
- \"Work Calendar\" is level 1 heading
- \"Morning Standup\" is level 2 heading under Work Calendar
- Work event has CALENDAR-ID \"work@test.com\"
- \"Personal Calendar\" is level 1 heading
- \"Lunch Meeting\" is level 2 heading under Personal Calendar
- Personal event has CALENDAR-ID \"personal@test.com\"
- Nested events maintain correct parent relationships
- Events isolated despite being at same nesting level"
  (test-integration-multi-event--with-temp-buffer
   "\
* Work Calendar
** Work Event
* Personal Calendar
** Personal Event
"
   ;; Create work event
   (goto-char (point-min))
   (re-search-forward "^\\*\\* Work Event")
   (org-gcal--update-entry "work@test.com"
                           test-integration-multi-event-event1)

   ;; Create personal event
   (goto-char (point-min))
   (re-search-forward "^\\*\\* Personal Event")
   (org-gcal--update-entry "personal@test.com"
                           test-integration-multi-event-event2)

   ;; Verify both events exist with correct hierarchy
   (goto-char (point-min))
   (re-search-forward "^\\* Work Calendar")
   (should (org-at-heading-p))
   (should (= (org-element-property :level (org-element-at-point)) 1))

   (re-search-forward "^\\*\\* Morning Standup")
   (should (org-at-heading-p))
   (should (= (org-element-property :level (org-element-at-point)) 2))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "work@test.com")))

   (goto-char (point-min))
   (re-search-forward "^\\* Personal Calendar")
   (should (org-at-heading-p))
   (should (= (org-element-property :level (org-element-at-point)) 1))

   (re-search-forward "^\\*\\* Lunch Meeting")
   (should (org-at-heading-p))
   (should (= (org-element-property :level (org-element-at-point)) 2))
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "personal@test.com")))))

(provide 'test-integration-multi-event-sync)
;;; test-integration-multi-event-sync.el ends here
