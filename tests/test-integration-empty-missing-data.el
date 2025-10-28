;;; test-integration-empty-missing-data.el --- Integration tests for empty/missing data handling -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integration tests for handling empty strings, nil values, and missing data.
;;
;; Tests graceful degradation when Google Calendar events contain missing or
;; empty fields. Validates that the system handles edge cases robustly across
;; the entire event processing pipeline, from JSON parsing through org entry
;; creation and updates.
;;
;; Components integrated:
;; - org-gcal--update-entry (entry creation/update orchestration)
;; - org-gcal--determine-headline (nil/empty summary handling, "busy" fallback)
;; - org-gcal--format-description-for-drawer (nil/empty description handling)
;; - org-gcal--format-event-timestamp (timestamp formatting)
;; - org-gcal--json-read (JSON parsing with missing fields)
;; - org-element-at-point (property extraction)
;; - Property setters (handling nil/empty values, preventing empty properties)
;;
;; Key behaviors tested:
;; - Missing summary becomes "busy" headline
;; - Empty strings vs nil values treated correctly
;; - Optional fields can be omitted without errors
;; - Whitespace-only fields preserved (not treated as empty)
;; - Updates can add previously missing fields
;; - Fields are not actively removed when cleared (preserved)

;;; Code:

(require 'org-gcal)
(require 'ert)

(unless (and (boundp 'org-gcal-client-id) org-gcal-client-id
             (boundp 'org-gcal-client-secret) org-gcal-client-secret)
  (setq org-gcal-client-id "test_client_id"
        org-gcal-client-secret "test_client_secret"))

(defconst test-integration-empty-missing-calendar-id "empty@test.com")

(defmacro test-integration-empty-missing--with-temp-buffer (contents &rest body)
  "Create an org-mode temp buffer with CONTENTS and execute BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun test-integration-empty-missing--json-read-string (json)
  "Parse JSON string into event plist."
  (with-temp-buffer
    (insert json)
    (org-gcal--json-read)))

;;; Test: Minimal Event (only required fields)

(defconst test-integration-empty-missing-minimal-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"minimal123\\\"\",
 \"id\": \"minimal-event\",
 \"status\": \"confirmed\",
 \"start\": {
  \"dateTime\": \"2025-01-25T10:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-25T11:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-empty-missing-minimal-event-succeeds ()
  "Test that event with only required fields creates successfully.

Google Calendar events may contain only the bare minimum fields (kind, etag,
id, status, start, end). The system should handle these gracefully, using
sensible defaults for missing optional fields.

Components integrated:
- org-gcal--json-read (parses JSON with missing optional fields)
- org-gcal--update-entry (creates entry from minimal event data)
- org-gcal--determine-headline (nil summary → \"busy\" headline)
- org-gcal--format-event-timestamp (formats timestamp from start/end)
- org-gcal--format-description-for-drawer (handles nil description)
- Workflow: Minimal JSON → parse → create entry with defaults → validate

Validates:
- Missing summary becomes \"busy\" headline
- entry-id property set correctly
- Timestamp created in :org-gcal: drawer
- No errors thrown for missing optional fields (description, location, etc.)
- System degrades gracefully with minimal data"
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-minimal-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; No summary should become "busy"
        (should (equal (org-element-property :title elem)
                       '("busy")))
        ;; Should have entry-id
        (should (equal (org-element-property :ENTRY-ID elem)
                       "minimal-event/empty@test.com"))
        ;; Should have timestamp in drawer
        (re-search-forward ":org-gcal:")
        (let ((elem (org-element-at-point)))
          (let ((contents (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem))))
            (should (string-match-p "<2025-01-25" contents))))))))

;;; Test: Empty String Fields

(defconst test-integration-empty-missing-empty-strings-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"empty456\\\"\",
 \"id\": \"empty-event\",
 \"status\": \"confirmed\",
 \"summary\": \"\",
 \"description\": \"\",
 \"location\": \"\",
 \"start\": {
  \"dateTime\": \"2025-01-26T14:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-26T15:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-empty-missing-empty-summary-becomes-busy ()
  "Test that empty summary string results in 'busy' headline.

Empty strings are different from missing (nil) fields in JSON, but both should
result in the \"busy\" headline fallback. This tests the empty string case.

Components integrated:
- org-gcal--json-read (parses JSON with empty string fields)
- org-gcal--update-entry (processes event with empty summary)
- org-gcal--determine-headline (empty string \"\" → \"busy\" headline)
- Workflow: Empty summary JSON → parse → determine headline → validate

Validates:
- Empty string summary (not nil) becomes \"busy\"
- System treats \"\" as semantically empty
- No empty headline created (would break org structure)"
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-empty-strings-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Empty summary should result in "busy" (not empty headline)
        (should (equal (org-element-property :title elem)
                       '("busy")))))))

(ert-deftest test-integration-empty-missing-empty-description-handled ()
  "Test that empty description is handled gracefully.

Empty descriptions should not cause errors or create malformed drawer content.
The drawer should contain the timestamp plus any whitespace from the empty
description processing.

Components integrated:
- org-gcal--update-entry (processes event with empty description)
- org-gcal--format-description-for-drawer (handles empty string \"\")
- Drawer insertion logic (creates :org-gcal: drawer)
- Workflow: Empty description → format drawer → insert → validate

Validates:
- Empty description doesn't cause errors
- Drawer still created with timestamp
- Drawer ends with newline (proper formatting)
- No malformed drawer content"
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-empty-strings-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let ((elem (org-element-at-point)))
        (let ((contents (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))))
          ;; Should have timestamp
          (should (string-match-p "<2025-01-26" contents))
          ;; Empty description should add just a newline
          (should (string-match-p "\n$" contents)))))))

(ert-deftest test-integration-empty-missing-empty-location-not-set ()
  "Test that empty location doesn't set LOCATION property.

Empty strings for location should result in no LOCATION property being set,
preventing org entries from having empty property values that clutter the
properties drawer.

Components integrated:
- org-gcal--update-entry (processes event with empty location)
- Property setters (skip setting properties for empty strings)
- org-element-at-point (verify property not set)
- Workflow: Empty location → property setter → validate nil property

Validates:
- Empty location string doesn't create LOCATION property
- Property drawer not cluttered with empty values
- nil returned when querying non-existent property
- System distinguishes between \"no value\" and \"empty value\""
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-empty-strings-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Empty location should result in nil LOCATION property
        (should (null (org-element-property :LOCATION elem)))))))

;;; Test: Missing Optional Fields

(defconst test-integration-empty-missing-no-optional-fields-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"noopt789\\\"\",
 \"id\": \"noopt-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Event with no optional fields\",
 \"start\": {
  \"dateTime\": \"2025-01-27T09:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-27T10:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-empty-missing-no-optional-fields-succeeds ()
  "Test that event without optional fields succeeds.

Events with only required fields (no description, location, source, recurrence,
etc.) should create valid org entries. All optional properties should be absent
(nil) rather than having empty or default values.

Components integrated:
- org-gcal--json-read (parses JSON without optional fields)
- org-gcal--update-entry (creates entry from partial data)
- org-gcal--determine-headline (uses provided summary)
- Property setters (handle missing optional data)
- Workflow: JSON with only required fields → parse → create → validate

Validates:
- Entry created successfully with required fields only
- Headline set from summary
- entry-id property set correctly
- Optional properties are nil (not set): LOCATION, LINK, TRANSPARENCY, HANGOUTS, RECURRENCE
- System doesn't invent default values for missing fields
- Robust handling of partial event data"
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-no-optional-fields-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Basic fields should be set
        (should (equal (org-element-property :title elem)
                       '("Event with no optional fields")))
        (should (equal (org-element-property :ENTRY-ID elem)
                       "noopt-event/empty@test.com"))
        ;; Optional fields should be absent
        (should (null (org-element-property :LOCATION elem)))
        (should (null (org-element-property :LINK elem)))
        (should (null (org-element-property :TRANSPARENCY elem)))
        (should (null (org-element-property :HANGOUTS elem)))
        (should (null (org-element-property :RECURRENCE elem)))))))

;;; Test: Whitespace-Only Fields

(defconst test-integration-empty-missing-whitespace-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"space123\\\"\",
 \"id\": \"space-event\",
 \"status\": \"confirmed\",
 \"summary\": \"   \",
 \"description\": \"  \\n  \",
 \"location\": \"   \",
 \"start\": {
  \"dateTime\": \"2025-01-28T11:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-28T12:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-empty-missing-whitespace-summary-preserved ()
  "Test that whitespace-only summary is preserved (not treated as empty).

Whitespace-only strings are truthy in Emacs Lisp, unlike empty strings.
The system should preserve whitespace-only summaries literally rather than
replacing them with \"busy\". This tests the boundary between empty and
whitespace-only.

Components integrated:
- org-gcal--json-read (parses JSON with whitespace-only fields)
- org-gcal--update-entry (processes whitespace summary)
- org-gcal--determine-headline (whitespace string → preserved as-is)
- Workflow: Whitespace summary → determine headline → validate preservation

Validates:
- Whitespace-only summary \"   \" preserved literally
- Not treated as empty (would become \"busy\")
- String truthiness respected in elisp logic
- Boundary case between empty and non-empty handled correctly"
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-whitespace-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Whitespace summary should be preserved (truthy in elisp)
        (should (equal (org-element-property :title elem)
                       '("   ")))))))

(ert-deftest test-integration-empty-missing-whitespace-description-preserved ()
  "Test that whitespace-only description is preserved.

Whitespace-only descriptions should be inserted into the drawer literally,
preserving the exact whitespace characters (spaces, newlines) from the event.

Components integrated:
- org-gcal--update-entry (processes whitespace description)
- org-gcal--format-description-for-drawer (preserves whitespace literally)
- Drawer insertion (adds whitespace content to :org-gcal: drawer)
- Workflow: Whitespace description → format → insert → validate

Validates:
- Whitespace-only description \"  \\n  \" preserved in drawer
- Exact whitespace pattern maintained
- Not treated as empty or nil
- Drawer content includes literal whitespace"
  (let ((event (test-integration-empty-missing--json-read-string
                test-integration-empty-missing-whitespace-json)))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let ((elem (org-element-at-point)))
        (let ((contents (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))))
          ;; Whitespace description should be in drawer
          (should (string-match-p "  \n  " contents)))))))

;;; Test: Updating from Empty to Non-Empty

(ert-deftest test-integration-empty-missing-update-adds-missing-fields ()
  "Test that updating event can add previously missing fields.

Events may be created with minimal data then later updated with full data.
The system should properly add previously missing fields during updates,
enriching the org entry as more information becomes available.

Components integrated:
- org-gcal--update-entry (handles both initial creation and update)
- org-gcal--determine-headline (updates from \"busy\" to real summary)
- Property setters (add new properties during update)
- org-gcal--format-description-for-drawer (adds description on update)
- Workflow: Minimal event → create → full event → update → validate enrichment

Validates:
- Initial creation with minimal data works (\"busy\" headline, no location)
- Update adds summary (headline changes from \"busy\" to \"Now Has Summary\")
- Update adds LOCATION property (was nil, now \"Conference Room A\")
- Update adds TRANSPARENCY property (was nil, now \"opaque\")
- Update adds description to drawer (was empty, now has content)
- System enriches entries as data becomes available"
  (let ((event-minimal (test-integration-empty-missing--json-read-string
                        test-integration-empty-missing-minimal-json))
        (event-full (test-integration-empty-missing--json-read-string
                     "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"minimal456\\\"\",
 \"id\": \"minimal-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Now Has Summary\",
 \"description\": \"Now has description\",
 \"location\": \"Conference Room A\",
 \"transparency\": \"opaque\",
 \"start\": {
  \"dateTime\": \"2025-01-25T10:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-25T11:00:00-08:00\"
 }
}
")))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      ;; First create with minimal event
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event-minimal)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        (should (equal (org-element-property :title elem)
                       '("busy")))
        (should (null (org-element-property :LOCATION elem))))
      ;; Now update with full event
      (org-back-to-heading)
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event-full)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Fields should now be populated
        (should (equal (org-element-property :title elem)
                       '("Now Has Summary")))
        (should (equal (org-element-property :LOCATION elem)
                       "Conference Room A"))
        (should (equal (org-element-property :TRANSPARENCY elem)
                       "opaque"))
        ;; Description should be in drawer
        (re-search-forward ":org-gcal:")
        (let ((elem (org-element-at-point)))
          (let ((contents (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem))))
            (should (string-match-p "Now has description" contents))))))))

;;; Test: Updating from Non-Empty to Empty

(ert-deftest test-integration-empty-missing-update-removes-cleared-fields ()
  "Test that updating event handles fields being cleared.

When fields are removed from an event (present in old version, absent in new),
the system currently preserves them rather than removing them. This test
documents the current behavior and serves as a specification for future changes.

Components integrated:
- org-gcal--update-entry (processes update with missing fields)
- Property setters (current behavior: preserve existing properties)
- Workflow: Full event → create → cleared event → update → validate preservation

Validates:
- Initial creation with full data (has LOCATION \"Office\")
- Update with cleared fields (description and location removed from JSON)
- **CURRENT BEHAVIOR**: LOCATION property preserved (still \"Office\")
- This documents that org-gcal doesn't actively remove properties
- Fields are additive/update-only, not actively deleted
- Future enhancement: could add property removal logic"
  (let ((event-full (test-integration-empty-missing--json-read-string
                     "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"full123\\\"\",
 \"id\": \"clear-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Original Summary\",
 \"description\": \"Original description\",
 \"location\": \"Office\",
 \"start\": {
  \"dateTime\": \"2025-01-29T13:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-29T14:00:00-08:00\"
 }
}
"))
        (event-cleared (test-integration-empty-missing--json-read-string
                        "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"full456\\\"\",
 \"id\": \"clear-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Original Summary\",
 \"start\": {
  \"dateTime\": \"2025-01-29T13:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-29T14:00:00-08:00\"
 }
}
")))
    (test-integration-empty-missing--with-temp-buffer
        "* "
      ;; First create with full event
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event-full)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        (should (equal (org-element-property :LOCATION elem)
                       "Office")))
      ;; Update with cleared fields
      (org-back-to-heading)
      (org-gcal--update-entry test-integration-empty-missing-calendar-id event-cleared)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Location should still be present (org-gcal preserves props not in update)
        ;; This documents current behavior - fields are not actively removed
        (should (equal (org-element-property :LOCATION elem)
                       "Office"))))))

(provide 'test-integration-empty-missing-data)
;;; test-integration-empty-missing-data.el ends here
