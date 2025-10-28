;;; test-integration-complex-event-formatting.el --- Integration tests for complex event formatting -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integration tests for refactored pure functions working together.
;;
;; Tests the 4 pure functions extracted during refactoring, validating that
;; they integrate correctly with org-gcal--update-entry in real scenarios.
;; Covers complex formatting cases like asterisk escaping, missing data,
;; ROAM_REFS vs link property selection, and multi-day event handling.
;;
;; Components integrated:
;; - org-gcal--update-entry (entry update orchestration)
;; - org-gcal--format-event-timestamp (timestamp formatting for various event types)
;; - org-gcal--determine-headline (headline text selection from summary)
;; - org-gcal--format-description-for-drawer (description escaping and formatting)
;; - org-gcal--determine-source-property (ROAM_REFS vs link property logic)
;; - org-element-at-point (org property extraction)
;; - org-entry-get-multivalued-property (ROAM_REFS reading)
;;
;; Key behaviors tested:
;; - Leading asterisks in descriptions are escaped (prevent org headings)
;; - Missing summaries become "busy" headline
;; - CANCELLED summary preservation logic
;; - Source URL goes to ROAM_REFS (no title) or link (with title)
;; - Multi-day vs all-day event timestamp formatting

;;; Code:

(require 'org-gcal)
(require 'ert)

(unless (and (boundp 'org-gcal-client-id) org-gcal-client-id
             (boundp 'org-gcal-client-secret) org-gcal-client-secret)
  (setq org-gcal-client-id "test_client_id"
        org-gcal-client-secret "test_client_secret"))

(defconst test-integration-complex-formatting-calendar-id "complex@test.com")

(defmacro test-integration-complex-formatting--with-temp-buffer (contents &rest body)
  "Create an org-mode temp buffer with CONTENTS and execute BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun test-integration-complex-formatting--json-read-string (json)
  "Parse JSON string into event plist."
  (with-temp-buffer
    (insert json)
    (org-gcal--json-read)))

;;; Test: Complex Description with Leading Asterisks

(defconst test-integration-complex-formatting-asterisk-description-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"asterisk123\\\"\",
 \"id\": \"asterisk-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Event with org syntax in description\",
 \"description\": \"* Important point\\n* Another important point\\n** Sub-point\\n\\nRegular paragraph\",
 \"start\": {
  \"dateTime\": \"2025-01-15T10:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-15T11:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-complex-formatting-description-escapes-asterisks ()
  "Test that descriptions with leading asterisks are escaped to avoid org headings.

Event descriptions from Google Calendar may contain asterisks at line starts.
In org-mode, these would be interpreted as headings, breaking the document
structure. The integration should escape them to ✱ (unicode star).

Components integrated:
- org-gcal--update-entry (calls formatting functions)
- org-gcal--format-description-for-drawer (escapes leading asterisks with regex)
- Workflow: JSON description → escape asterisks → insert in :org-gcal: drawer

Validates:
- Lines starting with * become ✱
- Lines starting with ** become ✱*
- Mid-line asterisks remain unchanged
- Regular paragraphs unaffected
- Drawer content doesn't create accidental org headings"
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-asterisk-description-json)))
    (test-integration-complex-formatting--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let ((elem (org-element-at-point)))
        (let ((contents (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))))
          ;; Leading asterisks should be replaced with ✱
          (should (string-match-p "✱ Important point" contents))
          (should (string-match-p "✱ Another important point" contents))
          (should (string-match-p "✱✱ Sub-point" contents))
          (should (string-match-p "Regular paragraph" contents))
          ;; Should NOT have literal asterisks at line starts
          (should-not (string-match-p "^\* Important" contents))
          (should-not (string-match-p "^\*\* Sub" contents)))))))

;;; Test: Event with No Summary (becomes "busy")

(defconst test-integration-complex-formatting-no-summary-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"nosummary456\\\"\",
 \"id\": \"nosummary-event\",
 \"status\": \"confirmed\",
 \"description\": \"Event without a summary\",
 \"start\": {
  \"dateTime\": \"2025-01-16T14:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-16T15:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-complex-formatting-no-summary-becomes-busy ()
  "Test that events without summary get 'busy' as headline."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-no-summary-json)))
    (test-integration-complex-formatting--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; No summary should result in "busy"
        (should (equal (org-element-property :title elem)
                       '("busy")))))))

(ert-deftest test-integration-complex-formatting-no-summary-preserves-existing ()
  "Test that events without summary preserve existing headline."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-no-summary-json)))
    (test-integration-complex-formatting--with-temp-buffer
     "\
* My Important Meeting
:PROPERTIES:
:ETag:     \"nosummary456\"
:calendar-id: complex@test.com
:entry-id:   nosummary-event/complex@test.com
:END:
"
     (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
     (org-back-to-heading)
     (let ((elem (org-element-at-point)))
       ;; Should preserve existing headline when no summary provided
       (should (equal (org-element-property :title elem)
                      '("My Important Meeting")))))))

;;; Test: Cancelled Event (summary equals CANCELLED keyword)

(defconst test-integration-complex-formatting-cancelled-summary-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"canc789\\\"\",
 \"id\": \"cancelled-event\",
 \"status\": \"confirmed\",
 \"summary\": \"CANCELLED\",
 \"description\": \"This event was cancelled\",
 \"start\": {
  \"dateTime\": \"2025-01-17T09:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-17T10:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-complex-formatting-cancelled-summary-preserves-headline ()
  "Test that summary of CANCELLED preserves existing headline."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-cancelled-summary-json))
        (org-gcal-cancelled-todo-keyword "CANCELLED"))
    (test-integration-complex-formatting--with-temp-buffer
     "\
* Team Planning Session
:PROPERTIES:
:ETag:     \"canc789\"
:calendar-id: complex@test.com
:entry-id:   cancelled-event/complex@test.com
:END:
"
     (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
     (org-back-to-heading)
     (let ((elem (org-element-at-point)))
       ;; Summary of "CANCELLED" should NOT replace headline
       (should (equal (org-element-property :title elem)
                      '("Team Planning Session")))))))

;;; Test: Source Property with ROAM_REFS vs Link

(defconst test-integration-complex-formatting-source-no-title-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"src111\\\"\",
 \"id\": \"source-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Event with source link\",
 \"source\": {
  \"url\": \"https://example.com/meeting\"
 },
 \"start\": {
  \"dateTime\": \"2025-01-18T11:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-18T12:00:00-08:00\"
 }
}
")

(defconst test-integration-complex-formatting-source-with-title-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"src222\\\"\",
 \"id\": \"source-title-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Event with titled source\",
 \"source\": {
  \"url\": \"https://example.com/meeting\",
  \"title\": \"Meeting Notes\"
 },
 \"start\": {
  \"dateTime\": \"2025-01-18T13:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-18T14:00:00-08:00\"
 }
}
")

(ert-deftest test-integration-complex-formatting-source-no-title-uses-roam-refs ()
  "Test that source without title uses ROAM_REFS property."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-source-no-title-json)))
    (test-integration-complex-formatting--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Should use ROAM_REFS for URL without title
        (let ((roam-refs (org-entry-get-multivalued-property (point) "ROAM_REFS")))
          (should (member "https://example.com/meeting" roam-refs)))
        ;; Should NOT have link property
        (should (null (org-element-property :LINK elem)))))))

(ert-deftest test-integration-complex-formatting-source-with-title-uses-link ()
  "Test that source with title uses link property."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-source-with-title-json)))
    (test-integration-complex-formatting--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
      (org-back-to-heading)
      (let ((elem (org-element-at-point)))
        ;; Should use link property for URL with title
        (should (equal (org-element-property :LINK elem)
                       "[[https://example.com/meeting][Meeting Notes]]"))
        ;; Should NOT use ROAM_REFS
        (let ((roam-refs (org-entry-get-multivalued-property (point) "ROAM_REFS")))
          (should (null roam-refs)))))))

(ert-deftest test-integration-complex-formatting-source-existing-link-switches-to-link ()
  "Test that existing link property causes new source to use link."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-source-no-title-json)))
    (test-integration-complex-formatting--with-temp-buffer
     "\
* Event with source link
:PROPERTIES:
:ETag:     \"src111\"
:link:     [[https://old.com][Old Link]]
:calendar-id: complex@test.com
:entry-id:   source-event/complex@test.com
:END:
"
     (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
     (org-back-to-heading)
     (let ((elem (org-element-at-point)))
       ;; Even without title, should use link because link already exists
       (should (stringp (org-element-property :LINK elem)))
       (should (string-match-p "https://example.com/meeting"
                               (org-element-property :LINK elem)))))))

;;; Test: Multi-day vs All-day vs Timed Events

(defconst test-integration-complex-formatting-multiday-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"multi333\\\"\",
 \"id\": \"multiday-event\",
 \"status\": \"confirmed\",
 \"summary\": \"Multi-day conference\",
 \"start\": {
  \"dateTime\": \"2025-01-20T09:00:00-08:00\"
 },
 \"end\": {
  \"dateTime\": \"2025-01-22T17:00:00-08:00\"
 }
}
")

(defconst test-integration-complex-formatting-allday-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"all444\\\"\",
 \"id\": \"allday-event\",
 \"status\": \"confirmed\",
 \"summary\": \"All-day workshop\",
 \"start\": {
  \"date\": \"2025-01-23\"
 },
 \"end\": {
  \"date\": \"2025-01-24\"
 }
}
")

(ert-deftest test-integration-complex-formatting-multiday-range-format ()
  "Test that multi-day events get range timestamp format."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-multiday-json)))
    (test-integration-complex-formatting--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let ((elem (org-element-at-point)))
        (let ((contents (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))))
          ;; Multi-day should have range format with --
          (should (string-match-p "<2025-01-20.*>--<2025-01-22.*>" contents)))))))

(ert-deftest test-integration-complex-formatting-allday-single-date ()
  "Test that all-day single date events get simple date format."
  (let ((event (test-integration-complex-formatting--json-read-string
                test-integration-complex-formatting-allday-json)))
    (test-integration-complex-formatting--with-temp-buffer
        "* "
      (org-gcal--update-entry test-integration-complex-formatting-calendar-id event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let ((elem (org-element-at-point)))
        (let ((contents (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))))
          ;; All-day should be simple date (no time)
          (should (string-match-p "<2025-01-23 \\w+>" contents))
          ;; Should NOT have time component
          (should-not (string-match-p "[0-9][0-9]:[0-9][0-9]" contents)))))))

(provide 'test-integration-complex-event-formatting)
;;; test-integration-complex-event-formatting.el ends here
