THIS_MAKEFILE_DIR = $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
EMACS ?= emacs
SRC=org-gcal.el org-generic-id.el

# Integration tests - test multiple components working together
INTEGRATION_TESTS=tests/test-integration-org-gcal.el \
	tests/test-integration-org-generic-id.el \
	tests/test-integration-recurring-events.el \
	tests/test-integration-complex-event-formatting.el \
	tests/test-integration-empty-missing-data.el \
	tests/test-integration-multi-event-sync.el

# Unit tests - one file per method
UNIT_TESTS=tests/test-org-gcal--safe-substring.el \
	tests/test-org-gcal--parse-date.el \
	tests/test-org-gcal--alldayp.el \
	tests/test-org-gcal--format-iso2org.el \
	tests/test-org-gcal--format-org2iso.el \
	tests/test-org-gcal--iso-next-day.el \
	tests/test-org-gcal--iso-previous-day.el \
	tests/test-org-gcal--convert-time-to-local-timezone.el \
	tests/test-org-gcal--parse-calendar-time-string.el \
	tests/test-org-gcal--event-id-from-entry-id.el \
	tests/test-org-gcal--format-entry-id.el \
	tests/test-org-gcal--event-cancelled-p.el \
	tests/test-org-gcal--source-from-link-string.el \
	tests/test-org-gcal--filter.el \
	tests/test-org-generic-id--hash-to-alist.el \
	tests/test-org-generic-id--alist-to-hash.el \
	tests/test-org-generic-id--locations-hash-to-alist.el \
	tests/test-org-generic-id--locations-alist-to-hash.el \
	tests/test-org-generic-id--last-update-id-time-get.el \
	tests/test-org-generic-id--last-update-id-time-put.el \
	tests/test-org-gcal--format-event-timestamp.el \
	tests/test-org-gcal--determine-headline.el \
	tests/test-org-gcal--format-description-for-drawer.el \
	tests/test-org-gcal--determine-source-property.el

# All tests
TEST=$(UNIT_TESTS) $(INTEGRATION_TESTS)

BUILD_LOG = build.log
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
ELCFILES = $(SRC:.el=.elc)
.DEFAULT_GOAL := all

.PHONY: all clean load-path compile test test-unit test-integration elpa install

all: compile test

load-path:
	$(CASK) load-path

clean:
	rm -f $(ELCFILES) $(BUILD_LOG)
	rm -rf $(PKG_DIR)
	$(CASK) clean-elc
	find . -name "*.elc" -type f -delete
	find . -name "*.eln" -type f -delete
	rm -rf eln-cache/

install: elpa
elpa: $(PKG_DIR)
$(PKG_DIR): Cask
	$(CASK) install
	touch $@

compile: $(SRC) $(TEST) elpa
	$(CASK) emacs -batch -L . -L tests \
	  -f batch-byte-compile $$($(CASK) files) \
	  $(foreach test,$(TEST),$(addprefix $(THIS_MAKEFILE_DIR)/,$(test)))

test-unit: $(SRC) $(UNIT_TESTS) elpa compile
	@echo "Running unit tests..."
	$(CASK) emacs --batch \
	-L $(THIS_MAKEFILE_DIR) \
	$(foreach test,$(UNIT_TESTS),$(addprefix -l $(THIS_MAKEFILE_DIR)/,$(test))) \
	-f ert-run-tests-batch-and-exit

test-integration: $(SRC) $(INTEGRATION_TESTS) elpa compile
	@echo "Running integration tests..."
	$(CASK) emacs --batch \
	-L $(THIS_MAKEFILE_DIR) \
	$(foreach test,$(INTEGRATION_TESTS),$(addprefix -l $(THIS_MAKEFILE_DIR)/,$(test))) \
	-f ert-run-tests-batch-and-exit

test: test-unit test-integration
	@echo "All tests completed!"
