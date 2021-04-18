SHELL := /bin/bash
RESTIC := $(shell command -v restic 2> /dev/null)

CRONTAB_INPUT := *.cron
CRONTAB_BACKUP := tmp/crontab.bak
CRONTAB_TO_SET := tmp/crontab

default: help

.PHONY: install
install: ## Install the *.cron jobs from this directory into the user's cron table
install: test cronjobs

.PHONY: clean
clean: ## Delete generated artifacts & reset to a clean state
	rm -f $(CRONTAB_TO_SET)
	rm -f $(CRONTAB_BACKUP)

.PHONY: test
test: ## Verify system is properly configured for backups
	@echo -n "Running tests… "
	# Nothing to do, yet
	@echo "OK"

$(CRONTAB_BACKUP): .FORCE
	@echo -n "Backing up crontab… "
	@touch $@
	@if crontab -l >/dev/null; then crontab -l > $@; fi
	@echo "OK"

$(CRONTAB_TO_SET): $(CRONTAB_BACKUP) $(CRONTAB_INPUT)
	@echo -n "Generating new cron table… "
	@cat $(CRONTAB_BACKUP) | grep -v "andrewsardone-dotfiles" > $@ || echo -n "" > $@
	@cat $(CRONTAB_INPUT) | awk '{print $$0, " # andrewsardone-dotfiles"}' >> $@
	@echo "OK"

.PHONY: cronjobs
cronjobs: tmp/crontab
	@echo -n "Installing cron commands…"
	@crontab $<
	@echo "OK"

.FORCE:

# via https://gist.github.com/prwhite/8168133
help: ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
