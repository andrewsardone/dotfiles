SHELL := /bin/bash
YYYYMM := $(shell date +%Y%m)
CLEANUP_DIR = $(HOME)/Documents/Cleanup/$(YYYYMM)

default: help

cleanup: ## Archive Desktop and Download files for tidiness
	@mkdir -p $(CLEANUP_DIR)
	@mv -v $(HOME)/Desktop/* $(CLEANUP_DIR) 2> /dev/null || echo 'Desktop empty'
	@mv -v $(HOME)/Downloads/* $(CLEANUP_DIR) 2> /dev/null || echo 'Downloads empty'

# via https://gist.github.com/prwhite/8168133
help: ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
