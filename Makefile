default: install

dependencies:
	@command -v stow >/dev/null 2>&1 || { echo >&2 "Please install GNU stow"; exit 1; }

install: dependencies
	@echo TODO
