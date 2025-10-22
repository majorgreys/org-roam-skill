.PHONY: prepare test lint clean help

help:
	@echo "Available targets:"
	@echo "  make prepare  - Install dependencies via Eldev"
	@echo "  make test     - Run all tests"
	@echo "  make lint     - Run linting checks"
	@echo "  make clean    - Remove compiled files and Eldev cache"
	@echo ""
	@echo "Prerequisites:"
	@echo "  Eldev must be installed (https://github.com/doublep/eldev)"
	@echo "  Install: curl -fsSL https://raw.github.com/doublep/eldev/master/webinstall/github-eldev | sh"

prepare:
	eldev -C --unstable prepare

test:
	eldev -C --unstable test

lint:
	eldev -C --unstable lint

clean:
	@echo "Cleaning compiled files and cache..."
	@rm -f *.elc test/*.elc
	@rm -rf .eldev
	@echo "Done!"
