SBCL ?= sbcl
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
SERVICEDIR = /etc/systemd/system
CONFDIR = /home/cloak/.cloak

VERSION = $(shell grep ':version' cloak.asd | head -1 | sed 's/.*"\(.*\)".*/\1/')
BINARY = bin/cloak

.PHONY: all build test clean install uninstall deploy service-install service-enable service-start help

all: build

build: ## Build the CLoak executable
	@echo "Building CLoak v$(VERSION)..."
	$(SBCL) --non-interactive --load build.lisp
	@echo "Built: $(BINARY)"
	@ls -lh $(BINARY)

test: ## Run the test suite
	$(SBCL) --non-interactive \
		--eval '(push #P"./" asdf:*central-registry*)' \
		--eval '(ql:quickload "cloak" :silent t)' \
		--eval '(asdf:test-system "cloak")' \
		--eval '(uiop:quit (if (fiveam:run-all-tests) 0 1))'

clean: ## Remove build artifacts
	rm -rf bin/
	rm -rf ~/.cache/common-lisp/sbcl-*/$(shell pwd)/

install: build ## Install binary to PREFIX/bin
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 $(BINARY) $(DESTDIR)$(BINDIR)/cloak
	@echo "Installed cloak to $(DESTDIR)$(BINDIR)/cloak"

uninstall: ## Remove installed binary and service
	rm -f $(DESTDIR)$(BINDIR)/cloak
	rm -f $(DESTDIR)$(SERVICEDIR)/cloak.service
	@echo "Uninstalled cloak"

deploy: install service-install service-enable ## Full deployment: build, install, enable service
	@echo ""
	@echo "CLoak v$(VERSION) deployed."
	@echo "  Binary:  $(BINDIR)/cloak"
	@echo "  Service: $(SERVICEDIR)/cloak.service"
	@echo "  Config:  $(CONFDIR)/config.lisp"
	@echo ""
	@echo "Next steps:"
	@echo "  1. Create cloak user: sudo useradd -r -s /usr/sbin/nologin -m -d /home/cloak cloak"
	@echo "  2. Generate config:   sudo -u cloak $(BINDIR)/cloak --generate-config"
	@echo "  3. Edit config:       sudo -u cloak vi $(CONFDIR)/config.lisp"
	@echo "  4. Start service:     sudo systemctl start cloak"

service-install: ## Install systemd service file
	install -d $(DESTDIR)$(SERVICEDIR)
	install -m 644 cloak.service $(DESTDIR)$(SERVICEDIR)/cloak.service
	systemctl daemon-reload
	@echo "Installed cloak.service"

service-enable: ## Enable cloak service to start on boot
	systemctl enable cloak.service
	@echo "Enabled cloak.service"

service-start: ## Start the cloak service
	systemctl start cloak.service
	@echo "Started cloak.service"

service-stop: ## Stop the cloak service
	systemctl stop cloak.service
	@echo "Stopped cloak.service"

service-status: ## Show service status
	systemctl status cloak.service

release: build ## Create a release tarball
	@mkdir -p release
	tar czf release/cloak-$(VERSION)-$(shell uname -m)-linux.tar.gz \
		-C bin cloak \
		--transform 's,^,cloak-$(VERSION)/,'
	@echo "Release: release/cloak-$(VERSION)-$(shell uname -m)-linux.tar.gz"
	@ls -lh release/cloak-$(VERSION)-$(shell uname -m)-linux.tar.gz

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'
