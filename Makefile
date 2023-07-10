SHELL = /bin/sh

ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif
MANPREFIX := $(PREFIX)/share/man
QUICKLISP_DIR := ~/quicklisp

help:
	@echo "Use one of the following options:"
	@echo " - install"
	@echo " - uninstall"
	@echo " - reinstall"
	@echo " - update"

crater-get:
	@echo "Setting up Crater for temporary use..."
	git clone https://github.com/crater-space/cli /tmp/crater-cli

primary-deps:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v sbcl),)
	@echo "SBCL found."
else
	@echo "SBCL not found!"
	@echo "Attemping to install SBCL using Crater..."
	/tmp/crater-cli/crater install sbcl
endif
	@echo "Looking for 'ffmpeg'..."
ifneq ($(shell command -v ffmpeg),)
	@echo "'ffmpeg' found."
else
	@echo "'ffmpeg' not found!"
	@echo "Attemping to install 'ffmpeg' using Crater..."
	/tmp/crater-cli/crater install ffmpeg
endif
	@echo "All required dependencies found."

crater-remove:
	@echo "Removing Crater..."
	rm -rf /tmp/crater-cli

req: crater-get primary-deps crater-remove

quicklisp:
ifeq ("$(wildcard $(QUICKLISP_DIR))", "")
	@echo "Setting up Quicklisp..."
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp
	sbcl --load /tmp/quicklisp.lisp --non-interactive --eval "(quicklisp-quickstart:install)"
	sbcl --load ~/quicklisp/setup.lisp --non-interactive --eval "(ql:add-to-init-file)"
else
	@echo "Quicklisp found."
endif

binary:
	@echo "Generating binary..."
	sbcl --non-interactive --load build.lisp
	@echo "Binary generated."

place:
	@echo "Installing binary..."
	sudo install ./story-slicer-bin $(PREFIX)/bin/story-slicer
	sudo install ./scripts/* $(PREFIX)/bin/
	@echo "Binary installed."

manpage:
	@echo "Creating manpage..."
	mkdir -p $(MANPREFIX)
	sudo rsync ./man/story-slicer.1 $(MANPREFIX)/man1/
	@echo "Manpage created."

install: req quicklisp binary place manpage
	@echo "story-slicer is now installed."

uninstall:
	@echo "Uninstalling story-slicer..."
	sudo rm $(PREFIX)/bin/story-slicer*
	sudo rm $(MANPREFIX)/man1/story-slicer.1
	@echo "story-slicer has been uninstalled."

reinstall: uninstall install

get-latest:
	git pull origin main

update: get-latest reinstall
