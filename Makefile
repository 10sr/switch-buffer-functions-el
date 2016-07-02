emacs ?= emacs

ert_tests_el = $(wildcard test/*.el)
el = $(wildcard *.el)
elc = $(el:%.el=%.elc)

.PHONY: all test test-ert build

all:

test: build test-ert info

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -f batch-byte-compile $<


test-ert: $(ert_tests_el)
	$(emacs) -batch -Q -L . --eval "(require 'ert)" $(^:%=-l "%") \
		-f ert-run-tests-batch-and-exit


elisp_get_file_package_info := \
	(lambda (f) \
		(with-temp-buffer \
			(insert-file-contents-literally f) \
			(package-buffer-info)))

elisp_print_infos := \
	(mapc \
		(lambda (f) \
			(message \"Loading info: %s\" f) \
			(message \"%S\" (funcall $(elisp_get_file_package_info) f))) \
		command-line-args-left)

info: $(el)
	$(emacs) -batch -Q \
		--eval "(require 'package)" \
		--eval "$(elisp_print_infos)" \
		$^

