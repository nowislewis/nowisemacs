EMACS = emacs
LIB_DIR = lib
AUTOLOAD_FILE = autoloads.el

update:
	./useful-tools/update_submodule.sh
init:
	@git submodule update --init
	@git submodule foreach git reset --hard
build:
	$(EMACS) --batch -Q -l useful-tools/build.el \
		--eval '''(progn \
			(setq generated-autoload-file "$(AUTOLOAD_FILE)") \
			(loaddefs-generate (find-package-dirs (expand-file-name "$(LIB_DIR)")) "$(AUTOLOAD_FILE)"))'''
