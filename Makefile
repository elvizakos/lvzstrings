VERSION:=2.1.2
PACKAGE_NAME:=lvzstrings-mode-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" --exclude="Makefile" --exclude="ChangeLog" --exclude="ChangeLog" --exclude="COPYING" --exclude="*.gif" --exclude="*.md" --exclude="*.org" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	echo "" > lvzstrings-mode-autoloads
	cp -r ./* $@
	sed -re "s/VERSION/$(VERSION)/" $@/lvzstrings-mode-pkg.el > $@/"~tmp~"
	mv $@/"~tmp~" $@/lvzstrings-mode-pkg.el
	sed -re 's/;; Version: %%VERSION%%/;; Version: '"$(VERSION)"'/' $@/lvzstrings-mode.el > $@/"~tmp~"
	sed -re 's/\(defconst lvzstrings-version \"%%VERSION%%\"/\(defconst lvzstrings-version "'"$(VERSION)"'"/' $@/"~tmp~" > $@/lvzstrings-mode.el
	rm $@/"~tmp~"

install:
#emacs -e "(progn (package-initialize)(package-install \'lvzstrings-mode))"
#emacs25 -e '(progn (package-initialize) (package-install-file "'$(PACKAGE_DIR)'/lvzstrings-mode.el"))'
	tar -xvf ../$(PACKAGE_NAME).tar -C ~/.emacs.d/elpa/

remove:
	rm -rf ~/.emacs.d/elpa/$(PACKAGE_NAME)

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)

#end

