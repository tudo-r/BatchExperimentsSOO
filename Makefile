R	:= R --no-save --no-restore
RSCRIPT	:= Rscript 
DELETE	:= rm -fR

.SILENT:
.PHONEY: clean roxygenize package windows dependencies install test html check

usage:
	echo "Available targets:"
	echo ""
	echo " clean         - Clean everything up"
	echo " roxygenize    - roxygenize skel/ into pkg/"
	echo " package       - build source package"
	echo " windows       - build windows binary via win-builder/devtools"
	echo " dependencies  - install required packages"
	echo " install       - install the package"
	echo " test          - run tests"
	echo " html          - build static html documentation"
	echo " check         - run R CMD check on the package"

clean:
	echo "Cleaning up ..."
	${DELETE} skel/src/*.o skel/src/*.so pkg.Rcheck
	${DELETE} pkg
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ../tools/roxygenize
	echo "Setting version ..."
	${RSCRIPT} ../tools/set-version
	${DELETE} pkg/inst
	find pkg -type d -name .svn  | xargs rm -fR

package: roxygenize
	echo "Building package file ..."
	${R} CMD build pkg/

windows: roxygenize
	echo "Building windows binary ..."
	${RSCRIPT} ../tools/win_build
  
dependencies:
	echo "Installing depencies"
	${RSCRIPT} install_dependencies.R 

install: roxygenize
	echo "Installing package ..."
	${R} CMD INSTALL pkg

test: install
	echo "Testing package ..."
	${RSCRIPT} ./test_all.R

html: install
	echo "Building HTML documentation"
	${RSCRIPT} ./staticdocs.R

check: roxygenize
	echo "Running R CMD check ..."
	${R} CMD check pkg

  
