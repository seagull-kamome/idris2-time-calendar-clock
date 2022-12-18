
PKG=time-calendar-clock
DEPS=-p missing-numeric -p elab-util -p sop -p contrib


build:
	idris2 --build ${PKG}.ipkg

install:
	idris2 --install ${PKG}.ipkg

clean:
	idris2 --clean ${PKG}.ipkg
	find . -name *~ -delete
	find . -name *.bak -delete

deepclean:
	rm -rf build

check: check-chez check-gambit check-node check-js check-refc
check-chez:
	idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg chez -x main
check-gambit:
	# idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg gambit -x main
check-node:
	idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg node -x main
check-js:
	# idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg javascript -o test-javascript.js
check-refc:
	idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg refc -o test-refc
	build/exec/test-refc

.PHONY: clean deepclean build install check

