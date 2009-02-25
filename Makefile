LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

all:
	mkdir -p ebin
	(cd src;$(MAKE))

edoc:
	(cd src;$(MAKE) edoc)

test:
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)

dist-src:
	mkdir mochiweb-1/ && cp -rfv Makefile README priv scripts src support mochiweb-1/
	tar zcf mochiweb-1.tgz mochiweb-1

install: all
	mkdir -p ${LIBDIR}/mochiweb-1/{ebin,include}
	for i in ebin/*.beam; do install $$i $(LIBDIR)/mochiweb-1/$$i ; done
