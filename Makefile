LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

all:
	mkdir -p ebin
	(cd src;$(MAKE))

edoc:
	(cd src;$(MAKE) edoc)

test:
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)

dist-src:
	mkdir mochiweb-0.2/ && cp -rfv Makefile README priv scripts src support mochiweb-0.2/
	tar zcf mochiweb-0.2.tgz mochiweb-0.2

install: all
	mkdir -p ${LIBDIR}/mochiweb-1/{ebin,include}
	for i in ebin/*.beam; do install $$i $(LIBDIR)/mochiweb-0.2/$$i ; done
