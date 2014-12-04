BIN= linked-list-web-demo



package: bin ghcjs static
	tar -czf $(BIN).tar $(BIN)

ghcjs: bin
	cabal install --ghcjs --bindir=$(BIN)

static: bin
	cp static/*.html $(BIN); exit 0;
	cp static/*.css $(BIN); exit 0;
	cp static/*.js $(BIN); exit 0;

bin: clean
	mkdir $(BIN)

clean:
	rm $(BIN).tar; exit 0;
	rm -r $(BIN); exit 0;
	rm -r dist; exit 0;

install:
	scp pages.tgz akira:/www/
	ssh akira "cd /www/ && tar -xvzf pages.tgz && cp build/src/* ./ && rm -rf build && rm pages.tgz"
