BIN= linked-list-dist
GHCJS= dist
PROJECT= linked-list-web-demo
INDEX= linked_list.html
SERVER= akira
STAGE= :
SETUP= "tar -xzf $(BIN).tgz && cp -vr $(BIN)/* /srv/www/demos/"
BROWSER= firefox

all: build

test: build
	echo $(SETUP)
	$(BROWSER) $(BIN)/$(INDEX)

deploy: package
	scp $(BIN).tgz $(SERVER)$(STAGE)
	ssh $(SERVER) $(SETUP)

package: build
	tar -czf $(BIN).tgz $(BIN)

build: ghcjs static

ghcjs: bin
	cabal configure --ghcjs
	cabal build
	cp -r $(GHCJS)/build/$(PROJECT)/*.jsexe $(BIN)

static: bin
	cp static/*.html $(BIN); exit 0;
	cp static/*.css $(BIN); exit 0;
	cp static/*.js $(BIN); exit 0;

bin: clean
	mkdir $(BIN)

clean:
	rm $(BIN).tgz; exit 0;
	rm -r $(BIN); exit 0;
	rm -r $(GHCJS); exit 0;
