BIN= linked-list
VERSION= 0.1.0.1
BINDIR= $(BIN)-$(VERSION)
DISTFILE= $(BINDER).tar.gz
GHCJS= dist
PROJECT= linked-list-web-demo
INDEX= linked_list.html
SERVER= akira
STAGE= :
SETUP= "tar -xzf $(DISTFILE) && cp -vr $(BINDIR)/* /srv/www/demos/"
BROWSER= firefox

all: build

test: build
	echo $(SETUP)
	$(BROWSER) $(BINDIR)/$(INDEX)

deploy: package
	scp $(DISTFILE) $(SERVER)$(STAGE)
	ssh $(SERVER) $(SETUP)

package: build
	tar -czf $(DISTFILE) $(BINDIR)

build: ghcjs static

ghcjs: bin
	cabal configure --ghcjs
	cabal build
	cp -r $(GHCJS)/build/$(PROJECT)/*.jsexe $(BINDIR)

static: bin
	cp static/*.html $(BINDIR); exit 0;
	cp static/*.css $(BINDIR); exit 0;
	cp static/*.js $(BINDIR); exit 0;

bin: clean
	mkdir $(BINDIR)

clean:
	rm $(DISTFILE); exit 0;
	rm -r $(BINDIR); exit 0;
	rm -r $(GHCJS); exit 0;
