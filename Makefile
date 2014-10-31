build:
	mkdir build
	mkdir build/src
	cabal configure --ghcjs
	cabal build
	cp -v dist/build/capstone/capstone.jsexe/* build/src/
	pandoc -r markdown -w html -o build/src/index.html src/index.md
	pandoc -r markdown -w html -o build/src/p1.html src/p1.md
	pandoc -r markdown -w html -o build/src/p2.html src/p2.md
	pandoc -r markdown -w html -o build/src/p3.html src/p3.md
	pandoc -r markdown -w html -o build/src/p4.html src/p4.md
	pandoc -r markdown -w html -o build/src/p5.html src/p5.md
	cp -v src/*.html build/src
	cp -v src/*.css build/src
	cp -v src/*.js build/src
	tar -czf pages.tgz build/src/*

clean:
	rm -vr build
	rm -vr dist
	rm -v pages.tgz

install:
	scp pages.tgz nlewchen@turing.slu.edu:WWW/
	ssh nlewchen@turing.slu.edu "cd WWW && tar -xvzf pages.tgz && cp build/src/* ./ && rm -rf build && rm pages.tgz"
