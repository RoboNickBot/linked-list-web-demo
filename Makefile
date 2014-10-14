build:
	mkdir build
	mkdir build/src
	pandoc -r markdown -w html -o build/src/index.html src/index.md
	pandoc -r markdown -w html -o build/src/p1.html src/p1.md
	pandoc -r markdown -w html -o build/src/p2.html src/p2.md
	pandoc -r markdown -w html -o build/src/p3.html src/p3.md
	pandoc -r markdown -w html -o build/src/p4.html src/p4.md
	pandoc -r markdown -w html -o build/src/p5.html src/p5.md
	elm --bundle-runtime src/lists.elm
	elm --bundle-runtime src/linked.elm
	cp -v src/* build/src

clean:
	rm -vr build
	rm -vr cache

install:
	scp build/src/* nlewchen@turing.slu.edu:WWW/
