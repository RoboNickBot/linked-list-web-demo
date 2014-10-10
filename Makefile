build:
	mkdir build
	mkdir build/src
	pandoc -r markdown -w html -o build/src/index.html src/index.md
	elm --bundle-runtime src/lists.elm
	elm --bundle-runtime src/linked.elm
	cp -v src/* build/src

clean:
	rm -vr build
	rm -vr cache

install:
	scp build/src/* nlewchen@turing.slu.edu:WWW/
