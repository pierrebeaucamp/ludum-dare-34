default: build
build: clean dist/main.js

clean:
	rm -rf dist

dist/main.js:
	mkdir dist
	pulp build -O
	psc-bundle output/**/*.js -m LudumDare -o dist/main.js
	sed -i -n '/LOG(/!p' dist/main.js
