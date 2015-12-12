default: build
build: dist/main.js

clean:
	rm -rf dist bower_components node_modules .psci_modules output

dist/main.js:
	rm -rf dist
	mkdir dist
	pulp build -O
	psc-bundle output/**/*.js -m LudumDare -o dist/main.js
