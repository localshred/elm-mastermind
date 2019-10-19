build:
	./node_modules/.bin/elm make src/Main.elm --output=assets/elm.js \
		&& mkdir -p dist \
		&& cp -R assets/* dist

clean:
	rm -rf dist/ && rm -rf assets/elm.js

open:
	open dist/index.html

server:
	./node_modules/.bin/elm-live src/Main.elm -o -v -H -s index.html -d assets -- --output=assets/elm.js

test:
	./node_modules/.bin/elm-test
