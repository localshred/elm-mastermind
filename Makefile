build:
	./node_modules/.bin/elm build src/Main.elm --output=assets/elm.js \
		&& cp -R assets dist

clean:
	rm -rf dist/ && rm -rf assets/elm.js

server:
	./node_modules/.bin/elm-live src/Main.elm -o -v -H -s index.html -d assets -- --output=assets/elm.js

test:
	./node_modules/.bin/elm-test
