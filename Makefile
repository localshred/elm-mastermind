test:
	./node_modules/.bin/elm-test

server:
	./node_modules/.bin/elm-live src/Main.elm -o -v -H -s index.html -d assets -- --output=assets/elm.js
