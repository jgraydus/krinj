ALL: main

main: client
	cabal build

client: client/src/generated/bindings.js
	cd client && npm run build

client/src/generated/bindings.js:
	cabal run generate-js-bindings

clean:
	rm -f client/src/generated/bindings.js
	cabal clean
	cd client && npm run clean

run: client
	cabal run web

