.PHONY: all clean radio html css js

all: radio html css js

clean:
	rm -rf build/*

radio:
	elm-make src/radio.elm --output=build/elm.js

html:
	cp src/index.html build/

css:
	cp src/styles.css build/

js:
	cp src/port.js build/

dev:
	live-server build/
