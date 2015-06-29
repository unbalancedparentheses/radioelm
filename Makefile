.PHONY: all elm radio html css js dev publish

all: elm html css js

clean:
	rm -rf build/ &&\
	mkdir -p build

elm:
	elm-make src/radio.elm --output=build/elm.js

html:
	cp src/index.html build/

css:
	cp src/styles.css build/

js:
	cp src/port.js build/

dev: all
	live-server build/& watch make ./src

publish: clean all
	cd build &&\
	git init &&\
	git remote add origin git@github.com:unbalancedparentheses/radioelm.git &&\
	git add . &&\
	git commit -m "update gh-pages to match master" &&\
	git push origin -f master:gh-pages &&\
	echo "Successfully pushed to production." &&\
	echo "Check https://unbalancedparentheses.github.io/radioelm/"
