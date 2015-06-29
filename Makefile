ELM_MAIN = radio
ELM_OUTPUT = elm.js
STATIC = index.html
BROWSER_TARGET = demo.html

SOURCE_DIR = src
BUILD_DIR = build

ELM_SOURCE = $(SOURCE_DIR)/$(ELM_MAIN).elm
ELM_BUILD = $(BUILD_DIR)/$(ELM_OUTPUT)

###

.PHONY: all clean radio html css js static dev publish

all: elm html css js static

clean:
	rm -rf $(BUILD_DIR) &&\
	mkdir -p $(BUILD_DIR)

elm:
	elm-make $(SOURCE_DIR)/$(ELM_MAIN).elm --output=$(ELM_BUILD)

html:
	cp $(SOURCE_DIR)/index.html $(BUILD_DIR)

css:
	cp $(SOURCE_DIR)/styles.css $(BUILD_DIR)

js:
	cp $(SOURCE_DIR)/port.js $(BUILD_DIR)

static:
	cp static/* $(BUILD_DIR)

dev: all
	live-server $(BUILD_DIR) &  watch make ./src
        
publish: clean all
	cd build &&\
	git init &&\
	git remote add origin git@github.com:unbalancedparentheses/radioelm.git &&\
	git add . &&\
	git commit -m "update gh-pages to match master" &&\
	git push origin -f master:gh-pages &&\
	echo "Successfully pushed to production." &&\
	echo "Check https://unbalancedparentheses.github.io/radioelm/"
