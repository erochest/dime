
# BUILD_FLAGS=--pedantic --library-profiling --executable-profiling
BUILD_FLAGS=--pedantic

RUN=stack exec -- dialogue
DB=dialogue.sqlite

run: build archivedb update stats publish archive
	cat `make last-archive` | xz > $(HOME)/Dropbox/dialogues/archive-`timestamp`.json.xz

init: build
	$(RUN) init --db-file $(DB)

journal: build
	$(RUN) journal --help

migrate: build
	$(RUN) migrate --db-file $(DB) --service twitter --input `make last-archive`

update: build adium google note twitter archive

adium: build
	$(RUN) update --db-file $(DB) --service adium

google: build
	$(RUN) update --db-file $(DB) --service google

note: build
	$(RUN) update --db-file $(DB) --service note

twitter: build
	$(RUN) update --db-file $(DB) --service twitter

archive: build
	$(RUN) archive --db-file $(DB) --output tmp/archive-`timestamp`.json

stats: build
	$(RUN) stats --db-file $(DB) --output tmp/stats-`timestamp`.json
	jq -r '.[] | [.year, .month, .adium.primary.count, .adium.secondary.count, .google.primary.count, .google.secondary.count, .twitter.primary.count, .twitter.secondary.count] | @csv' \
		`make last-stats` > tmp/stats-`timestamp`.csv

mail: build
	echo "delete from mail_message;" | sqlite3 $(DB)
	$(RUN) mail --db-file $(DB) --input tmp/mail.eml 2> tmp/log.out
	echo "select count(*) from mail_message;" | sqlite3 $(DB)

publish: build
	$(RUN) publish --db-file $(DB) --output tmp/epub3/
	-rm -rf tmp/epub3-contents
	mkdir tmp/epub3-contents
	unzip -d tmp/epub3-contents `make last-epub`
	epubcheck `make last-epub`

archivedb:
	cp dialogue.sqlite tmp/dialogue.sqlite-`timestamp`

last-archive:
	@ls -1 tmp/archive/*.json | tail -1

last-stats:
	@ls -1 tmp/stats-*.json | tail -1

last-epub:
	@ls -1 tmp/epub3/*.epub | tail -1

docs:
	stack haddock
	open `stack path --local-doc-root`/index.html

# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

configure:
	cabal configure \
		--package-db=clear \
		--package-db=global \
		--package-db=`stack path --snapshot-pkg-db` \
		--package-db=`stack path --local-pkg-db`

install:
	stack install

tags: $(SRC)
	codex update

hlint:
	hlint *.hs src specs

clean:
	stack clean
	codex cache clean

distclean: clean
	rm stack.yaml

build:
	stack build $(BUILD_FLAGS)

test:
	stack test $(BUILD_FLAGS) # --test-arguments "-m TODO"

bench:
	stack bench $(BUILD_FLAGS)

watch:
	stack build --file-watch --pedantic --fast --exec "make publish"

watch-test:
	stack test --file-watch --pedantic --test-arguments "-m Google"

restart: distclean build

rebuild: clean build

.PHONY: run docs configure install hlint clean distclean build test
	bench watch watch-test restart rebuild archivedb
	init journal
