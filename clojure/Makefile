all: API.md API-examples.md

API.md:
	clj -M:quickdoc \
		:filename-add-prefix clojure/ \
		:github/repo https://github.com/jimka2001/heavybool \
		:git/branch main

API-examples.md:
	clj -M:quickdoc \
		:outfile API-examples.md \
		:source-paths examples \
		:filename-add-prefix clojure/ \
		:github/repo https://github.com/jimka2001/heavybool \
		:git/branch main


clean:
	rm -f API.md API-examples.md
