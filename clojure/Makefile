API.md:
	clj -M:quickdoc :github/repo https://github.com/jimka2001/heavybool :git/branch main
	mv API.md API.tmp
	cat API.tmp | sed -e 's,heavybool/blob/main/src,heavybool/blob/main/clojure/src,' > API.md
	rm API.tmp
