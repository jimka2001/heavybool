doc:
	rm -rf target/scala-2.13/api
	mkdir -p target/scala-2.13/api
	sbt clean doc
	sbt doc
	git add --all -f target/scala-2.13/api/
