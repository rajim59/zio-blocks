USAEJHWm12$
git commit --allow-empty -m "chore: fix error"

sbt +test:compile

sbt +test
sbt +compile


sbt "project schemaJVM" test



sbt clean
sbt +test:compile

sbt ++2.13.18 schemaJVM/test

sbt ++3.3.7 schemaJVM/test


Refactor: Implement Static Validation Architecture for Scala 2 & 3
sbt "++2.13; check; ++3.3; check; ++3.7; check"

sbt ++2.13.x testJVM

sbt ++3.x coverage testJVM coverageReport

sbt ++2.13.x testJS

sbt ++3.x testJS

sbt ++2.13.x testNative

sbt ++3.7.4 scalaNextTestsNative/test








sbt reload
sbt +test

git commit --amend --no-edit -S


git checkout -b schema-thrift-v2


git push -f origin json-data-v2

sbt "scalafmtAll; scalafmtSbt"
sbt "++2.13.18!; scalafmtAll; ++3.3.7!; scalafmtAll"
sbt "++2.13; check; ++3.3; check; ++3.7; check"
sbt ++2.13.x testJVM
sbt ++3.x testJVM
sbt +test

sbt ++2.13.18 schemaJVM/test

sbt ++3.3.7 schemaJVM/test

sbt ++3.7.4 schemaJVM/test

sbt ++3.7.4! scalaNextTestsJVM/test scalaNextTestsJS/test scalaNextTestsNative/test benchmarks/test



sbt ++3.7.4! scalaNsbt ++3.x coverage testJVM coverageReport


The implementation covers 100% of the core algebra, builder API, error modeling, and laws as specified in the documentation. It represents a fully type-safe, verifiable, and offline-ready migration definition system