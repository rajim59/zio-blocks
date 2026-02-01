

sbt clean
sbt +test:compile
sbt "scalafmtAll; scalafmtSbt"

sbt "++2.13.18!; scalafmtAll; ++3.3.7!; scalafmtAll"

sbt "++2.13; check; ++3.3; check; ++3.7; check"

sbt ++2.13.x testJVM

sbt ++3.x coverage testJVM coverageReport

sbt ++2.13.x testJS

sbt ++3.x testJS

sbt ++2.13.x testNative

 sbt ++3.x testNative

 sbt ++3.x coverage testJVM coverageReport