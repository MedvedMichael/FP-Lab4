name := "quickcheck"

version := "0.1"

scalaVersion := "2.13.6"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")