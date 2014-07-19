import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

name := "slackey"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += "twitter-repo" at "http://maven.twttr.com"

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

libraryDependencies ++= Seq(
"com.twitter" % "finagle-core" % "6.6.2",
"com.twitter" % "finagle-http" % "6.6.2",
"org.scalaj" %% "scalaj-http" % "0.3.16",
"org.mongodb" %% "casbah" % "2.7.2",
"com.typesafe.akka" %% "akka-actor" % "2.3.4"
)
